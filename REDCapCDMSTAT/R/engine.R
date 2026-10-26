#' The REDCap CDMSTAT engine
#'
#' One object holds the local cache, the live data dictionary, the query
#' ledger and the analysis state. Create it with [cdmstat_connect()],
#' [cdmstat_cloud()], [cdmstat_odm()], [cdmstat_demo()] or [cdmstat_open()].
#'
#' @section Methods:
#' \describe{
#' \item{Data}{`$sync(full)`, `$on_det(record, event, instrument)`, `$data()`, `$summary()`, `$reload_if_changed()`}
#' \item{Data quality and queries}{`$validate()`, `$add_rule(id, logic, message, severity, field)`, `$remove_rule(id)`,
#'   `$raise_queries()`, `$add_query(record_id, field, message, ...)`, `$queries(status)`,
#'   `$transition_query(query_id, status, comment)`, `$comment_query(query_id, comment)`, `$query_audit(query_id)`, `$writeback(field)`}
#' \item{REDCap Data Resolution Workflow}{`$drw_enable(prefix, verify_sdv, lock_in_edc)`, `$drw_sync()`, `$drw_status()`, `$drw_links()`, `$drw_disable()`}
#' \item{Data cuts}{`$freeze(label, note)`, `$sign_off(cut_id, comment)`, `$cuts()`, `$use_cut(cut_id, require_signoff)`, `$active_cut()`}
#' \item{Analysis}{`$shape(longitudinal, repeating, labels, log, carry_constant)`, `$variables()`, `$arm()`,
#'   `$table_one(by, vars, event, tests, mode)`, `$outliers(field, method, threshold)`, `$trend(field, by)`, `$fit(formula, type, longitudinal, repeating)`}
#' \item{Randomisation}{`$setup_randomization(strata, ratio, block_sizes, per_stratum, seed, arm_field)`, `$randomize(record_id)`, `$randomization_status()`}
#' \item{Source data verification}{`$sdv_plan(critical_fields, first_n, sample_rate, seed)`, `$sdv_refresh()`, `$sdv_status(required_only)`, `$sdv_verify(unit_keys, comment)`}
#' \item{Locking and signatures}{`$lock(record_id, reason, event, form)`, `$unlock(lock_id, reason)`, `$locks()`,
#'   `$esign(record_id, meaning, identity_verified)`, `$signatures()`}
#' \item{Medical coding}{`$load_dictionary(dict, name)`, `$dictionary(name)`, `$code_terms(field, dictionary)`, `$coding(status)`, `$approve_coding(coding_key, code)`}
#' \item{Deviations and metrics}{`$log_deviation(record_id, category, description, severity, event)`, `$close_deviation(dev_id, comment)`,
#'   `$deviations()`, `$check_visit_windows(anchor, visit_field, window)`, `$site_metrics()`, `$enrollment_curve(date_field)`, `$activity(module, n)`}
#' \item{Export and reproducibility}{`$sdtm(map, studyid)`, `$export_cdisc(out_dir, map, studyid, xpt)`, `$script()`, `$log_code(code)`}
#' }
#' @format An [R6::R6Class] generator.
#' @usage NULL
#' @examples
#' nx <- cdmstat_demo(dir = file.path(tempdir(), "cdmstat-ex"), reset = TRUE)
#' nx$raise_queries()
#' head(nx$queries()[, c("record_id", "field", "message")])
#' nx$fit("sbp__week_24 ~ arm + sbp__baseline")
#' @export
CDMSTAT <- R6::R6Class("CDMSTAT",
  public = list(
    backend = NULL, ledger = NULL, dir = NULL, actor = NULL, connect_code = NULL,
    raw = NULL, metadata = NULL, events = NULL, mapping = NULL, repeating = NULL, info = NULL,
    last_sync = NULL, dict_hash = NULL, rules = list(),

    initialize = function(backend, dir, actor = NULL, connect_code = NULL) {
      dir.create(file.path(dir, "cuts"), showWarnings = FALSE, recursive = TRUE)
      self$backend <- backend
      self$dir <- normalizePath(dir, winslash = "/")
      self$actor <- actor %||% Sys.getenv("USERNAME", Sys.getenv("USER", "analyst"))
      self$connect_code <- connect_code %||% sprintf('nx <- cdmstat_open("%s")', self$dir)
      self$ledger <- Ledger$new(file.path(self$dir, "ledger.sqlite"))
      private$load_cache()
      private$load_rules()
      invisible(self)
    },

    id_field = function() self$metadata$field_name[1],
    repeating_forms = function() unique(self$repeating$form_name %||% character()),
    event_order = function() self$events$unique_event_name %||% character(),

    # ---- Synchronisation -------------------------------------------------
    # Full pull on first use; afterwards only records named in the REDCap log
    # since the last sync are re-exported and patched into the cache. The
    # dictionary is re-checked on every sync because a design change does
    # not fire a Data Entry Trigger.
    sync = function(full = FALSE) {
      private$need_backend()
      t0 <- Sys.time()
      if (full || is.null(self$raw)) {
        md_changed <- private$refresh_metadata()
        self$raw <- self$backend$records()
        res <- list(mode = "full", changed = length(unique(self$raw[[self$id_field()]])), deleted = 0L,
                    metadata_changed = md_changed)
      } else {
        begin <- format(self$last_sync - 120, "%Y-%m-%d %H:%M")
        lg <- tryCatch(self$backend$logging(begin), error = function(e) e)
        md_changed <- private$refresh_metadata()
        if (inherits(lg, "error")) {
          # No logging right for this token: fall back to a full download.
          self$raw <- self$backend$records()
          self$last_sync <- t0
          private$typed <- NULL
          private$save_cache()
          private$after_change()
          res <- list(mode = "full (no log access)", changed = length(unique(self$raw[[self$id_field()]])),
                      deleted = 0L, metadata_changed = md_changed)
          self$ledger$log_sync(res$mode, res$changed, 0L, md_changed, as.numeric(difftime(Sys.time(), t0, units = "secs")))
          return(invisible(res))
        }
        ch <- parse_log(lg)
        if (md_changed && length(setdiff(field_columns(self$metadata), names(self$raw)))) {
          self$raw <- self$backend$records()
          ch$changed <- character()
        }
        if (length(ch$deleted)) self$raw <- self$raw[!self$raw[[self$id_field()]] %in% ch$deleted, , drop = FALSE]
        upd <- setdiff(ch$changed, ch$deleted)
        if (length(upd)) private$patch(upd)
        res <- list(mode = "incremental", changed = length(upd), deleted = length(ch$deleted),
                    metadata_changed = md_changed)
      }
      self$last_sync <- t0
      private$save_cache()
      private$after_change()
      self$ledger$log_sync(res$mode, res$changed, res$deleted, res$metadata_changed,
                           as.numeric(difftime(Sys.time(), t0, units = "secs")))
      invisible(res)
    },

    # Handle a REDCap Data Entry Trigger: re-export just that record.
    on_det = function(record, event = NULL, instrument = NULL) {
      private$need_backend()
      t0 <- Sys.time()
      private$patch(as.character(record))
      private$save_cache()
      private$after_change()
      self$ledger$log_sync("det", 1L, 0L, FALSE, as.numeric(difftime(Sys.time(), t0, units = "secs")))
      invisible(list(mode = "det", record = record, event = event, instrument = instrument))
    },

    # Reload the cache if another process (e.g. the API) updated it.
    reload_if_changed = function() {
      f <- file.path(self$dir, "cache.rds")
      if (file.exists(f) && !identical(file.mtime(f), private$cache_mtime)) private$load_cache()
      invisible(self)
    },

    # ---- Data -------------------------------------------------------------
    data = function() {
      if (!is.null(private$cut)) return(private$cut$typed)
      if (is.null(private$typed)) private$typed <- type_data(self$raw, self$metadata)
      private$typed
    },
    summary = function() {
      d <- self$data()
      q <- self$ledger$get("SELECT status, COUNT(*) AS n FROM queries GROUP BY status")
      src <- if (is.null(self$backend)) "Offline cache" else switch(class(self$backend)[1],
        RedcapBackend = "REDCap API", DemoBackend = "Demo (simulated REDCap project)",
        RedcapCloudBackend = "REDCap Cloud API (ODM)", OdmBackend = "CDISC ODM file", class(self$backend)[1])
      list(project = self$info$project_title %||% "", project_id = self$info$project_id %||% "", source = src,
           records = length(unique(d[[self$id_field()]])), rows = nrow(d), fields = nrow(self$metadata),
           events = length(self$event_order()), last_sync = format(self$last_sync %||% NA),
           dict_hash = substr(self$dict_hash %||% "", 1, 12),
           queries = stats::setNames(as.list(q$n), q$status),
           active_cut = private$cut$id %||% NA_character_)
    },

    # ---- Data quality -----------------------------------------------------
    add_rule = function(id, logic, message, severity = "warning", field = NULL) {
      self$rules[[id]] <- list(id = id, logic = logic, message = message, severity = severity, field = field)
      private$save_rules()
      self$log_code(sprintf("nx$add_rule(%s, %s, %s, severity = %s)", deparse(id), deparse(logic),
                            deparse(message), deparse(severity)))
      invisible(self)
    },
    remove_rule = function(id) {
      self$rules[[id]] <- NULL
      private$save_rules()
      invisible(self)
    },
    validate = function() {
      iss <- run_validation(self$data(), self$metadata, self$id_field(), self$repeating_forms(), self$rules)
      if (is.null(private$cut)) {
        # Data edited in the EDC after a record/form was locked.
        l <- tryCatch(self$locks(), error = function(e) data.frame())
        l <- l[NROW(l) > 0 & l$integrity != "intact", , drop = FALSE]
        if (nrow(l)) {
          add <- data.frame(rule = paste0("lock:", l$lock_id), severity = "error", record_id = l$record_id,
                            event = l$event, instance = NA_character_, field = ifelse(is.na(l$form), "(record)", l$form),
                            value = NA_character_, message = paste("Data changed after lock by", l$locked_by, "on", l$locked_at),
                            stringsAsFactors = FALSE)
          add$issue_key <- issue_key(add$rule, add$record_id, add$event, add$instance, add$field)
          iss <- rbind(iss, add)
        }
      }
      iss
    },
    # Turn new rule violations into queries, and auto-resolve open rule
    # queries whose violation has disappeared after a data update.
    raise_queries = function(actor = self$actor) {
      issues <- self$validate()
      known <- self$ledger$get("SELECT query_id, issue_key, status FROM queries WHERE source = 'rule'")
      new <- issues[!issues$issue_key %in% known$issue_key, , drop = FALSE]
      for (i in seq_len(nrow(new))) {
        self$ledger$add_query(new$record_id[i], new$field[i], new$message[i], event = new$event[i],
                              instance = new$instance[i], value = new$value[i], source = "rule",
                              rule = new$rule[i], severity = new$severity[i], issue_key = new$issue_key[i],
                              actor = actor)
      }
      gone <- known[known$status %in% c("open", "answered") & !known$issue_key %in% issues$issue_key, , drop = FALSE]
      for (qid in gone$query_id) {
        self$ledger$transition(qid, "resolved", "system",
                               "Auto-resolved: rule no longer violated after a data update in REDCap.")
      }
      invisible(list(created = nrow(new), auto_resolved = nrow(gone), total_issues = nrow(issues)))
    },
    add_query = function(record_id, field, message, event = NA, instance = NA, value = NA,
                         source = "manual", actor = self$actor) {
      self$ledger$add_query(record_id, field, message, event = event, instance = instance, value = value,
                            source = source, actor = actor)
    },
    queries = function(status = NULL) self$ledger$queries(status),
    transition_query = function(query_id, status, comment, actor = self$actor) {
      self$ledger$transition(query_id, status, actor, comment)
    },
    comment_query = function(query_id, comment, actor = self$actor) self$ledger$comment(query_id, actor, comment),
    query_audit = function(query_id = NULL) self$ledger$audit(query_id),

    # Write the open-query count per record/event into a REDCap field so site
    # staff see it inside REDCap. The field must exist in the dictionary.
    writeback = function(field = "cdmstat_open_queries") {
      private$need_backend()
      if (!field %in% self$metadata$field_name) stop("Field '", field, "' is not in the data dictionary.", call. = FALSE)
      q <- self$ledger$queries(c("open", "answered"))
      base <- self$raw[is_blank(self$raw$redcap_repeat_instrument %||% rep(NA, nrow(self$raw))), , drop = FALSE]
      form <- self$metadata$form_name[self$metadata$field_name == field]
      evs <- if (!is.null(self$mapping) && nrow(self$mapping)) self$mapping$unique_event_name[self$mapping$form == form] else NULL
      if (!is.null(evs) && "redcap_event_name" %in% names(base)) base <- base[base$redcap_event_name %in% evs, , drop = FALSE]
      key <- paste(base[[self$id_field()]], base$redcap_event_name %||% "")
      qk <- paste(q$record_id, ifelse(is.na(q$event), "", q$event))
      counts <- as.integer(table(factor(qk, levels = key)))
      df <- data.frame(stats::setNames(list(base[[self$id_field()]]), self$id_field()), stringsAsFactors = FALSE)
      if ("redcap_event_name" %in% names(base)) df$redcap_event_name <- base$redcap_event_name
      df[[field]] <- as.character(counts)
      changed <- as.character(base[[field]]) != df[[field]] | is_blank(base[[field]])
      df <- df[changed, , drop = FALSE]
      n <- if (nrow(df)) self$backend$import(df) else 0L
      if (n) self$sync()
      invisible(n)
    },

    # ---- Data cuts --------------------------------------------------------
    # Freeze the current cache as an immutable, hashed analysis data cut.
    freeze = function(label, note = NULL, actor = self$actor) {
      id <- new_id("CUT")
      path <- file.path(self$dir, "cuts", paste0(id, ".rds"))
      saveRDS(list(id = id, label = label, raw = self$raw, metadata = self$metadata, events = self$events,
                   mapping = self$mapping, repeating = self$repeating, info = self$info,
                   dict_hash = self$dict_hash, created_at = now_stamp(), created_by = actor), path)
      sha <- digest::digest(file = path, algo = "sha256")
      self$ledger$add_cut(id, label, note, actor, sha, basename(path),
                          length(unique(self$raw[[self$id_field()]])), nrow(self$raw), self$dict_hash)
      id
    },
    sign_off = function(cut_id, comment, actor = self$actor) self$ledger$sign_off(cut_id, actor, comment),
    cuts = function() self$ledger$cuts(),
    # Point all analysis methods at a frozen cut (NULL returns to live data).
    # The file hash is verified against the ledger before use.
    use_cut = function(cut_id = NULL, require_signoff = FALSE) {
      if (is.null(cut_id)) {
        private$cut <- NULL
        self$log_code("nx$use_cut(NULL)")
        return(invisible(self))
      }
      rec <- self$ledger$get("SELECT * FROM cuts WHERE cut_id = ?", list(cut_id))
      if (!nrow(rec)) stop("Unknown cut: ", cut_id, call. = FALSE)
      if (require_signoff && is.na(rec$signed_off_by)) stop("Cut ", cut_id, " has not been signed off.", call. = FALSE)
      path <- file.path(self$dir, "cuts", rec$path)
      if (!identical(digest::digest(file = path, algo = "sha256"), rec$sha256)) {
        stop("Hash mismatch: cut file ", path, " has been altered.", call. = FALSE)
      }
      cut <- readRDS(path)
      cut$typed <- type_data(cut$raw, cut$metadata)
      cut$sha256 <- rec$sha256
      private$cut <- cut
      self$log_code(sprintf('nx$use_cut("%s")  # sha256 %s', cut_id, rec$sha256))
      invisible(self)
    },
    active_cut = function() if (is.null(private$cut)) NULL else private$cut[c("id", "label", "created_at", "sha256", "dict_hash")],

    # ---- Analysis ---------------------------------------------------------
    shape = function(longitudinal = "long", repeating = "drop", labels = TRUE, log = TRUE, carry_constant = FALSE) {
      if (log) self$log_code(sprintf("df <- nx$shape(longitudinal = %s, repeating = %s%s)", deparse(longitudinal), deparse(repeating),
                                     if (carry_constant) ", carry_constant = TRUE" else ""))
      md <- private$md()
      shape_data(self$data(), md, private$cut$mapping %||% self$mapping,
                 unique((private$cut$repeating %||% self$repeating)$form_name %||% character()),
                 md$field_name[1], longitudinal, repeating, labels, carry_constant)
    },
    variables = function(longitudinal = "long", repeating = "drop") {
      attr(self$shape(longitudinal, repeating, log = FALSE), "variables")
    },
    arm = function() detect_arm(private$md()),
    # Metadata-driven baseline table: detects the arm and baseline variables.
    table_one = function(by = NULL, vars = NULL, event = NULL, tests = list(), mode = "exploratory") {
      md <- private$md()
      by <- by %||% detect_arm(md)
      if (identical(by, "") || identical(by, NA_character_)) by <- NULL
      event <- event %||% self$event_order()[1]
      vars <- vars %||% baseline_vars(md, private$cut$mapping %||% self$mapping, event, md$field_name[1], by %||% NA)
      self$log_code(sprintf("t1 <- nx$table_one(by = %s, vars = %s, event = %s, tests = %s, mode = %s)",
                            deparse(by), paste(deparse(vars), collapse = ""), deparse(event),
                            paste(deparse(tests), collapse = ""), deparse(mode)))
      d <- self$shape("long", "drop", log = FALSE)
      if (!is.null(event) && !is.na(event) && "redcap_event_name" %in% names(d)) d <- d[d$redcap_event_name == event, , drop = FALSE]
      if (!is.null(by)) {
        # Arm is usually collected once; carry it to every row of the record.
        arm <- first_non_na_by(self$shape("long", "drop", log = FALSE)[, c(md$field_name[1], by)], md$field_name[1])
        d[[by]] <- arm[[by]][match(d[[md$field_name[1]]], arm[[md$field_name[1]]])]
      }
      table_one(d, vars, by, md, tests, mode)
    },
    outliers = function(field, method = "z", threshold = NULL) {
      self$log_code(sprintf("out <- nx$outliers(%s, method = %s%s)", deparse(field), deparse(method),
                            if (is.null(threshold)) "" else paste0(", threshold = ", threshold)))
      d <- self$shape("long", "drop", log = FALSE)
      detect_outliers(d, field, private$md()$field_name[1], method, threshold)
    },
    trend = function(field, by = NULL) {
      md <- private$md()
      by <- by %||% detect_arm(md)
      self$log_code(sprintf("tr <- nx$trend(%s, by = %s)", deparse(field), deparse(by)))
      d <- self$shape("long", "drop", log = FALSE)
      if (!is.null(by) && !is.na(by)) {
        arm <- first_non_na_by(d[, c(md$field_name[1], by)], md$field_name[1])
        d[[by]] <- arm[[by]][match(d[[md$field_name[1]]], arm[[md$field_name[1]]])]
      } else by <- NULL
      trend_summary(d, field, by, self$event_order())
    },
    fit = function(formula, type = "linear", longitudinal = if (type %in% c("mixed", "mixed_logistic")) "long" else "wide",
                   repeating = "collapse") {
      self$log_code(sprintf("fit <- nx$fit(%s, type = %s, longitudinal = %s, repeating = %s)", deparse(formula),
                            deparse(type), deparse(longitudinal), deparse(repeating)))
      fit_model(formula, self$shape(longitudinal, repeating, log = FALSE, carry_constant = longitudinal == "long"), type)
    },

    # ---- Reproducibility --------------------------------------------------
    log_code = function(code, actor = self$actor) {
      self$ledger$log_script(actor, code)
      invisible(code)
    },
    # A linear R script that replays the logged analysis steps.
    script = function(since = NULL) {
      lines <- self$ledger$script_lines()
      if (!is.null(since)) lines <- lines[lines$at >= since, , drop = FALSE]
      body <- if (nrow(lines)) paste0("# ", lines$at, " by ", lines$actor, "\n", lines$code, collapse = "\n") else "# (no steps logged yet)"
      paste0(
        "# Reproducible analysis script generated by REDCapCDMSTAT ", utils::packageVersion("REDCapCDMSTAT"), "\n",
        "# Generated: ", now_stamp(), "\n",
        "# Project: ", self$info$project_title %||% "", " (PID ", self$info$project_id %||% "", ")\n",
        "# Dictionary hash: ", self$dict_hash %||% "", "\n",
        "# The API token is read from the environment or keyring, never from this file.\n\n",
        "library(REDCapCDMSTAT)\n", self$connect_code, "\n\n", body, "\n\n",
        "sessionInfo()\n")
    },
    print = function(...) {
      s <- self$summary()
      cat("<CDMSTAT> ", s$project, "\n", "  records: ", s$records, "  rows: ", s$rows, "  fields: ", s$fields,
          "  events: ", s$events, "\n  last sync: ", s$last_sync, "  dictionary: ", s$dict_hash, "\n", sep = "")
      invisible(self)
    }
  ),

  private = list(
    typed = NULL, cut = NULL, cache_mtime = NULL,
    md = function() private$cut$metadata %||% self$metadata,
    # Keep SDV status current when data changes (verified forms that were
    # edited go back to "reverify").
    after_change = function() {
      if (!is.null(self$ledger$get_setting("sdv"))) self$sdv_refresh()
      # Mirror the ledger to REDCap's Data Resolution Workflow and pull site replies.
      if (isTRUE(self$ledger$get_setting("drw", list())$enabled)) {
        r <- tryCatch(self$drw_sync("system"), error = function(e) e)
        if (inherits(r, "error")) warning("DRW sync failed: ", conditionMessage(r), call. = FALSE)
      }
    },
    need_backend = function() if (is.null(self$backend)) stop("This CDMSTAT is offline (opened from cache); no backend.", call. = FALSE),
    refresh_metadata = function() {
      md <- self$backend$metadata()
      h <- dict_hash(md)
      if (identical(h, self$dict_hash)) return(FALSE)
      diff <- dict_diff(self$metadata %||% md[0, ], md)
      self$ledger$log_schema(self$dict_hash, h, diff)
      self$metadata <- md
      self$dict_hash <- h
      self$events <- self$backend$events()
      self$mapping <- self$backend$mapping()
      self$repeating <- self$backend$repeating()
      self$info <- self$backend$info()
      private$typed <- NULL
      TRUE
    },
    patch = function(ids) {
      new <- self$backend$records(ids)
      keep <- self$raw[!self$raw[[self$id_field()]] %in% ids, , drop = FALSE]
      self$raw <- rbind_fill(keep, new)
      self$raw <- self$raw[order_ids(self$raw[[self$id_field()]]), , drop = FALSE]
      rownames(self$raw) <- NULL
      private$typed <- NULL
    },
    save_cache = function() {
      f <- file.path(self$dir, "cache.rds")
      tmp <- paste0(f, ".tmp")
      saveRDS(list(raw = self$raw, metadata = self$metadata, events = self$events, mapping = self$mapping,
                   repeating = self$repeating, info = self$info, last_sync = self$last_sync,
                   dict_hash = self$dict_hash), tmp)
      file.rename(tmp, f)
      private$cache_mtime <- file.mtime(f)
    },
    load_cache = function() {
      f <- file.path(self$dir, "cache.rds")
      if (!file.exists(f)) return(invisible())
      c <- readRDS(f)
      for (nm in names(c)) self[[nm]] <- c[[nm]]
      private$typed <- NULL
      private$cache_mtime <- file.mtime(f)
    },
    save_rules = function() {
      jsonlite::write_json(unname(self$rules), file.path(self$dir, "rules.json"), auto_unbox = TRUE, pretty = TRUE)
    },
    load_rules = function() {
      f <- file.path(self$dir, "rules.json")
      if (!file.exists(f)) return(invisible())
      r <- jsonlite::read_json(f)
      self$rules <- stats::setNames(r, vapply(r, function(z) z$id, ""))
    }
  )
)

#' Connect to a live REDCap project
#'
#' Connect to a live REDCap project.
#'
#' @param url REDCap API URL, e.g. `https://redcap.example.org/api/`.
#' @param token API token. Discouraged: prefer `token_env` or `keyring_service` so tokens never appear in code.
#' @param token_env Name of the environment variable holding the API token.
#' @param keyring_service Service name in the OS keyring (the store used by 'keyring' and 'rccola') holding the token.
#' @param dir Folder holding the local cache, ledger, data cuts and settings. Shared by the app, the API and reports.
#' @param actor User name recorded in the audit trail and script log.
#' @param sync Run an incremental sync after connecting.
#' @return A [CDMSTAT] object.
#' @examples
#' \dontrun{
#' Sys.setenv(REDCAP_API_TOKEN = "...")  # better: .Renviron or keyring
#' nx <- cdmstat_connect("https://redcap.example.org/api/")
#' nx$validate()
#' }
#' @export
cdmstat_connect <- function(url, token = NULL, token_env = "REDCAP_API_TOKEN", keyring_service = NULL,
                          dir = file.path(tools::R_user_dir("REDCapCDMSTAT", "data"), "project"),
                          actor = NULL, sync = TRUE) {
  tok <- resolve_token(token, token_env, keyring_service)
  code <- sprintf('nx <- cdmstat_connect(url = "%s", token_env = "%s"%s, dir = "%s")', url, token_env,
                  if (is.null(keyring_service)) "" else sprintf(', keyring_service = "%s"', keyring_service),
                  normalizePath(dir, winslash = "/", mustWork = FALSE))
  nx <- CDMSTAT$new(RedcapBackend$new(url, tok), dir, actor, code)
  if (sync) nx$sync()
  nx
}

#' Open the simulated demo project
#'
#' Open the simulated demo project (no REDCap server needed).
#'
#' @param dir Folder holding the local cache, ledger, data cuts and settings. Shared by the app, the API and reports.
#' @param actor User name recorded in the audit trail and script log.
#' @param reset Delete and recreate the demo project.
#' @return A [CDMSTAT] object.
#' @examples
#' nx <- cdmstat_demo(dir = file.path(tempdir(), "cdmstat-demo"), reset = TRUE)
#' nx
#' head(nx$validate())
#' nx$table_one()[1:6, 1:6]
#' @export
cdmstat_demo <- function(dir = file.path(tools::R_user_dir("REDCapCDMSTAT", "data"), "demo"), actor = NULL,
                       reset = FALSE) {
  if (reset && dir.exists(dir)) unlink(dir, recursive = TRUE)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  backend <- DemoBackend$new(file.path(dir, "demo_backend.rds"))
  nx <- CDMSTAT$new(backend, dir, actor,
                  sprintf('nx <- cdmstat_demo(dir = "%s")', normalizePath(dir, winslash = "/")))
  fresh <- is.null(nx$raw)
  nx$sync(full = fresh)
  if (fresh && !length(nx$rules)) {
    nx$add_rule("sbp_gt_dbp", "[sbp] > [dbp]", "Systolic BP must exceed diastolic BP", "error", field = "dbp")
  }
  nx
}

#' Load any study from a CDISC ODM 1.3 file (or a function returning ODM
#'
#' text), e.g. an export from REDCap Cloud, OpenClinica or Medidata Rave.
#'
#' @param file Path to a file.
#' @param dir Folder holding the local cache, ledger, data cuts and settings. Shared by the app, the API and reports.
#' @param actor User name recorded in the audit trail and script log.
#' @return A synced [CDMSTAT] object.
#' @export
cdmstat_odm <- function(file, dir = file.path(tools::R_user_dir("REDCapCDMSTAT", "data"), "odm"), actor = NULL) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  src <- if (is.function(file)) file else normalizePath(file, winslash = "/")
  code <- if (is.function(file)) sprintf('nx <- cdmstat_open("%s")', normalizePath(dir, winslash = "/")) else
    sprintf('nx <- cdmstat_odm("%s", dir = "%s")', src, normalizePath(dir, winslash = "/"))
  nx <- CDMSTAT$new(OdmBackend$new(src, file.path(dir, "odm_hashes.rds")), dir, actor, code)
  nx$sync()
  nx
}

#' Connect to a REDCap Cloud (nPhase) study through its REST API
#'
#' Endpoint paths and the auth header vary by account; take them from the API
#' documentation inside your REDCap Cloud instance. The token is read from an
#' environment variable or the OS keyring, never stored.
#'
#' @param base_url Base URL of the REDCap Cloud instance.
#' @param study Study identifier substituted for `{study}` in the endpoint paths.
#' @param token API token. Discouraged: prefer `token_env` or `keyring_service` so tokens never appear in code.
#' @param token_env Name of the environment variable holding the API token.
#' @param keyring_service Service name in the OS keyring (the store used by 'keyring' and 'rccola') holding the token.
#' @param export_path Path of the study ODM export endpoint, from your REDCap Cloud API documentation.
#' @param import_path Path of the ODM import endpoint, or `NULL` to disable write-back.
#' @param auth_header HTTP header carrying the token.
#' @param auth_prefix Prefix written before the token in the header, e.g. `"Bearer "`.
#' @param dir Folder holding the local cache, ledger, data cuts and settings. Shared by the app, the API and reports.
#' @param actor User name recorded in the audit trail and script log.
#' @return A synced [CDMSTAT] object.
#' @export
cdmstat_cloud <- function(base_url, study, token = NULL, token_env = "REDCAP_CLOUD_TOKEN", keyring_service = NULL,
                          export_path = "/api/studies/{study}/odm", import_path = NULL,
                          auth_header = "Authorization", auth_prefix = "Bearer ",
                          dir = file.path(tools::R_user_dir("REDCapCDMSTAT", "data"), "cloud"), actor = NULL) {
  tok <- resolve_token(token, token_env, keyring_service)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  backend <- RedcapCloudBackend$new(base_url, study, tok, file.path(dir, "odm_hashes.rds"), export_path,
                                    import_path, auth_header, auth_prefix)
  code <- sprintf('nx <- cdmstat_cloud("%s", "%s", token_env = "%s", export_path = "%s"%s, dir = "%s")',
                  base_url, study, token_env, export_path,
                  if (is.null(import_path)) "" else sprintf(', import_path = "%s"', import_path),
                  normalizePath(dir, winslash = "/"))
  nx <- CDMSTAT$new(backend, dir, actor, code)
  nx$sync()
  nx
}

#' Open a cache directory offline
#'
#' Open an existing cache directory offline (for reports and reproducible
#' scripts that only need frozen cuts).
#'
#' @param dir Folder holding the local cache, ledger, data cuts and settings. Shared by the app, the API and reports.
#' @param actor User name recorded in the audit trail and script log.
#' @return A [CDMSTAT] object without a backend (sync and write-back are unavailable).
#' @export
cdmstat_open <- function(dir, actor = NULL) CDMSTAT$new(NULL, dir, actor)

#' Build a CDMSTAT from environment variables, as used by the app and the API
#'
#' CDMSTAT_SOURCE: "redcap" (default when CDMSTAT_URL is set), "cloud", "odm"
#'   or "demo" (default otherwise).
#' redcap: CDMSTAT_URL, CDMSTAT_TOKEN_ENV (default REDCAP_API_TOKEN), CDMSTAT_KEYRING_SERVICE
#' cloud:  CDMSTAT_URL (base URL), CDMSTAT_STUDY, CDMSTAT_TOKEN_ENV (default REDCAP_CLOUD_TOKEN),
#'         CDMSTAT_EXPORT_PATH, CDMSTAT_IMPORT_PATH, CDMSTAT_AUTH_HEADER, CDMSTAT_AUTH_PREFIX
#' odm:    CDMSTAT_ODM_FILE
#' all:    CDMSTAT_DIR
#'
#' @return A [CDMSTAT] object.
#' @export
cdmstat_from_env <- function() {
  env <- function(k, d = "") { v <- Sys.getenv(k); if (nzchar(v)) v else d }
  url <- env("CDMSTAT_URL")
  source <- env("CDMSTAT_SOURCE", if (nzchar(url)) "redcap" else "demo")
  dir <- env("CDMSTAT_DIR")
  # A connection saved by cdmstat_setup() is used when nothing else is configured.
  if (!nzchar(url) && !nzchar(Sys.getenv("CDMSTAT_SOURCE"))) {
    cfg <- cdmstat_connection(if (nzchar(dir)) dir else default_project_dir())
    if (!is.null(cfg)) {
      return(cdmstat_connect(cfg$url, token_env = cfg$token_env %||% "REDCAP_API_TOKEN",
                             keyring_service = cfg$keyring_service, dir = if (nzchar(dir)) dir else default_project_dir()))
    }
  }
  with_dir <- function(args) { if (nzchar(dir)) args$dir <- dir; args }
  ks <- env("CDMSTAT_KEYRING_SERVICE")
  ks <- if (nzchar(ks)) ks else NULL
  switch(source,
    demo = do.call(cdmstat_demo, with_dir(list())),
    odm = do.call(cdmstat_odm, with_dir(list(file = env("CDMSTAT_ODM_FILE")))),
    cloud = do.call(cdmstat_cloud, with_dir(list(
      base_url = url, study = env("CDMSTAT_STUDY"), token_env = env("CDMSTAT_TOKEN_ENV", "REDCAP_CLOUD_TOKEN"),
      keyring_service = ks, export_path = env("CDMSTAT_EXPORT_PATH", "/api/studies/{study}/odm"),
      import_path = if (nzchar(env("CDMSTAT_IMPORT_PATH"))) env("CDMSTAT_IMPORT_PATH") else NULL,
      auth_header = env("CDMSTAT_AUTH_HEADER", "Authorization"), auth_prefix = env("CDMSTAT_AUTH_PREFIX", "Bearer ")))),
    redcap = do.call(cdmstat_connect, with_dir(list(
      url = url, token_env = env("CDMSTAT_TOKEN_ENV", "REDCAP_API_TOKEN"), keyring_service = ks))),
    stop("Unknown CDMSTAT_SOURCE: ", source, call. = FALSE))
}
