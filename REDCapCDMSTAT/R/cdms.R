# Clinical data management modules of the kind commercial EDC/CDMS
# platforms such as REDCap Cloud provide: randomisation, source data
# verification, record locking, electronic signatures, medical coding,
# protocol deviations and operational site metrics. They are added to the
# engine class with $set() so each module stays in one place; all state lives
# in the ledger, so the Shiny app, the API and reports share it.

# ---- helpers ---------------------------------------------------------------

# Rows of the raw data belonging to a record, optionally one event / form.
scope_rows <- function(raw, id_field, record_id, event = NA, instance = NA) {
  keep <- raw[[id_field]] == record_id
  if (!is.na(event) && nzchar(event) && "redcap_event_name" %in% names(raw)) keep <- keep & raw$redcap_event_name == event
  if (!is.na(instance) && nzchar(instance) && "redcap_repeat_instance" %in% names(raw)) {
    keep <- keep & as.character(raw$redcap_repeat_instance) == as.character(instance)
  }
  raw[which(keep), , drop = FALSE]
}

scope_hash <- function(raw, md, id_field, record_id, event = NA, form = NA, instance = NA) {
  rows <- scope_rows(raw, id_field, record_id, event, instance)
  cols <- if (!is.na(form) && nzchar(form)) intersect(field_columns(md, md$field_name[md$form_name == form]), names(rows)) else
    setdiff(names(rows), grep("_complete$", names(rows), value = TRUE))
  v <- as.matrix(rows[, cols, drop = FALSE])
  v[is.na(v)] <- ""
  digest::digest(v, algo = "sha256")
}

# The site / centre a record belongs to: Data Access Group, else a site field.
site_of <- function(data, md, id_field) {
  if ("redcap_data_access_group" %in% names(data) && any(!is_blank(data$redcap_data_access_group))) {
    f <- "redcap_data_access_group"
    lab <- as.character(data[[f]])
  } else {
    cand <- md$field_name[md$field_type %in% c("radio", "dropdown", "text") &
                            grepl("^(site|site_id|centre|center|site_name|dag)$", md$field_name, ignore.case = TRUE)]
    if (!length(cand) || !cand[1] %in% names(data)) return(stats::setNames(rep("All", length(unique(data[[id_field]]))), unique(data[[id_field]])))
    f <- cand[1]
    i <- match(f, md$field_name)
    lab <- as.character(label_column(data[[f]], md[i, ]))
  }
  per <- tapply(lab, data[[id_field]], function(z) { z <- z[!is_blank(z)]; if (length(z)) z[1] else "Unknown" })
  stats::setNames(as.character(per), names(per))
}

norm_term <- function(x) trimws(gsub("[^a-z0-9 ]+", " ", tolower(x)))

#' A small demonstration coding dictionary. Codes are fictitious ("DEMO-");
#'
#' load your licensed MedDRA / WHODrug export with `$load_dictionary()`.
#'
#' @return Data frame with columns `code`, `term` and `synonyms`.
#' @examples
#' head(demo_dictionary())
#' @export
demo_dictionary <- function() {
  data.frame(
    code = c("DEMO-10019211", "DEMO-10028813", "DEMO-10013573", "DEMO-10037844", "DEMO-10016256",
             "DEMO-10047700", "DEMO-10012735", "DEMO-10011224", "DEMO-10003239", "DEMO-10022437"),
    term = c("Headache", "Nausea", "Dizziness", "Rash", "Fatigue", "Vomiting", "Diarrhoea", "Cough",
             "Arthralgia", "Insomnia"),
    synonyms = c("head ache|cephalalgia|head pain", "feeling sick|queasy", "lightheaded|giddiness|vertigo",
                 "skin rash|skin eruption|rash generalised", "tiredness|lethargy|asthenia", "emesis|throwing up",
                 "diarrhea|loose stools", "coughing", "joint pain|joint ache", "sleeplessness|trouble sleeping"),
    stringsAsFactors = FALSE)
}

match_term <- function(verbatim, dict) {
  v <- norm_term(verbatim)
  terms <- norm_term(dict$term)
  syn <- strsplit(ifelse(is.na(dict$synonyms), "", dict$synonyms), "|", fixed = TRUE)
  exact <- which(terms == v | vapply(syn, function(s) v %in% norm_term(s), logical(1)))
  if (length(exact)) return(list(code = dict$code[exact[1]], term = dict$term[exact[1]], score = 1, method = "exact"))
  cand <- c(terms, unlist(lapply(seq_along(syn), function(i) norm_term(syn[[i]]))))
  owner <- c(seq_along(terms), unlist(lapply(seq_along(syn), function(i) rep(i, length(syn[[i]])))))
  keep <- nzchar(cand)
  cand <- cand[keep]
  owner <- owner[keep]
  d <- as.numeric(utils::adist(v, cand))
  sim <- 1 - d / pmax(nchar(v), nchar(cand))
  # Also reward a dictionary term contained in a longer verbatim ("severe headache").
  contained <- vapply(cand, function(cc) nchar(cc) > 3 && grepl(paste0("\\b", cc, "\\b"), v), logical(1))
  sim[contained] <- pmax(sim[contained], 0.9)
  b <- which.max(sim)
  list(code = dict$code[owner[b]], term = dict$term[owner[b]], score = round(sim[b], 3), method = "fuzzy")
}


# Forms of a record (optionally one event / form) that hold data, as
# record/event/form/instance rows for EDC-side locking.
edc_units <- function(raw, md, id_field, record_id, event = NA, form = NA, repeating_forms = character()) {
  rows <- scope_rows(raw, id_field, record_id, event)
  out <- list()
  rep_col <- rows$redcap_repeat_instrument %||% rep("", nrow(rows))
  for (i in seq_len(nrow(rows))) {
    forms <- if (nzchar(rep_col[i] %||% "")) rep_col[i] else setdiff(unique(md$form_name), repeating_forms)
    if (!is.na(form)) forms <- intersect(forms, form)
    for (f in forms) {
      cols <- intersect(setdiff(field_columns(md, md$field_name[md$form_name == f]), id_field), names(rows))
      if (!length(cols) || all(is_blank(unlist(rows[i, cols, drop = FALSE])))) next
      out[[length(out) + 1]] <- list(record = record_id, event = rows$redcap_event_name[i] %||% NA, form = f,
                                     instance = if (nzchar(rep_col[i] %||% "")) rows$redcap_repeat_instance[i] else NA)
    }
  }
  out
}

edc_call <- function(nx, action, payload) {
  cfg <- nx$ledger$get_setting("drw", list())
  nx$backend$drw(action, payload, prefix = cfg$prefix %||% "cdmstat_drw")
}

edc_enabled <- function(nx, what) {
  cfg <- nx$ledger$get_setting("drw", list())
  isTRUE(cfg$enabled) && isTRUE(cfg[[what]]) && !is.null(nx$backend)
}

# ---- randomisation ---------------------------------------------------------

CDMSTAT$set("public", "setup_randomization", function(strata = character(), ratio = NULL, block_sizes = c(4, 6),
                                                      per_stratum = 200, seed = NULL, arm_field = NULL,
                                                      actor = self$actor, force = FALSE) {
  arm_field <- arm_field %||% detect_arm(self$metadata)
  if (is.na(arm_field)) stop("No arm field found; pass arm_field.", call. = FALSE)
  used <- self$ledger$get("SELECT COUNT(*) AS n FROM rand_schedule WHERE record_id IS NOT NULL")$n
  if (used > 0 && !force) stop("Randomisation has started (", used, " allocated); the schedule cannot be replaced.", call. = FALSE)
  arms <- parse_choices(self$metadata$select_choices_or_calculations[self$metadata$field_name == arm_field])$code
  ratio <- ratio %||% rep(1, length(arms))
  unit <- rep(arms, ratio)
  bad <- block_sizes[block_sizes %% length(unit) != 0]
  if (length(bad)) stop("Block sizes must be multiples of ", length(unit), " (got ", paste(bad, collapse = ", "), ").", call. = FALSE)
  levels_of <- lapply(strata, function(f) {
    i <- match(f, self$metadata$field_name)
    if (is.na(i)) stop("Unknown stratification field: ", f, call. = FALSE)
    ch <- parse_choices(self$metadata$select_choices_or_calculations[i])
    if (self$metadata$field_type[i] == "yesno") c("0", "1") else ch$code
  })
  grid <- if (length(strata)) expand.grid(stats::setNames(levels_of, strata), stringsAsFactors = FALSE) else data.frame(all = "all")
  key <- apply(grid, 1, function(r) paste(paste0(names(grid), "=", r), collapse = ";"))
  seed <- seed %||% sample.int(.Machine$integer.max, 1)
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  set.seed(seed)
  self$ledger$with(function(con) DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM rand_schedule")
    for (k in key) {
      seqs <- character()
      blocks <- integer()
      b <- 0
      while (length(seqs) < per_stratum) {
        b <- b + 1
        size <- if (length(block_sizes) > 1) sample(block_sizes, 1) else block_sizes
        seqs <- c(seqs, sample(rep(unit, size / length(unit))))
        blocks <- c(blocks, rep(b, size))
      }
      DBI::dbWriteTable(con, "rand_schedule", data.frame(stratum = k, seq = seq_along(seqs), block = blocks, arm = seqs,
                                                         record_id = NA_character_, assigned_by = NA_character_,
                                                         assigned_at = NA_character_), append = TRUE)
    }
  }))
  cfg <- list(arm_field = arm_field, strata = strata, ratio = ratio, block_sizes = block_sizes,
              per_stratum = per_stratum, seed_hash = digest::digest(seed, algo = "sha256"))
  self$ledger$set_setting("randomization", cfg, actor)
  self$ledger$activity(actor, "randomization", "setup", as.character(jsonlite::toJSON(cfg, auto_unbox = TRUE)))
  invisible(cfg)
})

# Allocate the next concealed slot in the subject's stratum and write the
# arm back to the EDC when the backend supports it.
CDMSTAT$set("public", "randomize", function(record_id, actor = self$actor, write_back = TRUE) {
  cfg <- self$ledger$get_setting("randomization")
  if (is.null(cfg)) stop("Randomisation is not set up.", call. = FALSE)
  done <- self$ledger$get("SELECT * FROM rand_schedule WHERE record_id = ?", list(record_id))
  if (nrow(done)) stop("Record ", record_id, " was already randomised (", done$assigned_at, ").", call. = FALSE)
  d <- self$data()
  rows <- d[d[[self$id_field()]] == record_id & is_blank(d$redcap_repeat_instrument %||% rep(NA, nrow(d))), , drop = FALSE]
  if (!nrow(rows)) stop("Record ", record_id, " not found.", call. = FALSE)
  strata <- unlist(cfg$strata)
  vals <- vapply(strata, function(f) { v <- rows[[f]][!is_blank(rows[[f]])]; if (length(v)) as.character(v[1]) else NA_character_ }, "")
  if (any(is.na(vals))) stop("Missing stratification value(s): ", paste(strata[is.na(vals)], collapse = ", "), call. = FALSE)
  if (!is.na(cfg$arm_field) && any(!is_blank(rows[[cfg$arm_field]]))) {
    stop("Record ", record_id, " already has an arm recorded in the EDC.", call. = FALSE)
  }
  key <- if (length(strata)) paste(paste0(strata, "=", vals), collapse = ";") else "all=all"
  at <- now_stamp()
  slot <- self$ledger$with(function(con) DBI::dbWithTransaction(con, {
    s <- DBI::dbGetQuery(con, "SELECT seq, arm FROM rand_schedule WHERE stratum = ? AND record_id IS NULL ORDER BY seq LIMIT 1", params = list(key))
    if (!nrow(s)) stop("Stratum ", key, " has no allocations left.", call. = FALSE)
    DBI::dbExecute(con, "UPDATE rand_schedule SET record_id = ?, assigned_by = ?, assigned_at = ? WHERE stratum = ? AND seq = ?",
                   params = list(record_id, actor, at, key, s$seq))
    s
  }))
  ch <- parse_choices(self$metadata$select_choices_or_calculations[self$metadata$field_name == cfg$arm_field])
  written <- FALSE
  if (write_back && !is.null(self$backend)) {
    first_ev <- if ("redcap_event_name" %in% names(rows)) {
      form <- self$metadata$form_name[self$metadata$field_name == cfg$arm_field]
      evs <- if (!is.null(self$mapping) && nrow(self$mapping)) self$mapping$unique_event_name[self$mapping$form == form] else rows$redcap_event_name
      intersect(evs, rows$redcap_event_name)[1]
    } else NULL
    df <- data.frame(stats::setNames(list(record_id), self$id_field()), stringsAsFactors = FALSE)
    if (!is.null(first_ev)) df$redcap_event_name <- first_ev
    df[[cfg$arm_field]] <- slot$arm
    written <- tryCatch({ self$backend$import(df) > 0 }, error = function(e) FALSE)
    if (written) self$sync()
  }
  self$ledger$activity(actor, "randomization", "allocate",
                       sprintf("record %s stratum %s seq %s -> arm %s%s", record_id, key, slot$seq, slot$arm,
                               if (written) " (written to EDC)" else ""))
  list(record_id = record_id, stratum = key, seq = slot$seq, arm = slot$arm,
       arm_label = ch$label[match(slot$arm, ch$code)], written_to_edc = written, at = at)
})

CDMSTAT$set("public", "randomization_status", function() {
  cfg <- self$ledger$get_setting("randomization")
  if (is.null(cfg)) return(list(configured = FALSE))
  s <- self$ledger$get("SELECT stratum, arm, COUNT(*) AS slots, SUM(record_id IS NOT NULL) AS used FROM rand_schedule GROUP BY stratum, arm")
  a <- self$ledger$get("SELECT stratum, seq, arm, record_id, assigned_by, assigned_at FROM rand_schedule WHERE record_id IS NOT NULL ORDER BY assigned_at")
  list(configured = TRUE, config = cfg, balance = s, allocations = a)
})

# ---- source data verification ----------------------------------------------

# Define the SDV plan: forms with critical fields are always verified, plus
# the first `first_n` subjects of every site and a random `sample_rate`
# fraction of the rest (risk-based SDV).
CDMSTAT$set("public", "sdv_plan", function(critical_fields = character(), first_n = 3, sample_rate = 0.2,
                                           seed = 1, actor = self$actor) {
  cfg <- list(critical_fields = critical_fields, first_n = first_n, sample_rate = sample_rate, seed = seed)
  self$ledger$set_setting("sdv", cfg, actor)
  self$ledger$activity(actor, "sdv", "plan", as.character(jsonlite::toJSON(cfg, auto_unbox = TRUE)))
  self$sdv_refresh()
})

# Recompute SDV units from the current data: add new units, and send
# verified units whose data changed back to "reverify".
CDMSTAT$set("public", "sdv_refresh", function() {
  cfg <- self$ledger$get_setting("sdv", list(critical_fields = character(), first_n = 3, sample_rate = 0.2, seed = 1))
  raw <- self$raw
  md <- self$metadata
  id <- self$id_field()
  sites <- site_of(self$data(), md, id)
  ids <- unique(raw[[id]])
  ids <- ids[order_ids(ids)]
  set_old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  set.seed(cfg$seed)
  sampled <- ids[stats::runif(length(ids)) < cfg$sample_rate]
  if (!is.null(set_old)) assign(".Random.seed", set_old, envir = globalenv())
  first <- unlist(lapply(split(ids, sites[ids]), function(z) utils::head(z, cfg$first_n)))
  crit_forms <- unique(md$form_name[md$field_name %in% unlist(cfg$critical_fields)])
  rep_col <- raw$redcap_repeat_instrument %||% rep("", nrow(raw))
  units <- list()
  for (i in seq_len(nrow(raw))) {
    rec <- raw[[id]][i]
    ev <- raw$redcap_event_name[i] %||% ""
    inst <- if (nzchar(rep_col[i] %||% "")) as.character(raw$redcap_repeat_instance[i]) else ""
    forms <- if (nzchar(rep_col[i] %||% "")) rep_col[i] else setdiff(unique(md$form_name), self$repeating_forms())
    for (f in forms) {
      cols <- intersect(setdiff(field_columns(md, md$field_name[md$form_name == f]), id), names(raw))
      vals <- unlist(raw[i, cols, drop = FALSE])
      if (!length(cols) || all(is_blank(vals))) next
      reason <- c(if (f %in% crit_forms) "critical data", if (rec %in% first) "first subjects at site",
                  if (rec %in% sampled) "random sample")
      v <- as.matrix(raw[i, cols, drop = FALSE])
      v[is.na(v)] <- ""
      units[[length(units) + 1]] <- data.frame(
        unit_key = paste(rec, ev, f, inst, sep = "|"), record_id = rec, event = ev, form = f, instance = inst,
        site = unname(sites[rec]), required = as.integer(length(reason) > 0),
        reason = paste(reason, collapse = ", "), data_hash = digest::digest(v, algo = "md5"), stringsAsFactors = FALSE)
    }
  }
  u <- do.call(rbind, units)
  self$ledger$with(function(con) DBI::dbWithTransaction(con, {
    old <- DBI::dbGetQuery(con, "SELECT unit_key, status, verified_hash FROM sdv")
    new <- u[!u$unit_key %in% old$unit_key, , drop = FALSE]
    if (nrow(new)) {
      new$status <- "pending"
      new$verified_hash <- NA_character_
      new$verified_by <- NA_character_
      new$verified_at <- NA_character_
      new$comment <- NA_character_
      DBI::dbWriteTable(con, "sdv", new, append = TRUE)
    }
    upd <- u[u$unit_key %in% old$unit_key, , drop = FALSE]
    for (k in seq_len(nrow(upd))) {
      o <- old[old$unit_key == upd$unit_key[k], ]
      status <- if (o$status == "verified" && !identical(o$verified_hash, upd$data_hash[k])) "reverify" else o$status
      DBI::dbExecute(con, "UPDATE sdv SET data_hash = ?, required = ?, reason = ?, site = ?, status = ? WHERE unit_key = ?",
                     params = list(upd$data_hash[k], upd$required[k], upd$reason[k], upd$site[k], status, upd$unit_key[k]))
    }
  }))
  invisible(self$sdv_status())
})

CDMSTAT$set("public", "sdv_status", function(required_only = FALSE) {
  self$ledger$get(paste("SELECT * FROM sdv", if (required_only) "WHERE required = 1" else "", "ORDER BY site, record_id, event, form"))
})

CDMSTAT$set("public", "sdv_verify", function(unit_keys, actor = self$actor, comment = "Verified against source") {
  at <- now_stamp()
  for (k in unit_keys) {
    n <- self$ledger$run("UPDATE sdv SET status = 'verified', verified_hash = data_hash, verified_by = ?, verified_at = ?, comment = ? WHERE unit_key = ?",
                         list(actor, at, comment, k))
    if (!n) stop("Unknown SDV unit: ", k, call. = FALSE)
    self$ledger$activity(actor, "sdv", "verify", k)
    if (edc_enabled(self, "verify_sdv")) {
      # Mirror to REDCap: mark each value on the verified form as verified in the DRW.
      parts <- strsplit(k, "|", fixed = TRUE)[[1]]
      parts <- c(parts, rep("", 4 - length(parts)))
      md <- self$metadata
      rows <- scope_rows(self$raw, self$id_field(), parts[1], parts[2], parts[4])
      if (!nzchar(parts[4]) && "redcap_repeat_instrument" %in% names(rows)) rows <- rows[is_blank(rows$redcap_repeat_instrument), , drop = FALSE]
      fields <- setdiff(md$field_name[md$form_name == parts[3] & md$field_type != "descriptive"], self$id_field())
      for (f in fields) {
        cols <- intersect(field_columns(md, f), names(rows))
        if (!length(cols) || all(is_blank(unlist(rows[1, cols, drop = FALSE])))) next
        r <- tryCatch(edc_call(self, "verify-field", list(record = parts[1], field = f, event = if (nzchar(parts[2])) parts[2] else NULL,
                                                         instance = if (nzchar(parts[4])) parts[4] else NULL,
                                                         repeat_instrument = if (nzchar(parts[4])) parts[3] else NULL,
                                                         comment = comment, actor = actor)), error = function(e) e)
        if (inherits(r, "error")) self$ledger$activity(actor, "sdv", "drw-verify-failed", paste(k, f, conditionMessage(r)))
      }
    }
  }
  invisible(length(unit_keys))
})

# ---- locking and electronic signatures -------------------------------------

CDMSTAT$set("public", "lock", function(record_id, reason, event = NA, form = NA, actor = self$actor) {
  if (missing(reason) || !nzchar(trimws(reason))) stop("A reason is required to lock.", call. = FALSE)
  h <- scope_hash(self$raw, self$metadata, self$id_field(), record_id, event, form)
  id <- new_id("LOCK")
  self$ledger$run("INSERT INTO locks VALUES (?,?,?,?,?,?,?,?,?,?,?)",
                  list(id, record_id, if (is.na(event)) NA_character_ else event, if (is.na(form)) NA_character_ else form,
                       h, reason, actor, now_stamp(), NA_character_, NA_character_, NA_character_))
  self$ledger$activity(actor, "lock", "lock", paste0(id, " record ", record_id, if (!is.na(event)) paste0(" event ", event) else "", if (!is.na(form)) paste0(" form ", form) else ""))
  if (edc_enabled(self, "lock_in_edc")) {
    n <- 0L
    for (u in edc_units(self$raw, self$metadata, self$id_field(), record_id, event, form, self$repeating_forms())) {
      edc_call(self, "lock-form", list(record = u$record, form = u$form, event = u$event, instance = u$instance,
                                       reason = reason, actor = actor))
      n <- n + 1L
    }
    self$ledger$activity(actor, "lock", "edc-lock", sprintf("%s: %d form(s) locked in REDCap", id, n))
  }
  id
})

CDMSTAT$set("public", "unlock", function(lock_id, reason, actor = self$actor) {
  if (missing(reason) || !nzchar(trimws(reason))) stop("A reason is required to unlock.", call. = FALSE)
  n <- self$ledger$run("UPDATE locks SET unlocked_by = ?, unlocked_at = ?, unlock_reason = ? WHERE lock_id = ? AND unlocked_at IS NULL",
                       list(actor, now_stamp(), reason, lock_id))
  if (!n) stop("No active lock ", lock_id, call. = FALSE)
  self$ledger$activity(actor, "lock", "unlock", paste(lock_id, reason))
  if (edc_enabled(self, "lock_in_edc")) {
    l <- self$ledger$get("SELECT * FROM locks WHERE lock_id = ?", list(lock_id))
    for (u in edc_units(self$raw, self$metadata, self$id_field(), l$record_id, l$event, l$form, self$repeating_forms())) {
      edc_call(self, "unlock-form", list(record = u$record, form = u$form, event = u$event, instance = u$instance,
                                         reason = reason, actor = actor))
    }
  }
  invisible(TRUE)
})

# Active locks with an integrity check: "intact" or "CHANGED AFTER LOCK".
CDMSTAT$set("public", "locks", function(active_only = TRUE) {
  l <- self$ledger$get(paste("SELECT * FROM locks", if (active_only) "WHERE unlocked_at IS NULL" else "", "ORDER BY locked_at DESC"))
  l$integrity <- vapply(seq_len(nrow(l)), function(i) {
    h <- scope_hash(self$raw, self$metadata, self$id_field(), l$record_id[i], l$event[i], l$form[i])
    if (identical(h, l$data_hash[i])) "intact" else "CHANGED AFTER LOCK"
  }, "")
  l
})

# Sign a record (21 CFR Part 11 style): the signature binds the signer, the
# meaning of the signature and a hash of the record's data at signing time.
# The caller must have re-verified the signer's identity (the gateway does
# this by re-checking the user's credential) and passes identity_verified.
CDMSTAT$set("public", "esign", function(record_id, meaning, actor = self$actor, identity_verified = FALSE) {
  if (!isTRUE(identity_verified)) stop("Signer identity must be re-verified before signing.", call. = FALSE)
  if (missing(meaning) || !nzchar(trimws(meaning))) stop("The meaning of the signature is required.", call. = FALSE)
  h <- scope_hash(self$raw, self$metadata, self$id_field(), record_id)
  id <- new_id("SIG")
  self$ledger$run("INSERT INTO signatures VALUES (?,?,?,?,?,?)", list(id, record_id, h, actor, meaning, now_stamp()))
  self$ledger$activity(actor, "esign", "sign", sprintf("%s record %s: %s", id, record_id, meaning))
  id
})

CDMSTAT$set("public", "signatures", function() {
  s <- self$ledger$get("SELECT * FROM signatures ORDER BY signed_at DESC")
  s$status <- vapply(seq_len(nrow(s)), function(i) {
    h <- scope_hash(self$raw, self$metadata, self$id_field(), s$record_id[i])
    if (identical(h, s$data_hash[i])) "valid" else "INVALID - data changed after signing"
  }, "")
  s
})

# ---- medical coding --------------------------------------------------------

# Load a coding dictionary (e.g. a MedDRA LLT/PT or WHODrug export) with
# columns code, term and optionally synonyms ("|"-separated).
CDMSTAT$set("public", "load_dictionary", function(dict, name = "default", actor = self$actor) {
  if (is.character(dict) && length(dict) == 1) dict <- utils::read.csv(dict, stringsAsFactors = FALSE)
  if (!all(c("code", "term") %in% names(dict))) stop("Dictionary needs columns 'code' and 'term'.", call. = FALSE)
  if (!"synonyms" %in% names(dict)) dict$synonyms <- ""
  saveRDS(dict[, c("code", "term", "synonyms")], file.path(self$dir, paste0("dictionary_", name, ".rds")))
  self$ledger$activity(actor, "coding", "dictionary", sprintf("%s: %d terms", name, nrow(dict)))
  invisible(nrow(dict))
})

CDMSTAT$set("public", "dictionary", function(name = "default") {
  f <- file.path(self$dir, paste0("dictionary_", name, ".rds"))
  if (file.exists(f)) readRDS(f) else demo_dictionary()
})

# Code verbatim terms: exact/synonym matches are auto-coded, close fuzzy
# matches are queued with a suggestion for a coder to approve. Changed
# verbatims are re-coded; approved codes for unchanged verbatims are kept.
CDMSTAT$set("public", "code_terms", function(field = "ae_term", dictionary = "default", auto = TRUE,
                                             min_score = 0.6, actor = self$actor) {
  d <- self$data()
  if (!field %in% names(d)) stop("Field '", field, "' not in data.", call. = FALSE)
  dict <- self$dictionary(dictionary)
  rows <- which(!is_blank(d[[field]]))
  old <- self$ledger$get("SELECT coding_key, verbatim, status FROM coding WHERE field = ?", list(field))
  at <- now_stamp()
  n_auto <- n_pending <- 0
  for (i in rows) {
    key <- paste(d[[self$id_field()]][i], d$redcap_event_name[i] %||% "", d$redcap_repeat_instance[i] %||% "", field, sep = "|")
    verb <- as.character(d[[field]][i])
    o <- old[old$coding_key == key, ]
    if (nrow(o) && identical(o$verbatim, verb)) next
    m <- match_term(verb, dict)
    exact <- auto && m$method == "exact"
    status <- if (exact) "auto-coded" else if (m$score >= min_score) "pending" else "uncoded"
    if (exact) n_auto <- n_auto + 1 else n_pending <- n_pending + 1
    self$ledger$run("INSERT OR REPLACE INTO coding VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
                    list(key, d[[self$id_field()]][i], d$redcap_event_name[i] %||% NA_character_,
                         as.character(d$redcap_repeat_instance[i] %||% NA), field, verb,
                         if (exact) m$code else NA_character_, if (exact) m$term else NA_character_,
                         m$code, m$term, m$score, m$method, status,
                         if (exact) "system" else NA_character_, if (exact) at else NA_character_,
                         NA_character_, NA_character_))
  }
  self$ledger$activity(actor, "coding", "run", sprintf("%s: %d auto-coded, %d for review", field, n_auto, n_pending))
  invisible(list(auto_coded = n_auto, for_review = n_pending))
})

CDMSTAT$set("public", "coding", function(status = NULL) {
  if (is.null(status)) self$ledger$get("SELECT * FROM coding ORDER BY status, verbatim") else
    self$ledger$get(sprintf("SELECT * FROM coding WHERE status IN (%s) ORDER BY verbatim", paste(rep("?", length(status)), collapse = ",")), as.list(status))
})

# Approve a suggestion, or assign another dictionary code.
CDMSTAT$set("public", "approve_coding", function(coding_key, code = NULL, dictionary = "default", actor = self$actor) {
  r <- self$ledger$get("SELECT * FROM coding WHERE coding_key = ?", list(coding_key))
  if (!nrow(r)) stop("Unknown coding item: ", coding_key, call. = FALSE)
  code <- code %||% r$suggestion_code
  dict <- self$dictionary(dictionary)
  term <- dict$term[match(code, dict$code)]
  if (is.na(term)) stop("Code ", code, " is not in the dictionary.", call. = FALSE)
  self$ledger$run("UPDATE coding SET code = ?, term = ?, status = 'approved', approved_by = ?, approved_at = ?, method = ? WHERE coding_key = ?",
                  list(code, term, actor, now_stamp(), if (identical(code, r$suggestion_code)) r$method else "manual", coding_key))
  self$ledger$activity(actor, "coding", "approve", sprintf("%s '%s' -> %s %s", coding_key, r$verbatim, code, term))
  invisible(term)
})

# ---- protocol deviations ---------------------------------------------------

CDMSTAT$set("public", "log_deviation", function(record_id, category, description, severity = "minor", event = NA,
                                                source = "manual", issue_key = NA, actor = self$actor) {
  id <- new_id("DEV")
  self$ledger$run("INSERT INTO deviations VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
                  list(id, if (is.na(issue_key)) NA_character_ else issue_key, record_id,
                       if (is.na(event)) NA_character_ else event, category, description, severity, "open", source,
                       actor, now_stamp(), NA_character_, NA_character_, NA_character_))
  self$ledger$activity(actor, "deviation", "log", sprintf("%s record %s: %s", id, record_id, description))
  id
})

CDMSTAT$set("public", "close_deviation", function(dev_id, comment, actor = self$actor) {
  if (missing(comment) || !nzchar(trimws(comment))) stop("A closing comment is required.", call. = FALSE)
  n <- self$ledger$run("UPDATE deviations SET status = 'closed', closed_by = ?, closed_at = ?, close_comment = ? WHERE dev_id = ? AND status = 'open'",
                       list(actor, now_stamp(), comment, dev_id))
  if (!n) stop("No open deviation ", dev_id, call. = FALSE)
  self$ledger$activity(actor, "deviation", "close", paste(dev_id, comment))
  invisible(TRUE)
})

CDMSTAT$set("public", "deviations", function() self$ledger$get("SELECT * FROM deviations ORDER BY reported_at DESC"))

# Detect out-of-window visits from the event schedule (day offsets) and log
# each as a protocol deviation once.
CDMSTAT$set("public", "check_visit_windows", function(anchor = "consent_date", visit_field = "visit_date",
                                                      window = 7, actor = "system") {
  ev <- self$events
  if (is.null(ev) || !nrow(ev) || !"day_offset" %in% names(ev)) return(invisible(0L))
  d <- self$shape("long", "drop", labels = FALSE, log = FALSE)
  if (!all(c(anchor, visit_field) %in% names(d))) stop("Fields not found: ", anchor, ", ", visit_field, call. = FALSE)
  id <- self$id_field()
  base <- first_non_na_by(d[, c(id, anchor)], id)
  d$anchor <- base[[anchor]][match(d[[id]], base[[id]])]
  d$target <- as.numeric(ev$day_offset[match(d$redcap_event_name, ev$unique_event_name)])
  d$actual <- as.numeric(d[[visit_field]] - d$anchor)
  out <- d[!is.na(d$actual) & !is.na(d$target) & abs(d$actual - d$target) > window, , drop = FALSE]
  known <- self$ledger$get("SELECT issue_key FROM deviations WHERE issue_key IS NOT NULL")$issue_key
  n <- 0L
  for (i in seq_len(nrow(out))) {
    k <- paste("window", out[[id]][i], out$redcap_event_name[i], sep = "|")
    if (k %in% known) next
    self$log_deviation(out[[id]][i], "Visit out of window",
                       sprintf("Visit on day %d; scheduled day %d +/- %d", as.integer(out$actual[i]), as.integer(out$target[i]), window),
                       severity = if (abs(out$actual[i] - out$target[i]) > 3 * window) "major" else "minor",
                       event = out$redcap_event_name[i], source = "auto", issue_key = k, actor = actor)
    n <- n + 1L
  }
  invisible(n)
})

# ---- operational metrics ---------------------------------------------------

# Per-site operational metrics for monitoring and portfolio oversight.
CDMSTAT$set("public", "site_metrics", function() {
  d <- self$data()
  md <- self$metadata
  id <- self$id_field()
  sites <- site_of(d, md, id)
  arm <- detect_arm(md)
  randomized <- if (!is.na(arm) && arm %in% names(d)) unique(d[[id]][!is_blank(d[[arm]])]) else character()
  q <- self$ledger$queries()
  q$site <- unname(sites[q$record_id])
  now <- Sys.time()
  q$age <- as.numeric(difftime(now, as.POSIXct(q$created_at), units = "days"))
  q$ttr <- as.numeric(difftime(as.POSIXct(q$updated_at), as.POSIXct(q$created_at), units = "days"))
  sdv <- self$sdv_status(required_only = TRUE)
  dev <- self$deviations()
  dev$site <- unname(sites[dev$record_id])
  issues <- self$validate()
  issues$site <- unname(sites[issues$record_id])
  out <- do.call(rbind, lapply(sort(unique(sites)), function(s) {
    recs <- names(sites)[sites == s]
    qs <- q[!is.na(q$site) & q$site == s, , drop = FALSE]
    open <- qs$status %in% c("open", "answered")
    sv <- sdv[sdv$site == s, , drop = FALSE]
    data.frame(site = s, subjects = length(recs), randomized = sum(recs %in% randomized),
               open_queries = sum(open), queries_per_subject = round(sum(open) / max(1, length(recs)), 2),
               median_open_query_age_days = if (any(open)) round(stats::median(qs$age[open]), 1) else NA,
               median_days_to_resolution = if (any(qs$status %in% c("resolved", "closed"))) round(stats::median(qs$ttr[qs$status %in% c("resolved", "closed")]), 1) else NA,
               rule_violations = sum(issues$site == s, na.rm = TRUE),
               sdv_required = nrow(sv), sdv_verified_pct = if (nrow(sv)) round(100 * mean(sv$status == "verified"), 1) else NA,
               deviations = sum(dev$site == s, na.rm = TRUE), major_deviations = sum(dev$site == s & dev$severity == "major", na.rm = TRUE),
               stringsAsFactors = FALSE)
  }))
  out
})

# Cumulative enrolment by site over time, from a consent/enrolment date.
CDMSTAT$set("public", "enrollment_curve", function(date_field = NULL) {
  md <- self$metadata
  date_field <- date_field %||% {
    dates <- md$field_name[field_kinds(md) == "date"]
    c(dates[grepl("consent|enrol|enroll|random|screen", dates)], dates)[1]
  }
  d <- self$data()
  id <- self$id_field()
  first <- first_non_na_by(d[, c(id, date_field)], id)
  first$site <- unname(site_of(d, md, id)[first[[id]]])
  first <- first[!is.na(first[[date_field]]), ]
  first <- first[order(first[[date_field]]), ]
  first$n <- stats::ave(seq_len(nrow(first)), first$site, FUN = seq_along)
  data.frame(date = first[[date_field]], site = first$site, cumulative = first$n, stringsAsFactors = FALSE)
})

CDMSTAT$set("public", "activity", function(module = NULL, n = 200) {
  if (is.null(module)) self$ledger$get("SELECT * FROM activity ORDER BY id DESC LIMIT ?", list(as.integer(n))) else
    self$ledger$get("SELECT * FROM activity WHERE module = ? ORDER BY id DESC LIMIT ?", list(module, as.integer(n)))
})
