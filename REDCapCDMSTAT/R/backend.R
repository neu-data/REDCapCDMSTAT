# Backends expose the same small surface: info, metadata, events, mapping,
# repeating, records, logging, user_rights, import. CDMSTAT never talks to the
# REDCap API directly, so the demo backend can stand in for a live server.

redcap_call <- function(url, token, content, ..., format = "json") {
  args <- list(token = token, content = content, format = format, returnFormat = "json", ...)
  args <- args[!vapply(args, is.null, logical(1))]
  flat <- list()
  for (nm in names(args)) {
    v <- args[[nm]]
    if (nm %in% c("records", "fields", "events", "forms")) {
      for (i in seq_along(v)) flat[[sprintf("%s[%d]", nm, i - 1)]] <- as.character(v[i])
    } else {
      flat[[nm]] <- as.character(v)
    }
  }
  req <- httr2::request(url)
  req <- do.call(httr2::req_body_form, c(list(req), flat))
  req <- httr2::req_retry(req, max_tries = 3)
  req <- httr2::req_timeout(req, 300)
  req <- httr2::req_error(req, body = function(resp) {
    tryCatch(httr2::resp_body_json(resp)$error, error = function(e) httr2::resp_body_string(resp))
  })
  txt <- httr2::resp_body_string(httr2::req_perform(req))
  if (format == "json") jsonlite::fromJSON(txt, simplifyVector = TRUE) else txt
}

#' Backend for a live REDCap server. The token is kept private and is never
#' written to the cache, ledger or generated scripts.
RedcapBackend <- R6::R6Class("RedcapBackend",
  public = list(
    url = NULL,
    initialize = function(url, token) {
      self$url <- url
      private$token <- token
    },
    call = function(content, ..., format = "json") {
      redcap_call(self$url, private$token, content, ..., format = format)
    },
    info = function() as.list(self$call("project")),
    metadata = function() as_chr_df(self$call("metadata")),
    events = function() tryCatch(as_chr_df(self$call("event")), error = function(e) data.frame()),
    mapping = function() tryCatch(as_chr_df(self$call("formEventMapping")), error = function(e) data.frame()),
    repeating = function() tryCatch(as_chr_df(self$call("repeatingFormsEvents")), error = function(e) data.frame()),
    records = function(records = NULL, batch = 500) {
      pull <- function(ids) {
        as_chr_df(self$call("record", type = "flat", rawOrLabel = "raw",
                            exportDataAccessGroups = "true", records = ids))
      }
      if (is.null(records)) return(pull(NULL))
      out <- NULL
      for (chunk in split(records, ceiling(seq_along(records) / batch))) out <- rbind_fill(out, pull(chunk))
      out %||% data.frame()
    },
    logging = function(begin = NULL) as_chr_df(self$call("log", beginTime = begin)),
    user_rights = function() as_chr_df(self$call("user")),
    import = function(df) {
      df[] <- lapply(df, function(v) { v <- as.character(v); v[is.na(v)] <- ""; v })
      res <- self$call("record", type = "flat", overwriteBehavior = "normal",
                       data = as.character(jsonlite::toJSON(df, dataframe = "rows")))
      as.integer(res$count %||% res)
    }
  ),
  private = list(token = NULL)
)

#' Offline backend that behaves like a REDCap project, persisted to a file so
#' that several processes (Shiny, plumber) share the same simulated server.
DemoBackend <- R6::R6Class("DemoBackend",
  public = list(
    path = NULL,
    initialize = function(path, seed = 42) {
      self$path <- path
      if (!file.exists(path)) private$save(demo_project(seed))
    },
    state = function() readRDS(self$path),
    info = function() self$state()$info,
    metadata = function() self$state()$metadata,
    events = function() self$state()$events,
    mapping = function() self$state()$mapping,
    repeating = function() self$state()$repeating,
    records = function(records = NULL) {
      r <- self$state()$records
      if (!is.null(records)) r <- r[r$record_id %in% records, , drop = FALSE]
      r
    },
    logging = function(begin = NULL) {
      log <- self$state()$log
      if (!is.null(begin) && nrow(log)) {
        log <- log[as.POSIXct(log$timestamp, format = "%Y-%m-%d %H:%M") >= as.POSIXct(begin, format = "%Y-%m-%d %H:%M"), ]
      }
      log
    },
    user_rights = function() data.frame(username = "demo_user", data_export_tool = "1", stringsAsFactors = FALSE),
    import = function(df, user = "cdmstat_api") {
      st <- self$state()
      n <- 0
      for (i in seq_len(nrow(df))) {
        key <- st$records$record_id == df$record_id[i]
        if ("redcap_event_name" %in% names(df)) key <- key & st$records$redcap_event_name == df$redcap_event_name[i]
        key <- key & is_blank(st$records$redcap_repeat_instrument)
        idx <- which(key)
        if (!length(idx)) next
        for (cc in setdiff(names(df), c("record_id", "redcap_event_name"))) {
          if (cc %in% names(st$records) && !is_blank(df[[cc]][i])) st$records[idx, cc] <- as.character(df[[cc]][i])
        }
        st$log <- private$log_row(st$log, user, paste("Update record", df$record_id[i]),
                                  paste(names(df), collapse = ", "))
        n <- n + 1
      }
      private$save(st)
      n
    },
    # Simulate a data entry user saving a value in REDCap.
    edit = function(record, field, value, event = "baseline_arm_1", user = "site_coordinator") {
      st <- self$state()
      idx <- which(st$records$record_id == record & st$records$redcap_event_name == event &
                     is_blank(st$records$redcap_repeat_instrument))
      if (!length(idx)) stop("No such record/event: ", record, " / ", event)
      st$records[idx, field] <- as.character(value)
      st$log <- private$log_row(st$log, user, sprintf("Update record %s (%s)", record, event),
                                sprintf("%s = '%s'", field, value))
      private$save(st)
      invisible(TRUE)
    },
    # Simulate enrolling a new subject at the first event.
    add_record = function(record, values = list(), event = "baseline_arm_1", user = "site_coordinator") {
      st <- self$state()
      if (record %in% st$records$record_id) stop("Record ", record, " exists.")
      r <- stats::setNames(as.list(rep("", ncol(st$records))), names(st$records))
      r$record_id <- record
      r$redcap_event_name <- event
      for (nm in names(values)) r[[nm]] <- as.character(values[[nm]])
      st$records <- rbind(st$records, as.data.frame(r, stringsAsFactors = FALSE))
      st$log <- private$log_row(st$log, user, paste("Create record", record), "")
      private$save(st)
      invisible(TRUE)
    },
    delete_record = function(record, user = "site_coordinator") {
      st <- self$state()
      st$records <- st$records[st$records$record_id != record, , drop = FALSE]
      st$log <- private$log_row(st$log, user, paste("Delete record", record), "")
      private$save(st)
      invisible(TRUE)
    },
    # Simulate a designer editing the data dictionary online.
    change_metadata = function(field, column, value, user = "project_designer") {
      st <- self$state()
      st$metadata[st$metadata$field_name == field, column] <- value
      st$log <- private$log_row(st$log, user, "Manage/Design", sprintf("Edit project field %s", field))
      private$save(st)
      invisible(TRUE)
    }
  ),
  private = list(
    save = function(st) saveRDS(st, self$path),
    log_row = function(log, user, action, details) {
      rbind(log, data.frame(timestamp = format(Sys.time(), "%Y-%m-%d %H:%M"), username = user,
                            action = action, details = details, stringsAsFactors = FALSE))
    }
  )
)

#' Resolve a REDCap API token without hard-coding it in scripts.
#'
#' Order: explicit `token` (discouraged), environment variable, then the OS
#' keyring via the 'keyring' package (the same store 'rccola' uses).
resolve_token <- function(token = NULL, token_env = "REDCAP_API_TOKEN", keyring_service = NULL,
                          keyring_user = NULL) {
  if (!is.null(token)) {
    warning("Passing a token directly is discouraged; use an environment variable or keyring.", call. = FALSE)
    return(token)
  }
  tok <- Sys.getenv(token_env)
  if (nzchar(tok)) return(tok)
  if (!is.null(keyring_service) && requireNamespace("keyring", quietly = TRUE)) {
    return(keyring::key_get(keyring_service, username = keyring_user))
  }
  stop("No REDCap API token found. Set ", token_env, " or supply keyring_service.", call. = FALSE)
}

#' Backend for any study available as a CDISC ODM 1.3 export (REDCap Cloud,
#' OpenClinica, Medidata Rave, Castor, REDCap itself ...).
#'
#' `fetch` is a function returning the ODM XML text. ODM snapshots have no
#' change log, so the backend keeps a hash of every subject and reports the
#' subjects whose data changed (or disappeared) since the previous read, in
#' the same shape as the REDCap logging export. Incremental sync, the query
#' ledger and auto-resolution therefore work unchanged.
OdmBackend <- R6::R6Class("OdmBackend",
  public = list(
    fetch = NULL, state_path = NULL, label = "ODM",
    initialize = function(fetch, state_path) {
      if (!requireNamespace("xml2", quietly = TRUE)) stop("Install 'xml2' to read ODM.", call. = FALSE)
      self$fetch <- if (is.function(fetch)) fetch else { f <- fetch; function() paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n") }
      self$state_path <- state_path
    },
    read = function(force = FALSE) {
      if (force || is.null(private$cache) || difftime(Sys.time(), private$at, units = "secs") > 5) {
        private$cache <- read_odm(self$fetch())
        private$at <- Sys.time()
      }
      private$cache
    },
    info = function() self$read()$info,
    metadata = function() self$read(force = TRUE)$metadata,
    events = function() self$read()$events,
    mapping = function() self$read()$mapping,
    repeating = function() self$read()$repeating,
    records = function(records = NULL) {
      r <- self$read()$records
      private$remember(r)
      if (!is.null(records)) r <- r[r$record_id %in% records, , drop = FALSE]
      r
    },
    logging = function(begin = NULL) {
      r <- self$read(force = TRUE)$records
      old <- if (file.exists(self$state_path)) readRDS(self$state_path) else character()
      new <- private$hashes(r)
      changed <- names(new)[is.na(old[names(new)]) | old[names(new)] != new]
      deleted <- setdiff(names(old), names(new))
      stamp <- format(Sys.time(), "%Y-%m-%d %H:%M")
      data.frame(timestamp = rep(stamp, length(changed) + length(deleted)),
                 username = self$label,
                 action = c(paste("Update record", changed), paste("Delete record", deleted)),
                 details = "", stringsAsFactors = FALSE)
    },
    user_rights = function() data.frame(),
    import = function(df) stop("Write-back is not configured for this ODM source.", call. = FALSE)
  ),
  private = list(
    cache = NULL, at = NULL,
    hashes = function(r) {
      if (!nrow(r)) return(character())
      vapply(split(r, r$record_id), function(d) digest::digest(d[, setdiff(names(d), character())], algo = "md5"), "")
    },
    remember = function(r) saveRDS(private$hashes(r), self$state_path)
  )
)

#' Backend for REDCap Cloud (nPhase).
#'
#' REDCap Cloud is a separate commercial EDC whose REST API is documented
#' inside each customer's account rather than publicly, so every path and the
#' authentication header are configurable. Data are exchanged as CDISC ODM,
#' which REDCap Cloud exports and imports. Set `export_path` (and optionally
#' `import_path`) to the study ODM endpoints given in your REDCap Cloud API
#' documentation; `{study}` is replaced by `study`.
RedcapCloudBackend <- R6::R6Class("RedcapCloudBackend", inherit = OdmBackend,
  public = list(
    base_url = NULL, study = NULL, export_path = NULL, import_path = NULL,
    auth_header = NULL, auth_prefix = NULL, label = "REDCap Cloud",
    initialize = function(base_url, study, token, state_path,
                          export_path = "/api/studies/{study}/odm",
                          import_path = NULL, auth_header = "Authorization", auth_prefix = "Bearer ") {
      self$base_url <- sub("/+$", "", base_url)
      self$study <- study
      self$export_path <- export_path
      self$import_path <- import_path
      self$auth_header <- auth_header
      self$auth_prefix <- auth_prefix
      private$token <- token
      super$initialize(function() self$request("GET", self$export_path), state_path)
    },
    request = function(method, path, body = NULL) {
      url <- paste0(self$base_url, gsub("{study}", utils::URLencode(self$study, reserved = TRUE), path, fixed = TRUE))
      req <- httr2::request(url)
      req <- do.call(httr2::req_headers, c(list(req), stats::setNames(list(paste0(self$auth_prefix, private$token)), self$auth_header)))
      req <- httr2::req_method(req, method)
      if (!is.null(body)) req <- httr2::req_body_raw(req, body, type = "application/xml")
      req <- httr2::req_retry(req, max_tries = 3)
      req <- httr2::req_timeout(req, 300)
      httr2::resp_body_string(httr2::req_perform(req))
    },
    #' Write values back as an ODM ClinicalData upsert.
    import = function(df) {
      if (is.null(self$import_path)) stop("Set import_path to enable write-back to REDCap Cloud.", call. = FALSE)
      st <- self$read()
      st$records <- df
      self$request("POST", self$import_path, write_odm(st, clinical = TRUE))
      nrow(df)
    }
  ),
  private = list(token = NULL)
)
