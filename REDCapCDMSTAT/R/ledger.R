ledger_schema <- c(
  "CREATE TABLE IF NOT EXISTS queries (
     query_id TEXT PRIMARY KEY, issue_key TEXT, record_id TEXT, event TEXT, instance TEXT,
     field TEXT, value TEXT, message TEXT, source TEXT, rule TEXT, severity TEXT,
     status TEXT, assigned_to TEXT, created_by TEXT, created_at TEXT, updated_at TEXT)",
  "CREATE TABLE IF NOT EXISTS query_audit (
     audit_id INTEGER PRIMARY KEY AUTOINCREMENT, query_id TEXT, action TEXT, actor TEXT,
     comment TEXT, old_status TEXT, new_status TEXT, at TEXT)",
  "CREATE TABLE IF NOT EXISTS cuts (
     cut_id TEXT PRIMARY KEY, label TEXT, note TEXT, created_by TEXT, created_at TEXT,
     sha256 TEXT, path TEXT, n_records INTEGER, n_rows INTEGER, dict_hash TEXT,
     signed_off_by TEXT, signed_off_at TEXT, signoff_comment TEXT)",
  "CREATE TABLE IF NOT EXISTS schema_log (
     id INTEGER PRIMARY KEY AUTOINCREMENT, at TEXT, old_hash TEXT, new_hash TEXT, summary TEXT, diff_json TEXT)",
  "CREATE TABLE IF NOT EXISTS sync_log (
     id INTEGER PRIMARY KEY AUTOINCREMENT, at TEXT, mode TEXT, n_changed INTEGER,
     n_deleted INTEGER, metadata_changed INTEGER, seconds REAL)",
  "CREATE TABLE IF NOT EXISTS script_log (
     id INTEGER PRIMARY KEY AUTOINCREMENT, at TEXT, actor TEXT, code TEXT)",
  "CREATE TABLE IF NOT EXISTS settings (key TEXT PRIMARY KEY, value TEXT, updated_by TEXT, updated_at TEXT)",
  "CREATE TABLE IF NOT EXISTS rand_schedule (
     stratum TEXT, seq INTEGER, block INTEGER, arm TEXT, record_id TEXT, assigned_by TEXT, assigned_at TEXT,
     PRIMARY KEY (stratum, seq))",
  "CREATE TABLE IF NOT EXISTS sdv (
     unit_key TEXT PRIMARY KEY, record_id TEXT, event TEXT, form TEXT, instance TEXT, site TEXT,
     required INTEGER, reason TEXT, status TEXT, data_hash TEXT, verified_hash TEXT,
     verified_by TEXT, verified_at TEXT, comment TEXT)",
  "CREATE TABLE IF NOT EXISTS locks (
     lock_id TEXT PRIMARY KEY, record_id TEXT, event TEXT, form TEXT, data_hash TEXT, reason TEXT,
     locked_by TEXT, locked_at TEXT, unlocked_by TEXT, unlocked_at TEXT, unlock_reason TEXT)",
  "CREATE TABLE IF NOT EXISTS signatures (
     sig_id TEXT PRIMARY KEY, record_id TEXT, data_hash TEXT, signer TEXT, meaning TEXT, signed_at TEXT)",
  "CREATE TABLE IF NOT EXISTS coding (
     coding_key TEXT PRIMARY KEY, record_id TEXT, event TEXT, instance TEXT, field TEXT, verbatim TEXT,
     code TEXT, term TEXT, suggestion_code TEXT, suggestion_term TEXT, score REAL, method TEXT, status TEXT,
     coded_by TEXT, coded_at TEXT, approved_by TEXT, approved_at TEXT)",
  "CREATE TABLE IF NOT EXISTS deviations (
     dev_id TEXT PRIMARY KEY, issue_key TEXT, record_id TEXT, event TEXT, category TEXT, description TEXT,
     severity TEXT, status TEXT, source TEXT, reported_by TEXT, reported_at TEXT, closed_by TEXT,
     closed_at TEXT, close_comment TEXT)",
  "CREATE TABLE IF NOT EXISTS drw_link (
     query_id TEXT PRIMARY KEY, status_id INTEGER, last_res_id INTEGER, pushed_status TEXT, synced_at TEXT)",
  "CREATE TABLE IF NOT EXISTS activity (
     id INTEGER PRIMARY KEY AUTOINCREMENT, at TEXT, actor TEXT, module TEXT, action TEXT, detail TEXT)",
  "CREATE INDEX IF NOT EXISTS idx_queries_status ON queries(status)",
  "CREATE INDEX IF NOT EXISTS idx_audit_query ON query_audit(query_id)"
)

# Allowed query state transitions. Every transition needs an actor and a
# comment (reason for change), which lands in the append-only audit table.
query_transitions <- list(
  open = c("answered", "resolved", "closed"),
  answered = c("open", "resolved", "closed"),
  resolved = c("open", "closed"),
  closed = c("open")
)

#' SQLite ledger
#'
#' Stores queries and their audit trail, data cuts, dictionary drift, sync history, the script log and all CDMS module state.
#'
#' @section Methods:
#' `$add_query()`, `$transition()`, `$comment()`, `$queries()`, `$audit()`, `$add_cut()`, `$sign_off()`, `$cuts()`, `$log_schema()`, `$schema_history()`, `$log_sync()`, `$sync_history()`, `$log_script()`, `$script_lines()`, `$activity()`, `$get_setting()`, `$set_setting()`, `$get()`, `$run()`.
#' @format An [R6::R6Class] generator.
#' @usage NULL
#' @export
Ledger <- R6::R6Class("Ledger",
  public = list(
    path = NULL,
    initialize = function(path) {
      self$path <- path
      self$with(function(con) {
        DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
        for (s in ledger_schema) DBI::dbExecute(con, s)
      })
    },
    with = function(fn) {
      con <- DBI::dbConnect(RSQLite::SQLite(), self$path)
      on.exit(DBI::dbDisconnect(con))
      DBI::dbExecute(con, "PRAGMA busy_timeout = 10000")
      fn(con)
    },
    get = function(sql, params = list()) {
      self$with(function(con) {
        if (length(params)) DBI::dbGetQuery(con, sql, params = unname(params)) else DBI::dbGetQuery(con, sql)
      })
    },
    run = function(sql, params = list()) {
      self$with(function(con) {
        if (length(params)) DBI::dbExecute(con, sql, params = unname(params)) else DBI::dbExecute(con, sql)
      })
    },

    add_query = function(record_id, field, message, event = NA, instance = NA, value = NA,
                         source = "manual", rule = NA, severity = "warning", issue_key = NA,
                         actor = "unknown", assigned_to = NA) {
      id <- new_id("Q")
      at <- now_stamp()
      chr <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NA_character_ else as.character(x)
      self$with(function(con) {
        DBI::dbWithTransaction(con, {
          DBI::dbExecute(con, "INSERT INTO queries VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
                         params = list(id, chr(issue_key), chr(record_id), chr(event), chr(instance),
                                       chr(field), chr(value), chr(message), source, chr(rule), severity,
                                       "open", chr(assigned_to), actor, at, at))
          DBI::dbExecute(con, "INSERT INTO query_audit (query_id, action, actor, comment, old_status, new_status, at)
                               VALUES (?,?,?,?,?,?,?)",
                         params = list(id, "open", actor, chr(message), NA_character_, "open", at))
        })
      })
      id
    },
    transition = function(query_id, status, actor, comment) {
      if (is.null(comment) || !nzchar(trimws(comment))) stop("A comment (reason for change) is required.", call. = FALSE)
      cur <- self$get("SELECT status FROM queries WHERE query_id = ?", list(query_id))
      if (!nrow(cur)) stop("Unknown query: ", query_id, call. = FALSE)
      old <- cur$status[1]
      if (!status %in% query_transitions[[old]]) {
        stop(sprintf("Cannot move a query from '%s' to '%s'.", old, status), call. = FALSE)
      }
      at <- now_stamp()
      self$with(function(con) {
        DBI::dbWithTransaction(con, {
          DBI::dbExecute(con, "UPDATE queries SET status = ?, updated_at = ? WHERE query_id = ?",
                         params = list(status, at, query_id))
          DBI::dbExecute(con, "INSERT INTO query_audit (query_id, action, actor, comment, old_status, new_status, at)
                               VALUES (?,?,?,?,?,?,?)",
                         params = list(query_id, "transition", actor, comment, old, status, at))
        })
      })
      invisible(status)
    },
    comment = function(query_id, actor, comment) {
      cur <- self$get("SELECT status FROM queries WHERE query_id = ?", list(query_id))
      if (!nrow(cur)) stop("Unknown query: ", query_id, call. = FALSE)
      self$run("INSERT INTO query_audit (query_id, action, actor, comment, old_status, new_status, at)
                VALUES (?,?,?,?,?,?,?)",
               list(query_id, "comment", actor, comment, cur$status[1], cur$status[1], now_stamp()))
      invisible(TRUE)
    },
    queries = function(status = NULL) {
      if (is.null(status) || !length(status)) {
        return(self$get("SELECT * FROM queries ORDER BY created_at DESC"))
      }
      self$get(sprintf("SELECT * FROM queries WHERE status IN (%s) ORDER BY created_at DESC",
                       paste(rep("?", length(status)), collapse = ",")), as.list(status))
    },
    audit = function(query_id = NULL) {
      if (is.null(query_id)) return(self$get("SELECT * FROM query_audit ORDER BY audit_id DESC"))
      self$get("SELECT * FROM query_audit WHERE query_id = ? ORDER BY audit_id", list(query_id))
    },

    add_cut = function(cut_id, label, note, actor, sha, path, n_records, n_rows, dict_hash) {
      self$run("INSERT INTO cuts VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)",
               list(cut_id, label, note %||% NA_character_, actor, now_stamp(), sha, path,
                    as.integer(n_records), as.integer(n_rows), dict_hash,
                    NA_character_, NA_character_, NA_character_))
    },
    sign_off = function(cut_id, actor, comment) {
      cur <- self$get("SELECT signed_off_by FROM cuts WHERE cut_id = ?", list(cut_id))
      if (!nrow(cur)) stop("Unknown cut: ", cut_id, call. = FALSE)
      if (!is.na(cur$signed_off_by[1])) stop("Cut already signed off by ", cur$signed_off_by[1], call. = FALSE)
      self$run("UPDATE cuts SET signed_off_by = ?, signed_off_at = ?, signoff_comment = ? WHERE cut_id = ?",
               list(actor, now_stamp(), comment, cut_id))
    },
    cuts = function() self$get("SELECT * FROM cuts ORDER BY created_at DESC"),

    log_schema = function(old_hash, new_hash, diff) {
      self$run("INSERT INTO schema_log (at, old_hash, new_hash, summary, diff_json) VALUES (?,?,?,?,?)",
               list(now_stamp(), old_hash %||% NA_character_, new_hash, summarise_diff(diff),
                    as.character(jsonlite::toJSON(diff, auto_unbox = TRUE))))
    },
    schema_history = function() self$get("SELECT id, at, old_hash, new_hash, summary FROM schema_log ORDER BY id DESC"),
    log_sync = function(mode, n_changed, n_deleted, metadata_changed, seconds) {
      self$run("INSERT INTO sync_log (at, mode, n_changed, n_deleted, metadata_changed, seconds) VALUES (?,?,?,?,?,?)",
               list(now_stamp(), mode, as.integer(n_changed), as.integer(n_deleted),
                    as.integer(metadata_changed), as.numeric(seconds)))
    },
    sync_history = function(n = 50) self$get("SELECT * FROM sync_log ORDER BY id DESC LIMIT ?", list(as.integer(n))),
    log_script = function(actor, code) {
      self$run("INSERT INTO script_log (at, actor, code) VALUES (?,?,?)", list(now_stamp(), actor, code))
    },
    script_lines = function() self$get("SELECT * FROM script_log ORDER BY id"),
    # Append-only activity trail for the CDMS modules (randomisation, SDV,
    # locks, signatures, coding, deviations).
    activity = function(actor, module, action, detail = "") {
      self$run("INSERT INTO activity (at, actor, module, action, detail) VALUES (?,?,?,?,?)",
               list(now_stamp(), actor, module, action, as.character(detail)))
    },
    get_setting = function(key, default = NULL) {
      r <- self$get("SELECT value FROM settings WHERE key = ?", list(key))
      if (!nrow(r)) default else jsonlite::fromJSON(r$value[1], simplifyVector = TRUE)
    },
    set_setting = function(key, value, actor) {
      self$run("INSERT OR REPLACE INTO settings (key, value, updated_by, updated_at) VALUES (?,?,?,?)",
               list(key, as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null")), actor, now_stamp()))
    },
    stamp = function() {
      self$get("SELECT (SELECT MAX(updated_at) FROM queries) AS q, (SELECT COUNT(*) FROM query_audit) AS a,
                       (SELECT COUNT(*) FROM cuts) AS c, (SELECT MAX(signed_off_at) FROM cuts) AS s,
                       (SELECT COUNT(*) FROM activity) AS act")
    }
  )
)
