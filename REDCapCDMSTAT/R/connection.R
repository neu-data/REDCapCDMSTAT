# Connecting a real REDCap project: a diagnostic check of URL, token and
# rights, a one-call setup that stores the token outside the code, and the
# saved connection that the app and API pick up automatically.

normalise_api_url <- function(url) {
  url <- trimws(url)
  if (!grepl("^https?://", url)) url <- paste0("https://", url)
  if (!grepl("/api/?$", url)) url <- paste0(sub("/+$", "", url), "/api/")
  if (!grepl("/$", url)) url <- paste0(url, "/")
  url
}

api_error_hint <- function(msg) {
  m <- tolower(msg)
  if (grepl("could not resolve|resolve host|couldn't connect|failed to connect|timed out|timeout", m)) {
    return("The server could not be reached. Check the URL, your network/VPN, and that the server allows API access from this machine.")
  }
  if (grepl("permission|invalid token|token .*not|403|401", m)) {
    return("REDCap refused the token. Check it was copied completely, belongs to this project and server, and that your user has the API Export right.")
  }
  if (grepl("ssl|certificate", m)) return("TLS/certificate problem. Use the https address shown on the project's API page.")
  if (grepl("404", m)) return("Wrong address: the API URL usually ends in /api/ (see the project's API page).")
  "See the message above."
}

#' Check a REDCap API connection
#'
#' Runs read-only checks and explains any failure: the URL, the token's
#' format, whether the server answers, the project's settings, how many
#' records the token can read, whether the logging export is allowed (needed
#' for incremental sync) and whether the CDMSTAT DRW External Module is
#' enabled. Nothing is written to REDCap.
#'
#' @param url REDCap API URL (the `/api/` suffix is added if missing).
#' @param token API token. If `NULL`, it is read from `token_env` or `keyring_service`.
#' @param token_env Name of the environment variable holding the token.
#' @param keyring_service Service name in the OS keyring holding the token.
#' @return A data frame of checks (`check`, `result` PASS/WARN/FAIL/INFO,
#'   `detail`) with class `cdmstat_check`; the project information is in the
#'   `project` attribute.
#' @examples
#' \dontrun{
#' redcap_check("https://redcap.example.org/api/")   # token from REDCAP_API_TOKEN
#' }
#' @export
redcap_check <- function(url, token = NULL, token_env = "REDCAP_API_TOKEN", keyring_service = NULL) {
  out <- list()
  add <- function(check, result, detail = "") {
    out[[length(out) + 1]] <<- data.frame(check = check, result = result, detail = detail, stringsAsFactors = FALSE)
  }
  done <- function(project = NULL) {
    res <- do.call(rbind, out)
    class(res) <- c("cdmstat_check", "data.frame")
    attr(res, "project") <- project
    attr(res, "url") <- url
    res
  }
  url <- normalise_api_url(url)
  local_host <- grepl("^https?://(localhost|127\\.0\\.0\\.1|\\[::1\\])", url)
  add("API URL", if (grepl("^https://", url) || local_host) "PASS" else "WARN",
      if (grepl("^https://", url) || local_host) url else paste(url, "- not https: the token would travel unencrypted"))

  tok <- tryCatch(resolve_token(token, token_env, keyring_service), error = function(e) NULL, warning = function(w) token)
  if (is.null(tok) || !nzchar(tok)) {
    add("Token", "FAIL", sprintf("No token: pass `token`, set %s, or use keyring_service.", token_env))
    return(done())
  }
  fmt_ok <- grepl("^[0-9A-Fa-f]{32}$", tok) || grepl("^[0-9A-Fa-f]{64}$", tok)
  add("Token format", if (fmt_ok) "PASS" else "WARN",
      if (fmt_ok) "32 hexadecimal characters" else "REDCap project tokens are 32 hexadecimal characters; check for spaces or a partial copy")

  b <- RedcapBackend$new(url, tok)
  ver <- tryCatch(trimws(b$call("version", format = "csv")), error = function(e) e)
  if (inherits(ver, "error")) {
    add("Server and token", "FAIL", paste(conditionMessage(ver), "--", api_error_hint(conditionMessage(ver))))
    return(done())
  }
  add("Server and token", "PASS", paste("REDCap", ver))

  info <- tryCatch(b$info(), error = function(e) e)
  if (inherits(info, "error")) {
    add("Project information", "FAIL", conditionMessage(info))
    return(done())
  }
  status <- c("0" = "Development", "1" = "Production", "2" = "Analysis/Cleanup")[as.character(info$in_production %||% "0")]
  add("Project", "INFO", sprintf("PID %s: %s (%s%s%s)", info$project_id %||% "?", info$project_title %||% "",
                                  if (is.na(status)) "status unknown" else status,
                                  if (identical(as.character(info$is_longitudinal), "1")) ", longitudinal" else "",
                                  if (identical(as.character(info$has_repeating_instruments_or_events), "1")) ", repeating instruments" else ""))
  if (identical(as.character(info$in_production), "1")) {
    add("Production project", "WARN", "This is a production project with real data: test write-back, DRW sync and locking on a copy first.")
  }

  md <- tryCatch(b$metadata(), error = function(e) e)
  if (inherits(md, "error") || !NROW(md)) {
    add("Data dictionary", "FAIL", if (inherits(md, "error")) conditionMessage(md) else "empty")
    return(done(info))
  }
  add("Data dictionary", "PASS", sprintf("%d fields on %d forms", nrow(md), length(unique(md$form_name))))

  ids <- tryCatch({
    r <- as_chr_df(b$call("record", type = "flat", fields = md$field_name[1]))
    unique(r[[md$field_name[1]]])
  }, error = function(e) e)
  if (inherits(ids, "error")) {
    add("Record export", "FAIL", paste(conditionMessage(ids), "-- the user needs the API Export right and data-export access to the forms"))
  } else {
    add("Record export", if (length(ids)) "PASS" else "WARN",
        if (length(ids)) sprintf("%d records readable", length(ids)) else "No records readable (empty project, or data-access-group / export rights restrict this user)")
  }

  lg <- tryCatch(b$logging(format(Sys.time() - 3600, "%Y-%m-%d %H:%M")), error = function(e) e)
  add("Logging export (incremental sync)", if (inherits(lg, "error")) "WARN" else "PASS",
      if (inherits(lg, "error")) "Not allowed for this user: every sync will be a full download. Grant the Logging right to enable incremental sync."
      else "Allowed: syncs download only changed records")

  em <- tryCatch(b$drw("drw-status"), error = function(e) e)
  add("CDMSTAT DRW External Module", "INFO",
      if (inherits(em, "error")) "Not installed or not enabled in this project (optional; needed for REDCap-native queries and locking)"
      else sprintf("Enabled: DRW %s, schema %s, DRW rights level %s", if (isTRUE(em$drw_enabled)) "on" else "off",
                   if (isTRUE(em$schema_ok)) "ok" else "UNEXPECTED", em$rights_level %||% "?"))
  add("Write-back", "INFO", "Needs the API Import right; it is only used when you call $writeback(), $randomize() or enable the DRW module.")
  done(info)
}

#' @export
print.cdmstat_check <- function(x, ...) {
  cat("REDCap connection check:", attr(x, "url"), "\n\n")
  w <- max(nchar(x$check))
  for (i in seq_len(nrow(x))) cat(sprintf("  %-4s  %-*s  %s\n", x$result[i], w, x$check[i], x$detail[i]))
  bad <- sum(x$result == "FAIL")
  cat("\n", if (bad) sprintf("%d check(s) failed.", bad) else "Connection works.", "\n", sep = "")
  invisible(x)
}

connection_file <- function(dir) file.path(dir, "connection.json")

default_project_dir <- function() file.path(tools::R_user_dir("REDCapCDMSTAT", "data"), "project")

#' Set up a REDCap connection
#'
#' Checks the connection, stores the token outside your code, saves the
#' connection (without the token) in `dir`, and returns a synced engine. After
#' setup, [run_app()], [run_api()] and [cdmstat_from_env()] use this
#' connection automatically.
#'
#' @param url REDCap API URL.
#' @param token API token. If `NULL` you are prompted for it (input hidden).
#' @param store Where the token is kept: `"keyring"` (OS credential store; needs
#'   the 'keyring' package), `"renviron"` (your user `~/.Renviron` file, readable
#'   only by your account), or `"session"` (this R session only).
#' @param dir Folder for the cache, ledger and saved connection.
#' @param token_env Environment variable name used for the token.
#' @param sync Download the data after connecting.
#' @param actor User name recorded in the audit trail.
#' @return A [CDMSTAT] object, invisibly.
#' @examples
#' \dontrun{
#' nx <- cdmstat_setup("https://redcap.example.org/api/")   # prompts for the token
#' run_app()                                                # uses the saved connection
#' }
#' @export
cdmstat_setup <- function(url, token = NULL, store = c("renviron", "keyring", "session"), dir = default_project_dir(),
                          token_env = "REDCAP_API_TOKEN", sync = TRUE, actor = NULL) {
  store <- match.arg(store)
  url <- normalise_api_url(url)
  if (is.null(token)) {
    if (!interactive()) stop("Pass `token`, or run interactively to be prompted for it.", call. = FALSE)
    token <- if (requireNamespace("askpass", quietly = TRUE)) askpass::askpass("REDCap API token: ") else readline("REDCap API token: ")
  }
  token <- trimws(token)
  chk <- redcap_check(url, token)
  print(chk)
  if (any(chk$result == "FAIL")) stop("The connection check failed; nothing was saved.", call. = FALSE)
  info <- attr(chk, "project")
  pid <- as.character(info$project_id %||% "")

  keyring_service <- NULL
  if (store == "keyring") {
    if (!requireNamespace("keyring", quietly = TRUE)) stop("Install 'keyring', or use store = \"renviron\".", call. = FALSE)
    keyring_service <- paste0("cdmstat:", sub("^https?://([^/]+).*", "\\1", url), ":", pid)
    keyring::key_set_with_value(keyring_service, password = token)
  } else if (store == "renviron") {
    f <- file.path(Sys.getenv("HOME", path.expand("~")), ".Renviron")
    old <- if (file.exists(f)) readLines(f, warn = FALSE) else character()
    old <- old[!startsWith(old, paste0(token_env, "="))]
    writeLines(c(old, paste0(token_env, "=", token)), f)
    Sys.chmod(f, "0600")
    message("Token saved as ", token_env, " in ", f, " (readable only by your account).")
  }
  do.call(Sys.setenv, stats::setNames(list(token), token_env))

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  cfg <- list(source = "redcap", url = url, token_env = token_env, keyring_service = keyring_service,
              project_id = pid, project_title = info$project_title %||% "", store = store,
              created_at = now_stamp(), created_by = actor %||% Sys.getenv("USERNAME", Sys.getenv("USER", "")))
  jsonlite::write_json(cfg, connection_file(dir), auto_unbox = TRUE, pretty = TRUE, null = "null")
  Sys.setenv(CDMSTAT_SOURCE = "redcap", CDMSTAT_URL = url, CDMSTAT_TOKEN_ENV = token_env,
             CDMSTAT_DIR = normalizePath(dir, winslash = "/"))
  if (!is.null(keyring_service)) Sys.setenv(CDMSTAT_KEYRING_SERVICE = keyring_service)
  message("Connection saved to ", connection_file(dir), " (the token is not stored there).")
  nx <- cdmstat_connect(url, token_env = token_env, keyring_service = keyring_service, dir = dir, actor = actor, sync = sync)
  invisible(nx)
}

#' Read the saved connection of a cache directory
#'
#' @param dir Cache directory.
#' @return The saved settings as a list, or `NULL` if none.
#' @export
cdmstat_connection <- function(dir = default_project_dir()) {
  f <- connection_file(dir)
  if (!file.exists(f)) return(NULL)
  jsonlite::read_json(f, simplifyVector = TRUE)
}

#' Forget a saved connection
#'
#' Removes `connection.json` from the directory, and the token from the
#' keyring when it was stored there. The cache and ledger are kept.
#'
#' @param dir Cache directory.
#' @return `TRUE`, invisibly.
#' @export
cdmstat_disconnect <- function(dir = default_project_dir()) {
  cfg <- cdmstat_connection(dir)
  if (!is.null(cfg$keyring_service) && requireNamespace("keyring", quietly = TRUE)) {
    try(keyring::key_delete(cfg$keyring_service), silent = TRUE)
  }
  unlink(connection_file(dir))
  invisible(TRUE)
}
