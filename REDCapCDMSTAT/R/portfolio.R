#' Portfolio oversight across several projects
#'
#' Summarises queries, rule violations, SDV progress, deviations and locks
#' for several studies side by side, plus site metrics across all of them.
#'
#' @param projects a named list of CDMSTAT objects, or of cache directories (opened offline with [cdmstat_open()]).
#' @return A list with `studies` (one row per project), `sites` (site metrics
#'   with a project column) and `queries_by_status`.
#' @export
cdmstat_portfolio <- function(projects) {
  if (is.null(names(projects)) || any(!nzchar(names(projects)))) {
    names(projects) <- vapply(seq_along(projects), function(i) {
      p <- projects[[i]]
      if (inherits(p, "CDMSTAT")) p$info$project_title %||% paste("Project", i) else basename(p)
    }, "")
  }
  nxs <- lapply(projects, function(p) if (inherits(p, "CDMSTAT")) p else cdmstat_open(p))
  studies <- do.call(rbind, lapply(names(nxs), function(nm) {
    nx <- nxs[[nm]]
    s <- nx$summary()
    q <- nx$queries()
    open <- q$status %in% c("open", "answered")
    sdv <- nx$sdv_status(required_only = TRUE)
    dev <- nx$deviations()
    rz <- nx$randomization_status()
    data.frame(
      project = nm, title = s$project, source = s$source, records = s$records,
      randomized = if (isTRUE(rz$configured)) nrow(rz$allocations) else NA_integer_,
      open_queries = sum(open),
      oldest_open_query_days = if (any(open)) round(max(as.numeric(difftime(Sys.time(), as.POSIXct(q$created_at[open]), units = "days"))), 1) else NA,
      rule_violations = nrow(nx$validate()),
      sdv_verified_pct = if (nrow(sdv)) round(100 * mean(sdv$status == "verified"), 1) else NA,
      open_deviations = sum(dev$status == "open"), major_deviations = sum(dev$severity == "major"),
      locked_records = length(unique(nx$locks()$record_id)),
      last_sync = s$last_sync, stringsAsFactors = FALSE)
  }))
  sites <- do.call(rbind, lapply(names(nxs), function(nm) {
    m <- nxs[[nm]]$site_metrics()
    if (!nrow(m)) return(NULL)
    cbind(project = nm, m, stringsAsFactors = FALSE)
  }))
  qs <- do.call(rbind, lapply(names(nxs), function(nm) {
    q <- nxs[[nm]]$queries()
    if (!nrow(q)) return(NULL)
    cbind(project = nm, as.data.frame(table(status = factor(q$status, c("open", "answered", "resolved", "closed"))),
                                      stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  }))
  rownames(studies) <- NULL
  list(studies = studies, sites = sites, queries_by_status = qs)
}

#' Launch the portfolio dashboard for several project cache directories
#'
#' @param dirs named character vector of CDMSTAT cache directories; defaults to the comma-separated CDMSTAT_PORTFOLIO environment variable.
#' @param ... Passed to [shiny::runApp()].
#' @return Does not return while the app is running.
#' @export
run_portfolio <- function(dirs = NULL, ...) {
  for (p in c("shiny", "bslib", "DT", "plotly")) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Install '", p, "' to run the portfolio app.", call. = FALSE)
  }
  dirs <- dirs %||% trimws(strsplit(Sys.getenv("CDMSTAT_PORTFOLIO"), ",")[[1]])
  if (!length(dirs) || !all(nzchar(dirs))) stop("Pass dirs or set CDMSTAT_PORTFOLIO.", call. = FALSE)
  old <- options(REDCapCDMSTAT.portfolio = dirs)
  on.exit(options(old))
  shiny::runApp(system.file("portfolio", package = "REDCapCDMSTAT"), ...)
}
