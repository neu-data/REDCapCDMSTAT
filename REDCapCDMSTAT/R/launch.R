#' Launch the Shiny data management and analysis app
#'
#' @param engine an existing CDMSTAT object; if NULL, one is built from the environment (see [cdmstat_from_env()]), defaulting to the demo project.
#' @param ... Passed to [shiny::runApp()].
#' @return Does not return while the app is running.
#' @export
run_app <- function(engine = NULL, ...) {
  for (p in c("shiny", "bslib", "plotly", "DT")) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Install '", p, "' to run the app.", call. = FALSE)
  }
  old <- options(REDCapCDMSTAT.instance = engine)
  on.exit(options(old))
  shiny::runApp(system.file("app", package = "REDCapCDMSTAT"), ...)
}

#' Run the plumber REST API used by the standalone web app
#'
#' Bind to localhost and put the FastAPI gateway (web/gateway) in front of it;
#' when CDMSTAT_GATEWAY_SECRET is set every request must carry that secret.
#'
#' @param host Interface to listen on; keep `127.0.0.1` behind the gateway.
#' @param port TCP port.
#' @return Does not return while the API is running.
#' @export
run_api <- function(host = "127.0.0.1", port = 8000) {
  if (!requireNamespace("plumber", quietly = TRUE)) stop("Install 'plumber' to run the API.", call. = FALSE)
  pr <- plumber::pr(system.file("api", "plumber.R", package = "REDCapCDMSTAT"))
  pr <- plumber::pr_set_serializer(pr, plumber::serializer_unboxed_json(na = "null"))
  pr <- plumber::pr_set_error(pr, function(req, res, err) {
    if (res$status == 200L) res$status <- 500L
    list(error = conditionMessage(err))
  })
  plumber::pr_run(pr, host = host, port = port, docs = FALSE)
}

#' Render the reproducible Quarto analysis report for a frozen data cut
#'
#' Uses the Quarto CLI when available, then rmarkdown (needs pandoc), and
#' finally knitr + the 'markdown' package, which need no external tools.
#'
#' @param dir Folder holding the local cache, ledger, data cuts and settings. Shared by the app, the API and reports.
#' @param cut_id Identifier of a frozen data cut.
#' @param output_dir Folder the rendered report is copied to.
#' @param require_signoff Refuse to use a cut that has not been signed off.
#' @return Path of the rendered HTML report.
#' @export
render_report <- function(dir, cut_id, output_dir = getwd(), require_signoff = FALSE) {
  src <- system.file("quarto", "cdmstat-report.qmd", package = "REDCapCDMSTAT")
  work <- tempfile("cdmstat-report-")
  dir.create(work)
  params <- list(dir = normalizePath(dir, winslash = "/"), cut_id = cut_id, require_signoff = require_signoff)
  out_name <- paste0("cdmstat-report-", cut_id, ".html")
  if (requireNamespace("quarto", quietly = TRUE) && !is.null(quarto::quarto_path())) {
    file.copy(src, file.path(work, "cdmstat-report.qmd"))
    quarto::quarto_render(file.path(work, "cdmstat-report.qmd"), execute_params = params, output_file = out_name)
  } else if (requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()) {
    file.copy(src, file.path(work, "cdmstat-report.Rmd"))
    rmarkdown::render(file.path(work, "cdmstat-report.Rmd"), output_format = rmarkdown::html_document(toc = TRUE),
                      output_file = out_name, params = params, quiet = TRUE, envir = new.env())
  } else {
    if (!requireNamespace("knitr", quietly = TRUE) || !requireNamespace("markdown", quietly = TRUE)) {
      stop("Install Quarto, pandoc, or the 'knitr' and 'markdown' packages to render reports.", call. = FALSE)
    }
    old <- setwd(work)
    on.exit(setwd(old), add = TRUE)
    file.copy(src, "cdmstat-report.Rmd")
    env <- new.env()
    env$params <- params
    knitr::knit("cdmstat-report.Rmd", "cdmstat-report.md", quiet = TRUE, envir = env)
    md <- readLines("cdmstat-report.md", warn = FALSE)
    # Drop the YAML header, which markdown would print as text.
    if (length(md) && md[1] == "---") md <- md[-seq_len(which(md == "---")[2])]
    writeLines(c("# Statistical analysis report", "", md), "cdmstat-report.md")
    markdown::mark_html("cdmstat-report.md", output = out_name, options = "+toc+embed_resources")
  }
  dest <- file.path(output_dir, out_name)
  file.copy(file.path(work, out_name), dest, overwrite = TRUE)
  normalizePath(dest, winslash = "/")
}
