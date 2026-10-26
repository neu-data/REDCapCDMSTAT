#' REDCapCDMSTAT: clinical data management and statistical analysis for REDCap
#'
#' One engine for data managers and statisticians on REDCap, REDCap Cloud and
#' CDISC ODM studies: incremental sync with dictionary drift detection, live
#' branching and calculation logic, rule-based validation, an audited query
#' ledger mirrored to REDCap's Data Resolution Workflow, randomisation, source
#' data verification, locking, electronic signatures, medical coding,
#' protocol deviations, site metrics, hashed data cuts, baseline tables,
#' mixed and survival models, CDISC SDTM / define.xml / ODM export and a
#' reproducible script log. A Shiny app, a plumber API, a Quarto report and a
#' portfolio dashboard are built on it.
#'
#' Start with [cdmstat_demo()], then see [CDMSTAT] for the methods.
#'
#' @keywords internal
#' @importFrom R6 R6Class
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
"_PACKAGE"
