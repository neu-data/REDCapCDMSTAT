# REDCap CDMSTAT installation (IQ) and operational (OQ) qualification.
#
#   Rscript validation/run_validation.R [path/to/php]
#
# Writes validation/output/validation-<timestamp>.md (and .html when the
# 'markdown' package is installed) plus a CSV of every test result. The
# requirements traceability matrix is validation/requirements.csv; each
# requirement passes only if every test mapped to it passes. Performance
# qualification (PQ) on the deploying site's own data follows
# validation/PQ-protocol.md.

args <- commandArgs(trailingOnly = TRUE)
here <- normalizePath(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), winslash = "/")
root <- normalizePath(file.path(here, ".."), winslash = "/")
pkg_dir <- file.path(root, "REDCapCDMSTAT")
module_dir <- file.path(root, "redcap-module", "cdmstat_drw_v1.0.0")
out_dir <- file.path(here, "output")
dir.create(out_dir, showWarnings = FALSE)
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
lines <- character()
say <- function(...) lines <<- c(lines, paste0(...))

# ---- IQ ------------------------------------------------------------------
iq <- list()
check <- function(id, what, ok, detail = "") {
  iq[[length(iq) + 1]] <<- data.frame(id = id, check = what, result = if (isTRUE(ok)) "PASS" else "FAIL",
                                      detail = detail, stringsAsFactors = FALSE)
}
check("IQ-01", "R version >= 4.1.0", getRversion() >= "4.1.0", as.character(getRversion()))
desc <- read.dcf(file.path(pkg_dir, "DESCRIPTION"))
imports <- trimws(sub("\\(.*", "", strsplit(desc[, "Imports"], ",")[[1]]))
for (p in imports) {
  ok <- requireNamespace(p, quietly = TRUE)
  check("IQ-02", paste("Required package installed:", p), ok, if (ok) as.character(utils::packageVersion(p)) else "missing")
}
inst <- requireNamespace("REDCapCDMSTAT", quietly = TRUE)
check("IQ-03", "REDCapCDMSTAT installed and matches source version", inst &&
        identical(as.character(utils::packageVersion("REDCapCDMSTAT")), unname(desc[, "Version"])),
      if (inst) as.character(utils::packageVersion("REDCapCDMSTAT")) else "not installed")
for (f in c("app/app.R", "api/plumber.R", "quarto/cdmstat-report.qmd", "portfolio/app.R")) {
  check("IQ-04", paste("Installed component present:", f), inst && nzchar(system.file(f, package = "REDCapCDMSTAT")))
}
tmp <- tempfile("iq")
ledger_ok <- tryCatch({
  l <- REDCapCDMSTAT::Ledger$new(tempfile(fileext = ".sqlite"))
  id <- l$add_query("1", "f", "IQ check", actor = "iq")
  nrow(l$audit(id)) == 1
}, error = function(e) FALSE)
check("IQ-05", "Ledger database can be created, written and read", ledger_ok)
for (f in c("CDMSTATDRW.php", "config.json")) check("IQ-06", paste("External Module file present:", f), file.exists(file.path(module_dir, f)))
hashes <- vapply(list.files(file.path(pkg_dir, "R"), full.names = TRUE), function(f) digest::digest(file = f, algo = "sha256"), "")
iq <- do.call(rbind, iq)

# ---- OQ: R test suite ------------------------------------------------------
res <- as.data.frame(testthat::test_dir(file.path(pkg_dir, "tests", "testthat"), reporter = "silent",
                                        stop_on_failure = FALSE, package = "REDCapCDMSTAT", load_package = "installed"))
r_tests <- data.frame(suite = "R", group = sub("^test-|\\.R$", "", res$file), test = res$test,
                      expectations = res$nb, result = ifelse(res$failed > 0 | res$error, "FAIL", "PASS"),
                      stringsAsFactors = FALSE)

# ---- OQ: External Module tests (PHP) ----------------------------------------
php <- if (length(args)) args[1] else Sys.getenv("CDMSTAT_PHP", unname(Sys.which("php")))
php_tests <- data.frame(suite = character(), group = character(), test = character(), expectations = integer(),
                        result = character(), stringsAsFactors = FALSE)
php_note <- ""
if (nzchar(php) && file.exists(php)) {
  out <- system2(php, shQuote(file.path(module_dir, "tests", "run_tests.php")), stdout = TRUE, stderr = TRUE)
  grp <- ""
  for (ln in out) {
    if (grepl("^[a-z]", ln) && !grepl("checks,", ln)) grp <- trimws(ln)
    m <- regmatches(ln, regexec("^  (ok|FAIL) +(.*)$", ln))[[1]]
    if (length(m)) php_tests[nrow(php_tests) + 1, ] <- list("PHP", grp, m[3], 1L, if (m[2] == "ok") "PASS" else "FAIL")
  }
} else {
  php_note <- "PHP not found: External Module tests were not run (pass the php path as the first argument or set CDMSTAT_PHP)."
}
tests <- rbind(r_tests, php_tests)

# ---- Traceability ----------------------------------------------------------
req <- utils::read.csv(file.path(here, "requirements.csv"), stringsAsFactors = FALSE)
trace <- do.call(rbind, lapply(seq_len(nrow(req)), function(i) {
  refs <- trimws(strsplit(req$tests[i], ";")[[1]])
  st <- vapply(refs, function(r) {
    if (startsWith(r, "php:")) {
      g <- php_tests[php_tests$group == sub("^php:", "", r), ]
      if (!nrow(g)) return("NOT RUN")
      if (all(g$result == "PASS")) "PASS" else "FAIL"
    } else {
      g <- r_tests[r_tests$test == r, ]
      if (!nrow(g)) return("MISSING")
      if (all(g$result == "PASS")) "PASS" else "FAIL"
    }
  }, "")
  overall <- if (any(st %in% c("FAIL", "MISSING"))) "FAIL" else if (any(st == "NOT RUN")) "INCOMPLETE" else "PASS"
  data.frame(id = req$id[i], area = req$area[i], risk = req$risk[i], requirement = req$requirement[i],
             tests = length(refs), result = overall, stringsAsFactors = FALSE)
}))

# ---- Report ----------------------------------------------------------------
md_table <- function(d) {
  d[] <- lapply(d, function(v) gsub("\\|", "/", as.character(v)))
  c(paste0("| ", paste(names(d), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(d)), collapse = "|"), "|"),
    apply(d, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
overall <- if (all(iq$result == "PASS") && all(tests$result == "PASS") && all(trace$result == "PASS")) "PASS" else
  if (any(iq$result == "FAIL") || any(tests$result == "FAIL") || any(trace$result == "FAIL")) "FAIL" else "INCOMPLETE"
say("# REDCap CDMSTAT validation report")
say("")
say("- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
say("- Host: ", Sys.info()[["nodename"]], " (", R.version$platform, ")")
say("- Run by: ", Sys.info()[["user"]])
say("- Package version: ", desc[, "Version"])
say("- **Overall result: ", overall, "**")
say("")
say("## Summary")
say("")
say("- Installation checks: ", sum(iq$result == "PASS"), " of ", nrow(iq), " passed")
say("- R tests: ", sum(r_tests$result == "PASS"), " of ", nrow(r_tests), " passed (", sum(r_tests$expectations), " expectations)")
say("- External Module tests: ", if (nrow(php_tests)) paste0(sum(php_tests$result == "PASS"), " of ", nrow(php_tests), " passed") else "not run")
say("- Requirements: ", sum(trace$result == "PASS"), " of ", nrow(trace), " fully verified")
if (nzchar(php_note)) { say(""); say("> ", php_note) }
say("")
say("## Requirements traceability")
say("")
lines <- c(lines, md_table(trace))
say("")
say("## Installation qualification")
say("")
lines <- c(lines, md_table(iq))
say("")
say("## Operational qualification: test results")
say("")
lines <- c(lines, md_table(tests))
say("")
say("## Source integrity (SHA-256 of R sources)")
say("")
lines <- c(lines, md_table(data.frame(file = basename(names(hashes)), sha256 = unname(hashes))))
say("")
say("## Environment")
say("")
say("```")
lines <- c(lines, utils::capture.output(utils::sessionInfo()))
say("```")
say("")
say("## Approval")
say("")
say("| Role | Name | Signature | Date |")
say("|---|---|---|---|")
say("| Executed by | | | |")
say("| Reviewed by (QA) | | | |")
say("| Approved by | | | |")

md_file <- file.path(out_dir, paste0("validation-", stamp, ".md"))
writeLines(lines, md_file)
utils::write.csv(tests, file.path(out_dir, paste0("test-results-", stamp, ".csv")), row.names = FALSE)
if (requireNamespace("markdown", quietly = TRUE)) {
  markdown::mark_html(md_file, output = sub("\\.md$", ".html", md_file))
}
cat("Validation", overall, "->", md_file, "\n")
quit(status = if (overall == "FAIL") 1 else 0)
