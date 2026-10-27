#' Publication-ready table of a fitted model
#'
#' Formats a model fitted with `nx$fit()` or [fit_model()] as a 'gtsummary'
#' regression table, with variable labels from the data dictionary, and
#' exponentiated estimates (odds or hazard ratios) for logistic and Cox
#' models.
#'
#' @param fit Result of `nx$fit()` or [fit_model()].
#' @param exponentiate Show exponentiated coefficients. Defaults to `TRUE` for
#'   logistic, mixed logistic and Cox models.
#' @param ... Passed to [gtsummary::tbl_regression()].
#' @return A `gtsummary` table.
#' @examples
#' \donttest{
#' if (requireNamespace("gtsummary", quietly = TRUE) &&
#'     requireNamespace("broom.helpers", quietly = TRUE)) {
#'   nx <- cdmstat_demo(dir = file.path(tempdir(), "tbl-fit"), reset = TRUE)
#'   tbl_fit(nx$fit("sbp__week_24 ~ arm + sbp__baseline"))
#' }
#' }
#' @export
tbl_fit <- function(fit, exponentiate = NULL, ...) {
  for (p in c("gtsummary", "broom.helpers")) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Install '", p, "' for formatted model tables.", call. = FALSE)
  }
  model <- attr(fit, "model")
  if (is.null(model)) stop("`fit` has no model attached; refit it with nx$fit() or fit_model().", call. = FALSE)
  type <- attr(fit, "type") %||% "linear"
  if (type %in% c("mixed", "mixed_logistic") && !requireNamespace("broom.mixed", quietly = TRUE)) {
    stop("Install 'broom.mixed' to tabulate mixed models.", call. = FALSE)
  }
  exponentiate <- exponentiate %||% (type %in% c("logistic", "cox", "mixed_logistic"))
  args <- list(model, exponentiate = exponentiate, ...)
  ev <- attr(fit, "event_labels")
  if (!is.null(ev) && "redcap_event_name" %in% all.vars(stats::formula(model)) && is.null(args$label)) {
    args$label <- list(redcap_event_name = "Visit")
  }
  if (type %in% c("mixed", "mixed_logistic")) args$tidy_fun <- tidy_mixed
  tbl <- do.call(gtsummary::tbl_regression, args)
  if (!is.null(ev)) {
    # Show visit names ("Week 24") instead of REDCap's unique event names.
    tbl <- gtsummary::modify_table_body(tbl, function(b) {
      for (k in names(ev)) b$label <- gsub(k, ev[[k]], b$label, fixed = TRUE)
      b
    })
  }
  if (type %in% c("mixed", "mixed_logistic")) {
    tbl <- gtsummary::modify_footnote_header(tbl, "Fixed effects; p-values from Wald z tests", columns = "p.value")
  }
  gtsummary::bold_labels(gtsummary::bold_p(tbl))
}

# Fixed effects of a mixed model with Wald z p-values (as nx$fit() reports),
# since lme4 does not provide p-values for linear mixed models.
tidy_mixed <- function(x, ...) {
  t <- broom.mixed::tidy(x, effects = "fixed", ...)
  if (!"p.value" %in% names(t)) t$p.value <- 2 * stats::pnorm(-abs(t$statistic))
  t
}

# Map the engine's test names to 'gtsummary' add_p() tests.
gt_test <- c(t.test = "t.test", anova = "oneway.test", wilcox = "wilcox.test", kruskal = "kruskal.test",
             chisq = "chisq.test", fisher = "fisher.test")

# Table 1 as a 'gtsummary' table, using the engine's own test choices.
CDMSTAT$set("public", "table_one_gt", function(by = NULL, vars = NULL, event = NULL, tests = list(), mode = "exploratory") {
  if (!requireNamespace("gtsummary", quietly = TRUE)) stop("Install 'gtsummary' for formatted tables.", call. = FALSE)
  md <- private$md()
  id <- md$field_name[1]
  by <- by %||% detect_arm(md)
  if (identical(by, NA_character_)) by <- NULL
  event <- event %||% self$event_order()[1]
  vars <- vars %||% baseline_vars(md, private$cut$mapping %||% self$mapping, event, id, by %||% NA)
  # The engine decides each summary and test; gtsummary only lays it out.
  t1 <- self$table_one(by = by, vars = vars, event = event, tests = tests, mode = mode)
  rows <- t1[t1$variable %in% vars & nzchar(t1$label), , drop = FALSE]
  d <- self$shape("long", "drop", log = FALSE)
  if ("redcap_event_name" %in% names(d) && !is.null(event)) d <- d[d$redcap_event_name == event, , drop = FALSE]
  if (!is.null(by)) {
    arm <- first_non_na_by(self$shape("long", "drop", log = FALSE)[, c(id, by)], id)
    d[[by]] <- arm[[by]][match(d[[id]], arm[[id]])]
    d <- d[!is.na(d[[by]]), , drop = FALSE]
  }
  d <- d[, c(by, vars), drop = FALSE]
  stat <- list()
  test <- list()
  for (i in seq_len(nrow(rows))) {
    v <- rows$variable[i]
    if (is.numeric(d[[v]])) {
      stat[[v]] <- if (rows$level[i] == "mean (SD)") "{mean} ({sd})" else "{median} ({p25}, {p75})"
    }
    nm <- names(gt_test)[match(rows$test[i], c("Welch t-test", "Welch ANOVA", "Wilcoxon rank-sum",
                                                "Kruskal-Wallis", "Chi-squared", "Fisher's exact"))]
    if (!is.na(nm)) test[[v]] <- gt_test[[nm]]
  }
  labels <- stats::setNames(lapply(vars, function(v) field_label(md, v)), vars)
  self$log_code(sprintf("tbl <- nx$table_one_gt(by = %s, event = %s)", deparse(by), deparse(event)))
  tbl <- if (is.null(by)) {
    gtsummary::tbl_summary(d, label = labels, statistic = stat, missing = "ifany", missing_text = "Missing")
  } else {
    gtsummary::tbl_summary(d, by = tidyselect::all_of(by), label = labels, statistic = stat,
                           missing = "ifany", missing_text = "Missing")
  }
  if (!is.null(by)) {
    tbl <- gtsummary::add_overall(tbl)
    tbl <- gtsummary::add_p(tbl, test = test, pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = 2))
  }
  gtsummary::bold_labels(tbl)
})
