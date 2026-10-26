#' Find the randomisation / treatment-arm field from the data dictionary
#'
#' @param md Data dictionary (REDCap metadata export) as a data frame.
#' @return Field name, or `NA` if none is found.
#' @examples
#' detect_arm(demo_project(n = 5)$metadata)
#' @export
detect_arm <- function(md) {
  cand <- md[md$field_type %in% c("radio", "dropdown"), , drop = FALSE]
  hit <- cand$field_name[
    grepl("^(arm|group|treatment|trt|rand.*|allocation|study_arm)$", cand$field_name, ignore.case = TRUE) |
      grepl("randomi[sz]|treatment arm|allocation|study arm", cand$field_label, ignore.case = TRUE)]
  if (length(hit)) hit[1] else NA_character_
}

#' Candidate baseline variables: analysable fields on forms collected at the
#'
#' first event, excluding identifiers, dates, free text and the arm itself.
#'
#' @param md Data dictionary (REDCap metadata export) as a data frame.
#' @param mapping Form-event mapping (columns `unique_event_name`, `form`); `NULL` for classic projects.
#' @param first_event Unique name of the first (baseline) event.
#' @param id_field Name of the record ID field; defaults to the first dictionary field.
#' @param arm Name of the treatment-arm field, excluded from the baseline variables.
#' @return Character vector of field names.
#' @export
baseline_vars <- function(md, mapping = NULL, first_event = NULL, id_field = md$field_name[1], arm = detect_arm(md)) {
  forms <- if (!is.null(mapping) && nrow(mapping) && !is.null(first_event)) {
    mapping$form[mapping$unique_event_name == first_event]
  } else unique(md$form_name)
  kinds <- field_kinds(md)
  keep <- md$form_name %in% forms & kinds[md$field_name] %in% c("numeric", "categorical") &
    md$field_name != id_field & !md$identifier %in% "y" & !startsWith(md$field_name, "cdmstat_") &
    !md$field_name %in% arm
  md$field_name[keep]
}

check_normal <- function(groups, alpha = 0.05) {
  reasons <- character()
  normal <- TRUE
  for (nm in names(groups)) {
    g <- groups[[nm]]
    g <- g[!is.na(g)]
    if (length(g) < 3) { normal <- FALSE; reasons <- c(reasons, sprintf("%s: n<3", nm)); next }
    if (length(g) > 5000) { reasons <- c(reasons, sprintf("%s: n>5000, CLT", nm)); next }
    if (stats::sd(g) == 0) { normal <- FALSE; reasons <- c(reasons, sprintf("%s: no variance", nm)); next }
    p <- stats::shapiro.test(g)$p.value
    if (p <= alpha) normal <- FALSE
    reasons <- c(reasons, sprintf("%s: Shapiro-Wilk p=%s", nm, format.pval(p, digits = 2, eps = 0.001)))
  }
  list(normal = normal, reason = paste(reasons, collapse = "; "))
}

fmt_p <- function(p) if (is.na(p)) "" else format.pval(p, digits = 2, eps = 0.001)

#' Baseline characteristics table with guarded test selection
#'
#' Baseline characteristics table with guarded test selection.
#'
#' In "exploratory" mode continuous variables are tested for normality within
#' each group and summarised/tested accordingly (mean (SD) + Welch t-test or
#' ANOVA, otherwise median [IQR] + Wilcoxon or Kruskal-Wallis); categorical
#' variables use chi-squared unless an expected cell count is below 5, then
#' Fisher's exact test. In "sap" mode the tests in `tests` are pre-specified
#' and enforced instead, as a confirmatory analysis requires.
#'
#' @param data Typed data frame (see [type_data()]) or a shaped analysis data frame.
#' @param vars Variables to summarise.
#' @param by Grouping column, usually the randomised arm.
#' @param md Data dictionary (REDCap metadata export) as a data frame.
#' @param tests Named list of pre-specified tests (`"t.test"`, `"anova"`, `"wilcox"`, `"kruskal"`, `"chisq"`, `"fisher"`).
#' @param mode `"exploratory"` chooses tests from the data; `"sap"` requires every test to be pre-specified in `tests`.
#' @param alpha Significance level of the Shapiro-Wilk normality check.
#' @return Data frame with one row per variable or level, a column per group, `Overall`, `p_value`, `test` and `rationale`.
#' @export
table_one <- function(data, vars, by = NULL, md = NULL, tests = list(), mode = c("exploratory", "sap"),
                      alpha = 0.05) {
  mode <- match.arg(mode)
  lab <- function(v) if (is.null(md)) v else field_label(md, v)
  grp <- if (!is.null(by) && by %in% names(data)) droplevels(as.factor(data[[by]])) else NULL
  if (!is.null(grp)) {
    keep <- !is.na(grp)
    data <- data[keep, , drop = FALSE]
    grp <- grp[keep]
  }
  lv <- if (is.null(grp)) character() else levels(grp)
  can_test <- length(lv) >= 2
  rows <- list()
  add <- function(variable, label, level, cells, overall, p = NA, test = "", rationale = "") {
    r <- c(list(variable = variable, label = label, level = level), as.list(stats::setNames(cells, lv)),
           list(Overall = overall, p_value = fmt_p(p), test = test, rationale = rationale))
    rows[[length(rows) + 1]] <<- as.data.frame(r, stringsAsFactors = FALSE, check.names = FALSE)
  }
  n_cells <- if (length(lv)) as.character(as.integer(table(grp))) else character()
  add("N", "N", "", n_cells, as.character(nrow(data)))

  for (v in vars) {
    if (!v %in% names(data)) next
    x <- data[[v]]
    if (is.numeric(x)) {
      xs <- if (length(lv)) split(x, grp) else list(Overall = x)
      pre <- tests[[v]]
      norm <- if (is.null(pre)) check_normal(xs, alpha) else list(normal = pre %in% c("t.test", "anova"), reason = "")
      if (mode == "sap" && is.null(pre)) {
        stop("SAP mode: no pre-specified test for '", v, "'. Add it to `tests`.", call. = FALSE)
      }
      param <- isTRUE(norm$normal)
      summ <- function(z) {
        z <- z[!is.na(z)]
        if (!length(z)) return("-")
        if (param) sprintf("%.1f (%.1f)", mean(z), stats::sd(z))
        else sprintf("%.1f [%.1f, %.1f]", stats::median(z), stats::quantile(z, .25), stats::quantile(z, .75))
      }
      test <- pre %||% if (param) (if (length(lv) > 2) "anova" else "t.test") else (if (length(lv) > 2) "kruskal" else "wilcox")
      p <- if (can_test) tryCatch(switch(test,
        t.test = stats::t.test(x ~ grp)$p.value,
        anova = stats::oneway.test(x ~ grp)$p.value,
        wilcox = stats::wilcox.test(x ~ grp, exact = FALSE)$p.value,
        kruskal = stats::kruskal.test(x ~ grp)$p.value,
        NA), error = function(e) NA) else NA
      test_name <- c(t.test = "Welch t-test", anova = "Welch ANOVA", wilcox = "Wilcoxon rank-sum",
                     kruskal = "Kruskal-Wallis")[[test]]
      rationale <- if (!is.null(pre)) "Pre-specified (SAP)" else paste0(norm$reason, if (param) " -> normal" else " -> non-normal")
      add(v, lab(v), if (param) "mean (SD)" else "median [IQR]", vapply(xs[lv], summ, ""), summ(x),
          p, if (can_test) test_name else "", rationale)
      if (any(is.na(x))) {
        add(v, "", "Missing", if (length(lv)) vapply(xs[lv], function(z) as.character(sum(is.na(z))), "") else character(),
            as.character(sum(is.na(x))))
      }
    } else {
      f <- if (is.factor(x)) droplevels(x) else factor(x)
      tab <- if (length(lv)) table(f, grp) else NULL
      pre <- tests[[v]]
      if (mode == "sap" && is.null(pre)) {
        stop("SAP mode: no pre-specified test for '", v, "'. Add it to `tests`.", call. = FALSE)
      }
      test <- pre
      rationale <- "Pre-specified (SAP)"
      if (is.null(test)) {
        ok <- can_test && nrow(tab) >= 2
        expct <- if (ok) suppressWarnings(stats::chisq.test(tab)$expected) else NULL
        test <- if (!is.null(expct) && any(expct < 5)) "fisher" else "chisq"
        rationale <- if (is.null(expct)) "" else sprintf("min expected count %.1f -> %s", min(expct),
                                                         if (test == "fisher") "Fisher" else "chi-squared")
      }
      p <- if (can_test && nrow(tab) >= 2) tryCatch(
        if (test == "fisher") {
          stats::fisher.test(tab, simulate.p.value = any(dim(tab) > 2), B = 1e4)$p.value
        } else suppressWarnings(stats::chisq.test(tab)$p.value),
        error = function(e) NA) else NA
      add(v, lab(v), "", rep("", length(lv)), "", p,
          if (can_test) (if (test == "fisher") "Fisher's exact" else "Chi-squared") else "", rationale)
      pct <- function(k, n) if (n == 0) "-" else sprintf("%d (%.1f%%)", k, 100 * k / n)
      for (l in levels(f)) {
        cells <- if (length(lv)) vapply(lv, function(g) pct(sum(f[grp == g] == l, na.rm = TRUE), sum(!is.na(f[grp == g]))), "") else character()
        add(v, "", l, cells, pct(sum(f == l, na.rm = TRUE), sum(!is.na(f))))
      }
      if (any(is.na(f))) {
        add(v, "", "Missing", if (length(lv)) vapply(lv, function(g) as.character(sum(is.na(f[grp == g]))), "") else character(),
            as.character(sum(is.na(f))))
      }
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "by") <- by
  attr(out, "mode") <- mode
  out
}

#' Flag statistical outliers (|z| above a threshold, or Tukey IQR fences),
#'
#' optionally within groups such as visit events.
#'
#' @param data Typed data frame (see [type_data()]) or a shaped analysis data frame.
#' @param field Name of the field to analyse.
#' @param id_field Name of the record ID field; defaults to the first dictionary field.
#' @param method Outlier rule: `"z"` (robust z-score from median and MAD) or `"iqr"` (Tukey fences).
#' @param threshold Flagging threshold; defaults to 3 for `"z"` and 1.5 for `"iqr"`.
#' @param group Column within which scores are computed, by default the event.
#' @return Data frame with `record_id`, `event`, `field`, `value`, `score` and `outlier`.
#' @export
detect_outliers <- function(data, field, id_field = names(data)[1], method = c("z", "iqr"), threshold = NULL,
                            group = if ("redcap_event_name" %in% names(data)) "redcap_event_name" else NULL) {
  method <- match.arg(method)
  threshold <- threshold %||% if (method == "z") 3 else 1.5
  x <- suppressWarnings(as.numeric(data[[field]]))
  g <- if (!is.null(group) && group %in% names(data)) as.character(data[[group]]) else rep("all", nrow(data))
  score <- rep(NA_real_, length(x))
  for (lv in unique(g)) {
    i <- which(g == lv & !is.na(x))
    if (length(i) < 4) next
    if (method == "z") {
      # Robust z-score (median / MAD) so the outlier cannot hide itself by
      # inflating the standard deviation.
      s <- stats::mad(x[i])
      if (s == 0) s <- stats::sd(x[i])
      if (is.na(s) || s == 0) next
      score[i] <- (x[i] - stats::median(x[i])) / s
    } else {
      q <- stats::quantile(x[i], c(.25, .75))
      iqr <- q[2] - q[1]
      if (iqr == 0) next
      score[i] <- ifelse(x[i] < q[1], (x[i] - q[1]) / iqr, ifelse(x[i] > q[2], (x[i] - q[2]) / iqr, 0))
    }
  }
  flag <- !is.na(score) & abs(score) > threshold
  data.frame(record_id = as.character(data[[id_field]]),
             event = if ("redcap_event_name" %in% names(data)) as.character(data$redcap_event_name) else NA_character_,
             field = field, value = x, score = round(score, 2), outlier = flag, stringsAsFactors = FALSE)
}

#' Mean, SD, SE and 95% CI of a field per event and group
#'
#' @param data Typed data frame (see [type_data()]) or a shaped analysis data frame.
#' @param field Name of the field to analyse.
#' @param by Grouping column, usually the randomised arm.
#' @param event_order Event names in schedule order.
#' @return Data frame with `event`, `group`, `n`, `mean`, `sd`, `se`, `lower` and `upper`.
#' @export
trend_summary <- function(data, field, by = NULL, event_order = NULL) {
  x <- suppressWarnings(as.numeric(data[[field]]))
  ev <- as.character(data$redcap_event_name %||% rep("all", nrow(data)))
  g <- if (!is.null(by) && by %in% names(data)) as.character(data[[by]]) else rep("All", nrow(data))
  ok <- !is.na(x) & !is.na(g)
  key <- unique(data.frame(event = ev[ok], group = g[ok], stringsAsFactors = FALSE))
  res <- do.call(rbind, lapply(seq_len(nrow(key)), function(k) {
    z <- x[ok][ev[ok] == key$event[k] & g[ok] == key$group[k]]
    se <- if (length(z) > 1) stats::sd(z) / sqrt(length(z)) else NA
    data.frame(event = key$event[k], group = key$group[k], n = length(z), mean = mean(z),
               sd = if (length(z) > 1) stats::sd(z) else NA, se = se,
               lower = mean(z) - 1.96 * se, upper = mean(z) + 1.96 * se, stringsAsFactors = FALSE)
  }))
  if (is.null(res)) return(data.frame())
  lvls <- event_order %||% unique(ev)
  res$event <- factor(res$event, levels = intersect(lvls, unique(res$event)))
  res[order(res$group, res$event), , drop = FALSE]
}

#' Fit a model and return a tidy coefficient table
#'
#' Types: "linear" (lm), "logistic" (glm), "cox" (survival::coxph), and for
#' repeated measures on long data "mixed" (lme4::lmer) and
#' "mixed_logistic" (lme4::glmer), e.g.
#' `sbp ~ arm * redcap_event_name + (1 | record_id)`. Mixed-model p-values
#' use the Wald normal approximation.
#'
#' @param formula Model formula as a string.
#' @param data Typed data frame (see [type_data()]) or a shaped analysis data frame.
#' @param type Model type: `"linear"`, `"logistic"`, `"cox"`, `"mixed"` or `"mixed_logistic"`.
#' @return Coefficient table with estimates, standard errors, 95% CIs, p-values and (for logistic and Cox models) odds or hazard ratios. Attributes hold `n`, `type` and, for mixed models, `groups`, `random_effects` and `p_method`.
#' @export
fit_model <- function(formula, data, type = c("linear", "logistic", "cox", "mixed", "mixed_logistic")) {
  type <- match.arg(type)
  f <- stats::as.formula(formula)
  if (type %in% c("mixed", "mixed_logistic")) {
    if (!requireNamespace("lme4", quietly = TRUE)) stop("Install 'lme4' for mixed models.", call. = FALSE)
    if (!any(grepl("|", deparse(f), fixed = TRUE))) stop("A mixed model needs a random effect, e.g. + (1 | record_id).", call. = FALSE)
    m <- if (type == "mixed") lme4::lmer(f, data = data) else lme4::glmer(f, data = data, family = stats::binomial())
    s <- summary(m)$coefficients
    est <- s[, 1]
    se <- s[, 2]
    p <- 2 * stats::pnorm(-abs(est / se))
    out <- data.frame(term = rownames(s), estimate = est, std_error = se, conf_low = est - 1.96 * se,
                      conf_high = est + 1.96 * se, p_value = p, stringsAsFactors = FALSE, row.names = NULL)
    if (type == "mixed_logistic") {
      out$odds_ratio <- exp(out$estimate)
      out$ratio_low <- exp(out$conf_low)
      out$ratio_high <- exp(out$conf_high)
    }
    vc <- as.data.frame(lme4::VarCorr(m))
    attr(out, "n") <- stats::nobs(m)
    attr(out, "groups") <- lme4::ngrps(m)
    attr(out, "random_effects") <- vc[, intersect(c("grp", "var1", "var2", "vcov", "sdcor"), names(vc))]
    attr(out, "type") <- type
    attr(out, "p_method") <- "Wald z"
    return(out)
  }
  m <- switch(type,
    linear = stats::lm(f, data = data),
    logistic = stats::glm(f, data = data, family = stats::binomial()),
    cox = {
      if (!requireNamespace("survival", quietly = TRUE)) stop("Install 'survival' for Cox models.")
      env <- list2env(list(Surv = survival::Surv), parent = environment(f))
      environment(f) <- env
      survival::coxph(f, data = data)
    })
  s <- summary(m)$coefficients
  est <- s[, 1]
  se <- if (type == "cox") s[, 3] else s[, 2]
  p <- s[, ncol(s)]
  out <- data.frame(term = rownames(s), estimate = est, std_error = se,
                    conf_low = est - 1.96 * se, conf_high = est + 1.96 * se, p_value = p,
                    stringsAsFactors = FALSE, row.names = NULL)
  if (type %in% c("logistic", "cox")) {
    out$ratio <- exp(out$estimate)
    out$ratio_low <- exp(out$conf_low)
    out$ratio_high <- exp(out$conf_high)
    names(out)[names(out) == "ratio"] <- if (type == "cox") "hazard_ratio" else "odds_ratio"
  }
  attr(out, "n") <- stats::nobs(m)
  attr(out, "type") <- type
  out
}
