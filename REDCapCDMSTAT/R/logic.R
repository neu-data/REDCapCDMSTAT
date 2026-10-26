# Live logic engine: REDCap branching logic and calculations are translated
# from the current data dictionary every time they are evaluated, so a change
# to the dictionary online is picked up on the next sync with no code edits.

#' Translate a REDCap logic / calculation string into an R expression string.
translate_logic <- function(logic) {
  x <- logic
  store <- character()
  keep <- function(s) {
    store <<- c(store, s)
    sprintf("@@%d@@", length(store))
  }

  # String literals. Quoted numbers become numbers, as REDCap treats them.
  x <- gsub_fn(x, "'[^']*'|\"[^\"]*\"", function(m, g) {
    inner <- substr(m, 2, nchar(m) - 1)
    if (grepl("^\\s*-?[0-9]+(\\.[0-9]+)?\\s*$", inner)) keep(trimws(inner))
    else keep(deparse(inner))
  })

  # Cross-event references: [event][field] or [event][field(code)].
  x <- gsub_fn(x, "\\[([A-Za-z0-9_]+)\\]\\[([A-Za-z0-9_]+)(?:\\(([A-Za-z0-9_-]+)\\))?\\]",
               function(m, g) {
                 fld <- if (nzchar(g[3])) paste0(g[2], "___", gsub("-", "_", g[3])) else g[2]
                 keep(sprintf('.ev("%s", "%s")', g[1], fld))
               })

  # Same-row references: [field] or checkbox [field(code)].
  x <- gsub_fn(x, "\\[([A-Za-z0-9_]+)(?:\\(([A-Za-z0-9_-]+)\\))?\\]", function(m, g) {
    fld <- if (nzchar(g[2])) paste0(g[1], "___", gsub("-", "_", g[2])) else g[1]
    keep(paste0("`", fld, "`"))
  })

  x <- gsub("<>", "!=", x, fixed = TRUE)
  x <- gsub("(?<![<>!=])=(?!=)", "==", x, perl = TRUE)
  x <- gsub("\\band\\b", "&", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\bor\\b", "|", x, ignore.case = TRUE, perl = TRUE)
  for (f in c("if", "datediff", "rounddown", "roundup", "sum", "min", "max", "mean", "median")) {
    x <- gsub(paste0("(?<![.A-Za-z0-9_])", f, "\\s*\\("), paste0(".", f, "("), x, perl = TRUE)
  }

  for (i in rev(seq_along(store))) x <- gsub(sprintf("@@%d@@", i), store[i], x, fixed = TRUE)

  # [field] = '' means "is blank".
  ref <- "(`[^`]+`|\\.ev\\(\"[^\"]+\", \"[^\"]+\"\\))"
  x <- gsub(paste0(ref, "\\s*==\\s*\"\""), ".blank(\\1)", x, perl = TRUE)
  x <- gsub(paste0(ref, "\\s*!=\\s*\"\""), "!.blank(\\1)", x, perl = TRUE)
  x
}

numify <- function(v) {
  if (is.character(v)) {
    nv <- suppressWarnings(as.numeric(v))
    if (all(is_blank(v) | !is.na(nv))) return(nv)
  }
  v
}

logic_env <- function(data, full, id_field) {
  env <- new.env(parent = baseenv())
  rowfun <- function(f) function(...) {
    m <- cbind(...)
    if (!nrow(m)) return(numeric())
    apply(m, 1, function(v) if (all(is.na(v))) NA_real_ else f(v[!is.na(v)]))
  }
  todate <- function(d) {
    if (inherits(d, "Date")) return(d)
    if (is.character(d) && length(d) == 1 && d %in% c("today", "now")) return(Sys.Date())
    as.Date(substr(as.character(d), 1, 10))
  }
  env$true <- TRUE
  env$false <- FALSE
  env$.if <- function(cond, yes, no) ifelse(cond, yes, no)
  env$.sum <- rowfun(sum)
  env$.min <- rowfun(min)
  env$.max <- rowfun(max)
  env$.mean <- rowfun(mean)
  env$.median <- rowfun(stats::median)
  env$.rounddown <- function(x, d = 0) floor(x * 10^d) / 10^d
  env$.roundup <- function(x, d = 0) ceiling(x * 10^d) / 10^d
  env$.blank <- is_blank
  env$.datediff <- function(d1, d2, unit = "d", fmt = NULL, signed = FALSE) {
    if (is.logical(fmt)) signed <- fmt
    days <- as.numeric(todate(d2) - todate(d1))
    div <- switch(unit, y = 365.2425, M = 30.44, d = 1, h = 1 / 24, m = 1 / 1440, s = 1 / 86400, 1)
    r <- days / div
    if (!isTRUE(signed)) r <- abs(r)
    r
  }
  env$.ev <- function(ev, fld) {
    n <- nrow(data)
    if (is.null(full) || !fld %in% names(full)) return(rep(NA, n))
    src <- full
    if ("redcap_event_name" %in% names(src)) {
      keep <- src$redcap_event_name == ev
      if ("redcap_repeat_instrument" %in% names(src)) keep <- keep & is_blank(src$redcap_repeat_instrument)
      src <- src[which(keep), , drop = FALSE]
    }
    numify(src[[fld]][match(data[[id_field]], src[[id_field]])])
  }
  env
}

#' Evaluate REDCap logic row-wise on a data frame.
#'
#' @param data rows to evaluate (typed data).
#' @param full the full data set, used for cross-event references.
#' @return a vector with one value per row (NA where it cannot be evaluated).
eval_logic <- function(logic, data, full = NULL, id_field = names(data)[1]) {
  n <- nrow(data)
  if (is.null(logic) || is.na(logic) || !nzchar(trimws(logic))) return(rep(TRUE, n))
  code <- translate_logic(logic)
  refs <- unique(gsub("`", "", regmatches(code, gregexpr("`[^`]+`", code))[[1]]))
  dl <- list()
  for (r in refs) dl[[r]] <- if (r %in% names(data)) numify(data[[r]]) else rep(NA, n)
  env <- logic_env(data, full %||% data, id_field)
  res <- tryCatch(
    eval(parse(text = code), envir = dl, enclos = env),
    error = function(e) structure(rep(NA, n), logic_error = conditionMessage(e))
  )
  if (length(res) == 1 && n != 1) res <- rep(res, n)
  res
}
