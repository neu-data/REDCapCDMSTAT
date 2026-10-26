issue_key <- function(rule, record, event, instance, field) {
  vapply(paste(rule, record, event, instance, field, sep = "|"),
         function(s) substr(digest::digest(s, algo = "md5", serialize = FALSE), 1, 16), "",
         USE.NAMES = FALSE)
}

empty_issues <- function() {
  data.frame(rule = character(), severity = character(), record_id = character(), event = character(),
             instance = character(), field = character(), value = character(), message = character(),
             issue_key = character(), stringsAsFactors = FALSE)
}

#' Run data-quality rules
#'
#' Run data-quality rules derived from the live data dictionary plus custom rules.
#'
#' Dictionary-driven checks: validation ranges, required fields, values present
#' in fields hidden by branching logic, and stored calculated fields that no
#' longer match a recalculation. Custom rules are REDCap-syntax conditions that
#' must be TRUE; rows where they evaluate FALSE are reported.
#'
#' @param data Typed data frame (see [type_data()]) or a shaped analysis data frame.
#' @param md Data dictionary (REDCap metadata export) as a data frame.
#' @param id_field Name of the record ID field; defaults to the first dictionary field.
#' @param repeating_forms Names of repeating instruments.
#' @param rules List of custom rules, each with `id`, `logic` (REDCap syntax, must be TRUE), `message`, `severity` and optionally `field`.
#' @return Data frame of issues: `rule`, `severity`, `record_id`, `event`, `instance`, `field`, `value`, `message`, `issue_key`.
#' @export
run_validation <- function(data, md, id_field = md$field_name[1], repeating_forms = character(),
                           rules = list()) {
  out <- list()
  rep_col <- if ("redcap_repeat_instrument" %in% names(data)) data$redcap_repeat_instrument else rep(NA, nrow(data))
  is_rep <- !is_blank(rep_col)
  ev_col <- if ("redcap_event_name" %in% names(data)) "redcap_event_name" else NULL

  emit <- function(rule, severity, sub, idx, field, values, msg) {
    if (!length(idx)) return()
    out[[length(out) + 1]] <<- data.frame(
      rule = rule, severity = severity,
      record_id = as.character(sub[[id_field]][idx]),
      event = if (is.null(ev_col)) NA_character_ else as.character(sub[[ev_col]][idx]),
      instance = if ("redcap_repeat_instance" %in% names(sub)) as.character(sub$redcap_repeat_instance[idx]) else NA_character_,
      field = field, value = as.character(values[idx]), message = if (length(msg) > 1) msg[idx] else msg,
      stringsAsFactors = FALSE)
  }

  chk <- md[!md$field_type %in% "descriptive" & md$field_name != id_field &
              !startsWith(md$field_name, "cdmstat_"), , drop = FALSE]
  for (form in unique(chk$form_name)) {
    rows <- if (form %in% repeating_forms) which(rep_col == form) else which(!is_rep)
    sub <- data[rows, , drop = FALSE]
    fmd <- chk[chk$form_name == form, , drop = FALSE]
    comp <- paste0(form, "_complete")
    cols <- intersect(field_columns(fmd), names(sub))
    has_form <- if (comp %in% names(sub)) !is_blank(sub[[comp]]) else rep(FALSE, nrow(sub))
    for (cc in cols) has_form <- has_form | !is_blank(sub[[cc]])
    sub <- sub[has_form, , drop = FALSE]
    if (!nrow(sub)) next

    for (i in seq_len(nrow(fmd))) {
      f <- fmd$field_name[i]
      ft <- fmd$field_type[i]
      vt <- fmd$text_validation_type_or_show_slider_number[i]
      if (ft == "checkbox") {
        cb <- intersect(field_columns(fmd[i, ]), names(sub))
        vals <- if (length(cb)) ifelse(rowSums(as.matrix(sub[cb]) == 1, na.rm = TRUE) > 0, "checked", NA) else rep(NA, nrow(sub))
      } else {
        vals <- if (f %in% names(sub)) sub[[f]] else rep(NA, nrow(sub))
      }
      present <- !is_blank(vals)

      bl <- fmd$branching_logic[i]
      shown <- rep(TRUE, nrow(sub))
      if (!is_blank(bl)) {
        shown <- as.logical(eval_logic(bl, sub, data, id_field))
        shown[is.na(shown)] <- FALSE
        emit(paste0("branching:", f), "warning", sub, which(present & !shown), f, vals,
             sprintf("Value recorded but the field is hidden by branching logic (%s)", bl))
      }
      if (identical(fmd$required_field[i], "y")) {
        emit(paste0("required:", f), "error", sub, which(shown & !present), f, vals,
             "Required field is missing")
      }
      if (ft == "text" && (is_numeric_validation(vt) || is_date_validation(vt))) {
        lims <- c(fmd$text_validation_min[i], fmd$text_validation_max[i])
        conv <- if (is_date_validation(vt)) {
          function(z) if (is_blank(z)) NA else if (z %in% c("today", "now")) Sys.Date() else as.Date(substr(z, 1, 10))
        } else {
          function(z) if (is_blank(z)) NA else suppressWarnings(as.numeric(z))
        }
        lo <- conv(lims[1])
        hi <- conv(lims[2])
        bad <- present & ((!is.na(lo) & vals < lo) | (!is.na(hi) & vals > hi))
        bad[is.na(bad)] <- FALSE
        emit(paste0("range:", f), "error", sub, which(bad), f, vals,
             sprintf("Value outside allowed range [%s, %s]", lims[1], lims[2]))
      }
      if (ft == "calc") {
        calc <- suppressWarnings(as.numeric(eval_logic(fmd$select_choices_or_calculations[i], sub, data, id_field)))
        stored <- suppressWarnings(as.numeric(vals))
        bad <- !is.na(calc) & (is.na(stored) | abs(calc - stored) > 1e-6 * pmax(1, abs(calc)))
        emit(paste0("calc:", f), "warning", sub, which(bad), f, vals,
             sprintf("Stored value differs from recalculated value %s", format(round(calc, 4))))
      }
    }
  }

  base <- data[!is_rep, , drop = FALSE]
  for (r in rules) {
    res <- eval_logic(r$logic, base, data, id_field)
    refs <- unique(gsub("`", "", regmatches(translate_logic(r$logic), gregexpr("`[^`]+`", translate_logic(r$logic)))[[1]]))
    fld <- r$field %||% refs[1] %||% NA_character_
    vals <- if (!is.na(fld) && fld %in% names(base)) base[[fld]] else rep(NA, nrow(base))
    emit(paste0("custom:", r$id), r$severity %||% "warning", base, which(!is.na(res) & !as.logical(res)),
         fld, vals, r$message %||% paste("Rule failed:", r$logic))
  }

  if (!length(out)) return(empty_issues())
  res <- do.call(rbind, out)
  res$issue_key <- issue_key(res$rule, res$record_id, res$event, res$instance, res$field)
  res[order_ids(res$record_id), , drop = FALSE]
}
