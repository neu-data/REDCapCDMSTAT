#' Convert raw REDCap exports to analysis types
#'
#' Convert raw (all-character) REDCap export data to analysis types using the
#' data dictionary: numbers, dates, integer yes/no and checkbox columns.
#' Radio and dropdown fields keep their raw codes.
#'
#' @param raw Raw (all-character) REDCap export.
#' @param md Data dictionary (REDCap metadata export) as a data frame.
#' @return Data frame with typed columns.
#' @export
type_data <- function(raw, md) {
  out <- raw
  out[] <- lapply(out, function(v) {
    if (is.character(v)) v[!is.na(v) & !nzchar(trimws(v))] <- NA
    v
  })
  vt <- md$text_validation_type_or_show_slider_number
  for (i in seq_len(nrow(md))) {
    f <- md$field_name[i]
    ft <- md$field_type[i]
    if (ft == "checkbox") {
      for (cc in grep(paste0("^", f, "___"), names(out), value = TRUE)) {
        out[[cc]] <- suppressWarnings(as.integer(out[[cc]]))
      }
      next
    }
    if (!f %in% names(out)) next
    v <- out[[f]]
    if (ft %in% c("calc", "slider") || (ft == "text" && is_numeric_validation(vt[i]))) {
      out[[f]] <- suppressWarnings(as.numeric(v))
    } else if (ft == "text" && is_date_validation(vt[i])) {
      out[[f]] <- as.Date(substr(v, 1, 10))
    } else if (ft %in% c("yesno", "truefalse")) {
      out[[f]] <- suppressWarnings(as.integer(v))
    }
  }
  if ("redcap_repeat_instance" %in% names(out)) {
    out$redcap_repeat_instance <- suppressWarnings(as.integer(out$redcap_repeat_instance))
  }
  out
}

# Turn coded categorical columns into labelled factors.
label_column <- function(x, mdrow) {
  ft <- mdrow$field_type
  if (ft %in% c("radio", "dropdown")) {
    ch <- parse_choices(mdrow$select_choices_or_calculations)
    return(factor(as.character(x), levels = ch$code, labels = ch$label))
  }
  if (ft == "yesno") return(factor(as.character(x), levels = c("0", "1"), labels = c("No", "Yes")))
  if (ft == "truefalse") return(factor(as.character(x), levels = c("0", "1"), labels = c("False", "True")))
  x
}

# Expected export column names for a dictionary (checkboxes expanded).
field_columns <- function(md, fields = md$field_name) {
  out <- character()
  for (f in fields) {
    i <- match(f, md$field_name)
    if (is.na(i)) next
    if (md$field_type[i] == "checkbox") {
      ch <- parse_choices(md$select_choices_or_calculations[i])
      out <- c(out, paste0(f, "___", gsub("-", "_", ch$code)))
    } else if (md$field_type[i] != "descriptive") {
      out <- c(out, f)
    }
  }
  out
}
