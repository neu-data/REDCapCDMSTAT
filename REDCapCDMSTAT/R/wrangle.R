first_non_na_by <- function(df, id_field) {
  ids <- unique(df[[id_field]])
  out <- data.frame(stats::setNames(list(ids), id_field), stringsAsFactors = FALSE)
  idx <- match(df[[id_field]], ids)
  for (cc in setdiff(names(df), id_field)) {
    v <- df[[cc]]
    ok <- !is_blank(v)
    pick <- rep(NA_integer_, length(ids))
    w <- which(ok)
    pick[rev(idx[w])] <- rev(w)
    out[[cc]] <- v[pick]
  }
  out
}

collapse_repeating <- function(rep, md, repeating_forms, id_field) {
  ids <- unique(rep[[id_field]])
  out <- data.frame(stats::setNames(list(ids), id_field), stringsAsFactors = FALSE)
  for (form in repeating_forms) {
    sub <- rep[rep$redcap_repeat_instrument == form, , drop = FALSE]
    out[[paste0("n_", form)]] <- as.integer(table(factor(sub[[id_field]], levels = ids)))
    fmd <- md[md$form_name == form, , drop = FALSE]
    for (i in seq_len(nrow(fmd))) {
      f <- fmd$field_name[i]
      if (!f %in% names(sub)) next
      kind <- field_kinds(fmd[i, ])
      is_ordinal <- fmd$field_type[i] %in% c("radio", "dropdown") &&
        all(!is.na(suppressWarnings(as.numeric(parse_choices(fmd$select_choices_or_calculations[i])$code))))
      if (fmd$field_type[i] %in% c("yesno", "truefalse")) {
        nm <- paste0("any_", f)
        v <- tapply(suppressWarnings(as.numeric(sub[[f]])), factor(sub[[id_field]], levels = ids),
                    function(z) if (all(is.na(z))) NA else as.integer(max(z, na.rm = TRUE)))
        out[[nm]] <- as.integer(v)
      } else if (kind == "numeric" || is_ordinal) {
        nm <- paste0("max_", f)
        v <- tapply(suppressWarnings(as.numeric(sub[[f]])), factor(sub[[id_field]], levels = ids),
                    function(z) if (all(is.na(z))) NA else max(z, na.rm = TRUE))
        out[[nm]] <- as.numeric(v)
      }
    }
  }
  out
}

#' Reshape typed REDCap data for analysis.
#'
#' @param longitudinal "long" keeps one row per record and event; "wide" casts
#'   event-varying fields to `field__event` columns, one row per record.
#' @param repeating "drop" removes repeating-instrument rows, "collapse" adds
#'   per-record summaries (counts, any_ for yes/no, max_ for numeric/ordinal),
#'   "only" returns the repeating rows themselves.
#' @return a data frame with a "variables" attribute describing each column.
shape_data <- function(data, md, mapping = NULL, repeating_forms = character(), id_field = md$field_name[1],
                       longitudinal = c("long", "wide"), repeating = c("drop", "collapse", "only"),
                       labels = TRUE) {
  longitudinal <- match.arg(longitudinal)
  repeating <- match.arg(repeating)
  rep_col <- if ("redcap_repeat_instrument" %in% names(data)) data$redcap_repeat_instrument else rep(NA, nrow(data))
  is_rep <- !is_blank(rep_col)
  rep_fields <- field_columns(md, md$field_name[md$form_name %in% repeating_forms])
  admin <- c(grep("_complete$", names(data), value = TRUE), "redcap_repeat_instrument", "redcap_repeat_instance",
             "redcap_data_access_group", grep("^cdmstat_", names(data), value = TRUE))
  colmap <- list()

  if (repeating == "only") {
    keep <- intersect(c(id_field, "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance", rep_fields),
                      names(data))
    out <- data[is_rep, keep, drop = FALSE]
  } else {
    base <- data[!is_rep, setdiff(names(data), c(admin, rep_fields)), drop = FALSE]
    has_events <- "redcap_event_name" %in% names(base)
    if (longitudinal == "wide" && has_events) {
      field_of <- function(cols) {
        vapply(cols, function(cc) { f <- sub("___.*$", "", cc); if (f %in% md$field_name) f else cc }, "")
      }
      vars <- setdiff(names(base), c(id_field, "redcap_event_name"))
      form_n <- if (!is.null(mapping) && nrow(mapping)) table(mapping$form) else NULL
      form_of <- md$form_name[match(field_of(vars), md$field_name)]
      varying <- vars[!is.na(form_of) & !is.null(form_n) & as.integer(form_n[form_of]) > 1]
      varying <- varying[!is.na(varying)]
      const <- setdiff(vars, varying)
      out <- first_non_na_by(base[, c(id_field, const), drop = FALSE], id_field)
      for (cc in const) colmap[[cc]] <- list(field = field_of(cc), event = NA_character_)
      evs <- unique(base$redcap_event_name)
      for (ev in evs) {
        sub <- base[base$redcap_event_name == ev, , drop = FALSE]
        cols <- varying[vapply(varying, function(cc) any(!is_blank(sub[[cc]])), logical(1))]
        if (!length(cols)) next
        piece <- sub[, c(id_field, cols), drop = FALSE]
        short <- sub("_arm_[0-9]+$", "", ev)
        names(piece)[-1] <- paste0(cols, "__", short)
        for (k in seq_along(cols)) colmap[[paste0(cols[k], "__", short)]] <- list(field = field_of(cols[k]), event = ev)
        out <- merge(out, piece, by = id_field, all.x = TRUE, sort = FALSE)
      }
    } else {
      out <- base
    }
    if (repeating == "collapse" && any(is_rep)) {
      agg <- collapse_repeating(data[is_rep, , drop = FALSE], md, repeating_forms, id_field)
      out <- merge(out, agg, by = id_field, all.x = TRUE, sort = FALSE)
      # Records without any repeating instance had none of the events.
      for (cc in grep("^(n|any)_", names(agg), value = TRUE)) out[[cc]][is.na(out[[cc]])] <- 0L
    }
  }

  out <- out[order_ids(out[[id_field]]), , drop = FALSE]
  rownames(out) <- NULL
  base_field <- function(cc) colmap[[cc]]$field %||% sub("___.*$", "", cc)
  if (labels) {
    for (cc in names(out)) {
      f <- base_field(cc)
      i <- match(f, md$field_name)
      if (!is.na(i) && md$field_type[i] %in% c("radio", "dropdown", "yesno", "truefalse")) {
        out[[cc]] <- label_column(out[[cc]], md[i, ])
      }
    }
  }
  kinds <- field_kinds(md)
  attr(out, "variables") <- data.frame(
    name = names(out),
    field = vapply(names(out), base_field, ""),
    event = vapply(names(out), function(cc) colmap[[cc]]$event %||% NA_character_, ""),
    label = vapply(names(out), function(cc) {
      f <- base_field(cc)
      if (f %in% md$field_name) field_label(md, f) else gsub("_", " ", cc)
    }, ""),
    type = vapply(names(out), function(cc) {
      if (cc %in% c(id_field, "redcap_event_name", "redcap_repeat_instrument")) return("id")
      v <- out[[cc]]
      if (is.factor(v)) "categorical" else if (inherits(v, "Date")) "date" else if (is.numeric(v)) {
        if (isTRUE(unname(kinds[base_field(cc)]) == "checkbox")) "binary" else "numeric"
      } else "text"
    }, ""),
    stringsAsFactors = FALSE, row.names = NULL)
  out
}
