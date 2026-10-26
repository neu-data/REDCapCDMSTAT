`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

is_blank <- function(x) {
  if (is.null(x)) return(logical())
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    is.na(x) | !nzchar(trimws(x))
  } else {
    is.na(x)
  }
}

now_stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

new_id <- function(prefix) {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
         paste(sample(c(0:9, letters[1:6]), 6, TRUE), collapse = ""))
}

# Parse a REDCap choice string ("1, Male | 2, Female") into codes and labels.
#' Parse a REDCap choice string
#'
#' @param x Input text.
#' @return Data frame with `code` and `label`.
#' @examples
#' parse_choices("1, Male | 2, Female")
#' @export
parse_choices <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) {
    return(data.frame(code = character(), label = character(), stringsAsFactors = FALSE))
  }
  parts <- trimws(strsplit(x, "|", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  data.frame(
    code = trimws(sub(",.*$", "", parts)),
    label = trimws(sub("^[^,]*,", "", parts)),
    stringsAsFactors = FALSE
  )
}

# Replace every regex match using a callback fn(match, groups).
gsub_fn <- function(x, pattern, fn) {
  m <- gregexpr(pattern, x, perl = TRUE)[[1]]
  if (m[1] == -1) return(x)
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  cs <- attr(m, "capture.start")
  cl <- attr(m, "capture.length")
  out <- ""
  pos <- 1
  for (i in seq_along(starts)) {
    groups <- if (!is.null(cs) && ncol(cs) > 0) {
      substring(x, cs[i, ], cs[i, ] + cl[i, ] - 1)
    } else character()
    out <- paste0(out, substr(x, pos, starts[i] - 1),
                  fn(substr(x, starts[i], starts[i] + lens[i] - 1), groups))
    pos <- starts[i] + lens[i]
  }
  paste0(out, substr(x, pos, nchar(x)))
}

# rbind data frames whose columns differ, filling missing columns with NA.
rbind_fill <- function(a, b) {
  if (is.null(a) || !nrow(a)) return(b)
  if (is.null(b) || !nrow(b)) return(a)
  cols <- union(names(a), names(b))
  for (cc in setdiff(cols, names(a))) a[[cc]] <- rep(NA_character_, nrow(a))
  for (cc in setdiff(cols, names(b))) b[[cc]] <- rep(NA_character_, nrow(b))
  rbind(a[cols], b[cols])
}

as_chr_df <- function(x) {
  if (is.null(x) || length(x) == 0) return(data.frame())
  df <- as.data.frame(x, stringsAsFactors = FALSE)
  df[] <- lapply(df, function(v) { v <- as.character(v); v[is.na(v)] <- ""; v })
  df
}

order_ids <- function(ids) order(suppressWarnings(as.numeric(as.character(ids))), as.character(ids))

field_label <- function(md, field) {
  i <- match(field, md$field_name)
  if (is.na(i)) return(field)
  lab <- gsub("<[^>]+>", "", md$field_label[i])
  if (nzchar(lab)) lab else field
}

is_numeric_validation <- function(v) !is.na(v) & grepl("^(integer|number)", v)
is_date_validation <- function(v) !is.na(v) & grepl("^(date|datetime)", v)

# Classify each metadata field into an analysis type.
field_kinds <- function(md) {
  vt <- md$text_validation_type_or_show_slider_number
  kind <- rep("text", nrow(md))
  kind[md$field_type %in% c("calc", "slider") |
         (md$field_type == "text" & is_numeric_validation(vt))] <- "numeric"
  kind[md$field_type %in% c("radio", "dropdown", "yesno", "truefalse")] <- "categorical"
  kind[md$field_type == "checkbox"] <- "checkbox"
  kind[md$field_type == "text" & is_date_validation(vt)] <- "date"
  kind[md$field_type %in% c("descriptive", "file", "notes", "signature")] <- "other"
  stats::setNames(kind, md$field_name)
}
