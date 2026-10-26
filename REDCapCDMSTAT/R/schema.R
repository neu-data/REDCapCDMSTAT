#' Hash a data dictionary so drift can be detected cheaply
#'
#' @param md Data dictionary (REDCap metadata export) as a data frame.
#' @return SHA-256 hash string.
#' @export
dict_hash <- function(md) {
  md <- as.data.frame(md, stringsAsFactors = FALSE)
  rownames(md) <- NULL
  digest::digest(md, algo = "sha256")
}

#' Describe what changed between two data dictionaries
#'
#' @param old Previous data dictionary.
#' @param new Current data dictionary.
#' @return List with `added`, `removed` and `changed` (per field, the changed columns with old and new values).
#' @export
dict_diff <- function(old, new) {
  added <- setdiff(new$field_name, old$field_name)
  removed <- setdiff(old$field_name, new$field_name)
  common <- intersect(old$field_name, new$field_name)
  cols <- intersect(names(old), names(new))
  changed <- list()
  for (f in common) {
    a <- old[match(f, old$field_name), cols]
    b <- new[match(f, new$field_name), cols]
    diffcols <- cols[as.character(unlist(a)) != as.character(unlist(b))]
    if (length(diffcols)) {
      changed[[f]] <- lapply(stats::setNames(diffcols, diffcols), function(cc) {
        list(old = as.character(a[[cc]]), new = as.character(b[[cc]]))
      })
    }
  }
  list(added = added, removed = removed, changed = changed)
}

summarise_diff <- function(d) {
  parts <- character()
  if (length(d$added)) parts <- c(parts, paste0("added: ", paste(d$added, collapse = ", ")))
  if (length(d$removed)) parts <- c(parts, paste0("removed: ", paste(d$removed, collapse = ", ")))
  if (length(d$changed)) {
    parts <- c(parts, paste0("changed: ", paste(
      vapply(names(d$changed), function(f) paste0(f, " (", paste(names(d$changed[[f]]), collapse = "/"), ")"), ""),
      collapse = ", ")))
  }
  if (!length(parts)) "no field changes" else paste(parts, collapse = "; ")
}

# Parse a REDCap logging export into changed / deleted records and design events.
parse_log <- function(log) {
  if (is.null(log) || !NROW(log)) return(list(changed = character(), deleted = character(), design = FALSE))
  action <- as.character(log$action)
  m <- regmatches(action, regexec("(?i)(create|update|delete|lock|unlock|esign)\\w*\\s+record\\s+([^ ]+)", action, perl = TRUE))
  kind <- tolower(vapply(m, function(z) if (length(z)) z[2] else NA_character_, ""))
  rec <- vapply(m, function(z) if (length(z)) z[3] else NA_character_, "")
  if ("record" %in% names(log)) rec[is.na(rec) & nzchar(log$record)] <- log$record[is.na(rec) & nzchar(log$record)]
  deleted <- unique(rec[!is.na(kind) & kind == "delete"])
  changed <- unique(rec[!is.na(rec) & (is.na(kind) | kind != "delete")])
  list(
    changed = changed,
    deleted = deleted,
    design = any(grepl("Manage/Design|data dictionary|project field", action, ignore.case = TRUE))
  )
}
