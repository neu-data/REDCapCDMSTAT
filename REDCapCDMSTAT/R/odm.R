# CDISC ODM 1.3 import and export.
#
# ODM is the vendor-neutral exchange format exported by REDCap, REDCap Cloud,
# OpenClinica, Medidata Rave and most other EDC systems, so it is the common
# path for bringing non-REDCap studies into the engine. REDCap's own
# "redcap:" attributes (field type, validation, branching logic, calculations)
# are honoured when present, which makes a REDCap -> ODM -> CDMSTAT round
# trip lossless; for other systems the ODM data types, code lists, range
# checks and mandatory flags are mapped to the nearest REDCap equivalent.

odm_name <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9_]+", "_", x))
  x <- gsub("^_+|_+$", "", x)
  ifelse(grepl("^[0-9]", x), paste0("v_", x), x)
}

xml_esc <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

ga <- function(a, name, default = "") if (name %in% names(a)) a[[name]] else default

attr_of <- function(node, name, default = "") {
  a <- xml2::xml_attrs(node)
  if (name %in% names(a)) a[[name]] else default
}

text_of <- function(node, xpath) {
  n <- xml2::xml_find_first(node, xpath)
  if (inherits(n, "xml_missing")) "" else trimws(xml2::xml_text(n))
}

#' Parse a CDISC ODM 1.3 document into the REDCap-shaped structures used by
#' the engine: info, metadata, events, mapping, repeating and records.
read_odm <- function(x) {
  doc <- if (inherits(x, "xml_document")) x else xml2::read_xml(x)
  xml2::xml_ns_strip(doc)
  study <- xml2::xml_find_first(doc, "//Study")
  mdv <- xml2::xml_find_first(study, ".//MetaDataVersion")

  codelists <- list()
  for (cl in xml2::xml_find_all(mdv, "./CodeList")) {
    items <- xml2::xml_find_all(cl, "./CodeListItem|./EnumeratedItem")
    codes <- vapply(items, attr_of, "", name = "CodedValue")
    labels <- vapply(items, function(it) { l <- text_of(it, "./Decode/TranslatedText"); if (nzchar(l)) l else attr_of(it, "CodedValue") }, "")
    codelists[[attr_of(cl, "OID")]] <- paste(codes, labels, sep = ", ", collapse = " | ")
  }

  itemdefs <- list()
  for (it in xml2::xml_find_all(mdv, "./ItemDef")) itemdefs[[attr_of(it, "OID")]] <- it
  groupdefs <- list()
  for (g in xml2::xml_find_all(mdv, "./ItemGroupDef")) groupdefs[[attr_of(g, "OID")]] <- g

  form_name <- list()
  rows <- list()
  item_field <- character()
  rep_forms <- character()
  for (fd in xml2::xml_find_all(mdv, "./FormDef")) {
    foid <- attr_of(fd, "OID")
    fname <- odm_name(attr_of(fd, "FormName", attr_of(fd, "Name", foid)))
    form_name[[foid]] <- fname
    if (identical(attr_of(fd, "Repeating"), "Yes")) rep_forms <- c(rep_forms, fname)
    for (gr in xml2::xml_find_all(fd, "./ItemGroupRef")) {
      g <- groupdefs[[attr_of(gr, "ItemGroupOID")]]
      if (is.null(g)) next
      if (identical(attr_of(g, "Repeating"), "Yes")) rep_forms <- c(rep_forms, fname)
      for (ir in xml2::xml_find_all(g, "./ItemRef")) {
        ioid <- attr_of(ir, "ItemOID")
        it <- itemdefs[[ioid]]
        if (is.null(it)) next
        a <- xml2::xml_attrs(it)
        fld <- if (nzchar(ga(a, "Variable"))) ga(a, "Variable") else odm_name(ga(a, "Name", ioid))
        item_field[[ioid]] <- fld
        if (fld %in% vapply(rows, `[[`, "", "field_name")) next
        clref <- xml2::xml_find_first(it, "./CodeListRef")
        choices <- if (!inherits(clref, "xml_missing")) codelists[[attr_of(clref, "CodeListOID")]] %||% "" else ""
        dtype <- tolower(ga(a, "DataType", "text"))
        ftype <- ga(a, "FieldType", NA)
        vt <- ga(a, "TextValidationType", NA)
        if (is.na(ftype)) {
          ftype <- if (nzchar(choices)) "radio" else if (dtype == "boolean") "yesno" else "text"
          vt <- switch(dtype, integer = "integer", float = "number", double = "number", date = "date_ymd",
                       datetime = "datetime_ymd_hms", partialdate = "date_ymd", "")
        }
        if (ftype %in% c("checkbox")) ftype <- "yesno"   # REDCap ODM exports checkboxes as one boolean item per option
        vmin <- vmax <- ""
        for (rc in xml2::xml_find_all(it, "./RangeCheck")) {
          cmp <- attr_of(rc, "Comparator")
          val <- text_of(rc, "./CheckValue")
          if (cmp %in% c("GE", "GT")) vmin <- val
          if (cmp %in% c("LE", "LT")) vmax <- val
        }
        rows[[length(rows) + 1]] <- data.frame(
          field_name = fld, form_name = fname, section_header = "", field_type = ftype,
          field_label = { l <- text_of(it, "./Question/TranslatedText"); if (nzchar(l)) l else fld },
          select_choices_or_calculations = if (ftype == "calc") ga(a, "Calculation") else if (ftype %in% c("radio", "dropdown")) choices else "",
          field_note = ga(a, "FieldNote"),
          text_validation_type_or_show_slider_number = if (is.na(vt)) "" else vt,
          text_validation_min = ga(a, "TextValidationMin", vmin), text_validation_max = ga(a, "TextValidationMax", vmax),
          identifier = if (identical(ga(a, "Identifier"), "y")) "y" else "",
          branching_logic = ga(a, "BranchingLogic"),
          required_field = if (identical(attr_of(ir, "Mandatory"), "Yes") || identical(ga(a, "RequiredField"), "y")) "y" else "",
          custom_alignment = "", question_number = "", matrix_group_name = "", matrix_ranking = "",
          field_annotation = ga(a, "FieldAnnotation"), stringsAsFactors = FALSE)
      }
    }
  }
  md <- do.call(rbind, rows)
  md[] <- lapply(md, function(v) { v <- as.character(v); v[is.na(v)] <- ""; v })
  # A subject identifier field always comes first, as in REDCap.
  id_field <- "record_id"
  md <- md[md$field_name != id_field, , drop = FALSE]
  md <- rbind(data.frame(field_name = id_field, form_name = md$form_name[1], section_header = "", field_type = "text",
                         field_label = "Subject ID", select_choices_or_calculations = "", field_note = "",
                         text_validation_type_or_show_slider_number = "", text_validation_min = "",
                         text_validation_max = "", identifier = "", branching_logic = "", required_field = "",
                         custom_alignment = "", question_number = "", matrix_group_name = "", matrix_ranking = "",
                         field_annotation = "", stringsAsFactors = FALSE), md)
  rownames(md) <- NULL

  evs <- xml2::xml_find_all(mdv, "./StudyEventDef")
  ev_oid <- vapply(evs, attr_of, "", name = "OID")
  ev_unique <- vapply(evs, function(e) { u <- attr_of(e, "UniqueEventName"); if (nzchar(u)) u else odm_name(attr_of(e, "Name", attr_of(e, "OID"))) }, "")
  events <- data.frame(event_name = vapply(evs, attr_of, "", name = "Name"), arm_num = "1",
                       unique_event_name = ev_unique, day_offset = vapply(evs, attr_of, "", name = "DayOffset"),
                       stringsAsFactors = FALSE)
  mapping <- do.call(rbind, lapply(seq_along(evs), function(k) {
    f <- vapply(xml2::xml_find_all(evs[[k]], "./FormRef"), function(r) form_name[[attr_of(r, "FormOID")]] %||% "", "")
    if (!length(f)) return(NULL)
    data.frame(arm_num = "1", unique_event_name = ev_unique[k], form = f, stringsAsFactors = FALSE)
  })) %||% data.frame(arm_num = character(), unique_event_name = character(), form = character())
  rep_forms <- unique(rep_forms)
  repeating <- mapping[mapping$form %in% rep_forms, c("unique_event_name", "form"), drop = FALSE]
  names(repeating) <- c("event_name", "form_name")

  # Clinical data: one row per subject x event, plus one per repeating instance.
  forms <- unique(md$form_name)
  cols <- c(id_field, "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance",
            unlist(lapply(forms, function(f) c(setdiff(md$field_name[md$form_name == f], id_field), paste0(f, "_complete")))))
  rec <- list()
  key_of <- function(...) paste(..., sep = "\r")
  for (fdat in xml2::xml_find_all(doc, "//ClinicalData/SubjectData/StudyEventData/FormData")) {
    sev <- xml2::xml_parent(fdat)
    subj <- xml2::xml_parent(sev)
    sk <- attr_of(subj, "SubjectKey")
    ev <- ev_unique[match(attr_of(sev, "StudyEventOID"), ev_oid)]
    if (is.na(ev)) next
    fname <- form_name[[attr_of(fdat, "FormOID")]]
    if (is.null(fname)) next
    is_rep <- fname %in% rep_forms
    groups <- xml2::xml_find_all(fdat, "./ItemGroupData")
    for (g in groups) {
      inst <- if (is_rep) { r <- attr_of(fdat, "FormRepeatKey"); if (!nzchar(r)) r <- attr_of(g, "ItemGroupRepeatKey", "1"); r } else ""
      k <- key_of(sk, ev, if (is_rep) fname else "", inst)
      row <- rec[[k]] %||% c(stats::setNames(rep("", length(cols)), cols))
      row[c(id_field, "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")] <-
        c(sk, ev, if (is_rep) fname else "", inst)
      items <- xml2::xml_find_all(g, "./ItemData")
      ioids <- vapply(items, attr_of, "", name = "ItemOID")
      vals <- vapply(items, attr_of, "", name = "Value")
      flds <- unname(item_field[ioids])
      ok <- !is.na(flds) & flds %in% cols & flds != id_field
      row[flds[ok]] <- vals[ok]
      row[paste0(fname, "_complete")] <- attr_of(fdat, "FormStatus", "2")
      rec[[k]] <- row
    }
  }
  records <- if (length(rec)) as.data.frame(do.call(rbind, rec), stringsAsFactors = FALSE) else
    as.data.frame(matrix(character(), 0, length(cols), dimnames = list(NULL, cols)), stringsAsFactors = FALSE)
  rownames(records) <- NULL
  records <- records[order_ids(records[[id_field]]), , drop = FALSE]
  rownames(records) <- NULL

  list(info = list(project_id = attr_of(study, "OID"), project_title = text_of(study, ".//StudyName"),
                   is_longitudinal = if (nrow(events) > 1) "1" else "0",
                   has_repeating_instruments_or_events = if (nrow(repeating)) "1" else "0"),
       metadata = md, events = events, mapping = mapping, repeating = repeating, records = records)
}

#' Write a study (metadata + raw records) as a CDISC ODM 1.3 document.
#'
#' REDCap-specific attributes are written in the redcap: namespace so the file
#' re-imports losslessly; other systems ignore them.
write_odm <- function(state, file = NULL, study_oid = state$info$project_id %||% "STUDY",
                      clinical = TRUE) {
  md <- state$metadata
  id_field <- md$field_name[1]
  forms <- unique(md$form_name)
  events <- state$events
  if (is.null(events) || !nrow(events)) {
    events <- data.frame(event_name = "Event 1", unique_event_name = "event_1_arm_1", day_offset = "0")
  }
  rep_forms <- unique(state$repeating$form_name %||% character())
  dt <- function(i) {
    vt <- md$text_validation_type_or_show_slider_number[i]
    ft <- md$field_type[i]
    if (ft == "calc") return("float")
    if (ft %in% c("yesno", "truefalse")) return("boolean")
    if (grepl("^integer", vt)) "integer" else if (grepl("^number", vt)) "float" else
      if (grepl("^datetime", vt)) "datetime" else if (grepl("^date", vt)) "date" else "text"
  }
  oid_f <- function(f) paste0("F.", f)
  oid_i <- function(f) paste0("I.", f)
  out <- c('<?xml version="1.0" encoding="UTF-8"?>',
           sprintf('<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3" xmlns:redcap="https://projectredcap.org" ODMVersion="1.3.2" FileType="Snapshot" FileOID="%s" CreationDateTime="%s" SourceSystem="REDCapCDMSTAT">',
                   xml_esc(paste0(study_oid, "-", format(Sys.time(), "%Y%m%d%H%M%S"))), format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
           sprintf('<Study OID="%s"><GlobalVariables><StudyName>%s</StudyName><StudyDescription>%s</StudyDescription><ProtocolName>%s</ProtocolName></GlobalVariables>',
                   xml_esc(study_oid), xml_esc(state$info$project_title %||% study_oid), xml_esc(state$info$project_title %||% ""), xml_esc(study_oid)),
           '<MetaDataVersion OID="MDV.1" Name="CDMSTAT export"><Protocol>',
           sprintf('<StudyEventRef StudyEventOID="SE.%s" OrderNumber="%d" Mandatory="No"/>', xml_esc(events$unique_event_name), seq_len(nrow(events))),
           '</Protocol>')
  for (k in seq_len(nrow(events))) {
    ev <- events$unique_event_name[k]
    fs <- if (!is.null(state$mapping) && nrow(state$mapping)) state$mapping$form[state$mapping$unique_event_name == ev] else forms
    out <- c(out, sprintf('<StudyEventDef OID="SE.%s" Name="%s" Repeating="No" Type="Scheduled" redcap:UniqueEventName="%s" redcap:DayOffset="%s">',
                          xml_esc(ev), xml_esc(events$event_name[k]), xml_esc(ev), xml_esc(events$day_offset[k] %||% "")),
             sprintf('<FormRef FormOID="%s" Mandatory="No"/>', xml_esc(oid_f(fs))), "</StudyEventDef>")
  }
  for (f in forms) {
    out <- c(out, sprintf('<FormDef OID="%s" Name="%s" Repeating="%s" redcap:FormName="%s"><ItemGroupRef ItemGroupOID="IG.%s" Mandatory="No"/></FormDef>',
                          xml_esc(oid_f(f)), xml_esc(f), if (f %in% rep_forms) "Yes" else "No", xml_esc(f), xml_esc(f)))
  }
  cl <- character()
  item_cols <- list()
  for (f in forms) {
    idx <- which(md$form_name == f & md$field_type != "descriptive")
    refs <- character()
    for (i in idx) {
      fld <- md$field_name[i]
      mand <- if (identical(md$required_field[i], "y")) "Yes" else "No"
      if (md$field_type[i] == "checkbox") {
        ch <- parse_choices(md$select_choices_or_calculations[i])
        for (j in seq_len(nrow(ch))) {
          col <- paste0(fld, "___", gsub("-", "_", ch$code[j]))
          refs <- c(refs, sprintf('<ItemRef ItemOID="%s" Mandatory="No"/>', oid_i(col)))
          cl <- c(cl, sprintf('<ItemDef OID="%s" Name="%s" DataType="boolean" redcap:Variable="%s" redcap:FieldType="checkbox"><Question><TranslatedText>%s</TranslatedText></Question></ItemDef>',
                              oid_i(col), col, col, xml_esc(paste0(md$field_label[i], " (choice=", ch$label[j], ")"))))
          item_cols[[col]] <- col
        }
        next
      }
      refs <- c(refs, sprintf('<ItemRef ItemOID="%s" Mandatory="%s"/>', oid_i(fld), mand))
      choices <- parse_choices(md$select_choices_or_calculations[i])
      has_cl <- md$field_type[i] %in% c("radio", "dropdown") && nrow(choices)
      rc <- character()
      if (nzchar(md$text_validation_min[i]) && dt(i) %in% c("integer", "float")) {
        rc <- c(rc, sprintf('<RangeCheck Comparator="GE" SoftHard="Soft"><CheckValue>%s</CheckValue></RangeCheck>', xml_esc(md$text_validation_min[i])))
      }
      if (nzchar(md$text_validation_max[i]) && dt(i) %in% c("integer", "float")) {
        rc <- c(rc, sprintf('<RangeCheck Comparator="LE" SoftHard="Soft"><CheckValue>%s</CheckValue></RangeCheck>', xml_esc(md$text_validation_max[i])))
      }
      cl <- c(cl, paste0(
        sprintf('<ItemDef OID="%s" Name="%s" DataType="%s" redcap:Variable="%s" redcap:FieldType="%s" redcap:TextValidationType="%s" redcap:TextValidationMin="%s" redcap:TextValidationMax="%s" redcap:BranchingLogic="%s" redcap:Calculation="%s" redcap:RequiredField="%s" redcap:Identifier="%s" redcap:FieldNote="%s" redcap:FieldAnnotation="%s">',
                oid_i(fld), fld, dt(i), fld, md$field_type[i], xml_esc(md$text_validation_type_or_show_slider_number[i]),
                xml_esc(md$text_validation_min[i]), xml_esc(md$text_validation_max[i]), xml_esc(md$branching_logic[i]),
                xml_esc(if (md$field_type[i] == "calc") md$select_choices_or_calculations[i] else ""),
                xml_esc(md$required_field[i]), xml_esc(md$identifier[i]), xml_esc(md$field_note[i]), xml_esc(md$field_annotation[i])),
        sprintf('<Question><TranslatedText>%s</TranslatedText></Question>', xml_esc(md$field_label[i])),
        if (has_cl) sprintf('<CodeListRef CodeListOID="CL.%s"/>', fld) else "",
        paste(rc, collapse = ""), "</ItemDef>"))
      if (has_cl) {
        cl <- c(cl, paste0(sprintf('<CodeList OID="CL.%s" Name="%s" DataType="text">', fld, fld),
                           paste(sprintf('<CodeListItem CodedValue="%s"><Decode><TranslatedText>%s</TranslatedText></Decode></CodeListItem>',
                                         xml_esc(choices$code), xml_esc(choices$label)), collapse = ""), "</CodeList>"))
      }
      item_cols[[fld]] <- fld
    }
    out <- c(out, sprintf('<ItemGroupDef OID="IG.%s" Name="%s" Repeating="%s">', xml_esc(f), xml_esc(f), if (f %in% rep_forms) "Yes" else "No"),
             refs, "</ItemGroupDef>")
  }
  out <- c(out, cl, "</MetaDataVersion></Study>")

  if (clinical && !is.null(state$records) && nrow(state$records)) {
    r <- state$records
    out <- c(out, sprintf('<ClinicalData StudyOID="%s" MetaDataVersionOID="MDV.1">', xml_esc(study_oid)))
    ev_col <- if ("redcap_event_name" %in% names(r)) r$redcap_event_name else rep(events$unique_event_name[1], nrow(r))
    rep_col <- if ("redcap_repeat_instrument" %in% names(r)) r$redcap_repeat_instrument else rep("", nrow(r))
    rep_col[is.na(rep_col)] <- ""
    for (sk in unique(r[[id_field]])) {
      out <- c(out, sprintf('<SubjectData SubjectKey="%s">', xml_esc(sk)))
      rs <- which(r[[id_field]] == sk)
      for (ev in unique(ev_col[rs])) {
        out <- c(out, sprintf('<StudyEventData StudyEventOID="SE.%s">', xml_esc(ev)))
        for (i in rs[ev_col[rs] == ev]) {
          fs <- if (nzchar(rep_col[i])) rep_col[i] else setdiff(forms, rep_forms)
          for (f in fs) {
            colsf <- intersect(field_columns(md, md$field_name[md$form_name == f]), names(r))
            colsf <- setdiff(colsf, id_field)
            v <- vapply(colsf, function(cc) { z <- as.character(r[[cc]][i]); if (is.na(z)) "" else z }, "")
            if (!length(v) || all(!nzchar(v))) next
            rk <- if (nzchar(rep_col[i])) sprintf(' FormRepeatKey="%s"', xml_esc(r$redcap_repeat_instance[i])) else ""
            out <- c(out, sprintf('<FormData FormOID="%s"%s><ItemGroupData ItemGroupOID="IG.%s" ItemGroupRepeatKey="1" TransactionType="Upsert">', oid_f(f), rk, f),
                     sprintf('<ItemData ItemOID="%s" Value="%s"/>', oid_i(names(v)[nzchar(v)]), xml_esc(v[nzchar(v)])),
                     "</ItemGroupData></FormData>")
          }
        }
        out <- c(out, "</StudyEventData>")
      }
      out <- c(out, "</SubjectData>")
    }
    out <- c(out, "</ClinicalData>")
  }
  out <- c(out, "</ODM>")
  xml <- paste(out, collapse = "\n")
  if (!is.null(file)) { writeLines(xml, file, useBytes = TRUE); return(invisible(file)) }
  xml
}
