# CDISC SDTM export (DM, VS, LB, AE) driven by a field mapping, plus ODM.
#
# The mapping says which study fields feed which SDTM variables, so any
# project can be exported; `sdtm_default_map()` guesses one from common field
# names and is what the demo uses. Coded adverse-event terms from the medical
# coding module populate AEDECOD.

#' Guess an SDTM mapping from field names; edit the result for real studies
#'
#' @param md Data dictionary (REDCap metadata export) as a data frame.
#' @return Nested list mapping SDTM variables to study fields; edit it and pass it to `$sdtm(map = )`.
#' @export
sdtm_default_map <- function(md) {
  has <- function(f) f[f %in% md$field_name]
  pick <- function(...) { h <- has(c(...)); if (length(h)) h[1] else NA_character_ }
  list(
    dm = list(rfstdtc = pick("consent_date", "enrollment_date", "enrol_date", "rand_date"),
              brthdtc = pick("dob", "birth_date", "date_of_birth"), age = pick("age"), sex = pick("sex", "gender"),
              arm = detect_arm(md), siteid = pick("site", "site_id", "centre", "center")),
    vs = list(tests = c(WEIGHT = pick("weight"), HEIGHT = pick("height"), BMI = pick("bmi"),
                        SYSBP = pick("sbp", "systolic"), DIABP = pick("dbp", "diastolic")),
              units = c(WEIGHT = "kg", HEIGHT = "cm", BMI = "kg/m2", SYSBP = "mmHg", DIABP = "mmHg"),
              names = c(WEIGHT = "Weight", HEIGHT = "Height", BMI = "Body Mass Index", SYSBP = "Systolic Blood Pressure",
                        DIABP = "Diastolic Blood Pressure"),
              date = pick("visit_date", "vs_date")),
    lb = list(tests = c(HBA1C = pick("hba1c"), CREAT = pick("creatinine", "creat")),
              units = c(HBA1C = "%", CREAT = "umol/L"), names = c(HBA1C = "Hemoglobin A1C", CREAT = "Creatinine"),
              date = pick("visit_date", "lab_date")),
    sv = list(date = pick("visit_date", "sv_date")),
    ds = list(reason = pick("ds_reason", "discontinuation_reason", "withdrawal_reason"),
              date = pick("ds_date", "discontinuation_date", "end_of_study_date")),
    ae = list(term = pick("ae_term", "aeterm"), severity = pick("ae_grade", "ae_severity"), serious = pick("ae_serious"),
              start = pick("ae_start", "ae_onset"))
  )
}

CDMSTAT$set("public", "sdtm", function(map = NULL, studyid = NULL) {
  md <- self$metadata
  map <- map %||% sdtm_default_map(md)
  studyid <- studyid %||% (self$info$project_id %||% "STUDY")
  id <- self$id_field()
  lab <- self$shape("long", "drop", labels = TRUE, log = FALSE)
  raw <- self$shape("long", "drop", labels = FALSE, log = FALSE)
  usubjid <- function(x) paste(studyid, x, sep = "-")
  fmt_date <- function(x) ifelse(is.na(x), "", format(as.Date(x), "%Y-%m-%d"))
  ev_names <- stats::setNames(self$events$event_name %||% character(), self$events$unique_event_name %||% character())
  ev_num <- stats::setNames(seq_along(ev_names), names(ev_names))
  sites <- site_of(self$data(), md, id)
  self$log_code(sprintf("sdtm <- nx$sdtm(studyid = %s)", deparse(studyid)))

  get1 <- function(df, f) {
    if (is.na(f) || !f %in% names(df)) return(rep(NA, length(unique(df[[id]]))))
    b <- first_non_na_by(df[, c(id, f)], id)
    b[[f]][match(unique(df[[id]]), b[[id]])]
  }
  subj <- unique(lab[[id]])
  sex <- as.character(get1(lab, map$dm$sex))
  sex <- ifelse(grepl("^m", sex, ignore.case = TRUE), "M", ifelse(grepl("^f", sex, ignore.case = TRUE), "F", "U"))
  arm <- as.character(get1(lab, map$dm$arm))
  dm <- data.frame(STUDYID = studyid, DOMAIN = "DM", USUBJID = usubjid(subj), SUBJID = subj,
                   RFSTDTC = fmt_date(get1(raw, map$dm$rfstdtc)), SITEID = unname(sites[subj]),
                   BRTHDTC = fmt_date(get1(raw, map$dm$brthdtc)), AGE = get1(raw, map$dm$age), AGEU = "YEARS",
                   SEX = sex, ARM = ifelse(is.na(arm), "Not Assigned", arm), ACTARM = ifelse(is.na(arm), "Not Assigned", arm),
                   COUNTRY = "", stringsAsFactors = FALSE)

  findings <- function(spec, domain, prefix) {
    tests <- spec$tests[!is.na(spec$tests)]
    if (!length(tests)) return(data.frame())
    rows <- list()
    for (cd in names(tests)) {
      f <- tests[[cd]]
      ok <- !is.na(raw[[f]])
      if (!any(ok)) next
      r <- raw[ok, , drop = FALSE]
      rows[[cd]] <- data.frame(STUDYID = studyid, DOMAIN = domain, USUBJID = usubjid(r[[id]]),
                               TESTCD = cd, TEST = spec$names[[cd]] %||% cd,
                               ORRES = as.character(r[[f]]), ORRESU = spec$units[[cd]] %||% "",
                               STRESN = as.numeric(r[[f]]), STRESU = spec$units[[cd]] %||% "",
                               VISITNUM = unname(ev_num[r$redcap_event_name %||% NA]),
                               VISIT = unname(ev_names[r$redcap_event_name %||% NA]),
                               DTC = if (!is.na(spec$date) && spec$date %in% names(r)) fmt_date(r[[spec$date]]) else "",
                               stringsAsFactors = FALSE)
    }
    out <- do.call(rbind, rows)
    if (is.null(out)) return(data.frame())
    out <- out[order(out$USUBJID, out$TESTCD, out$VISITNUM), ]
    out$SEQ <- stats::ave(seq_len(nrow(out)), out$USUBJID, FUN = seq_along)
    names(out)[names(out) %in% c("TESTCD", "TEST", "ORRES", "ORRESU", "STRESN", "STRESU", "DTC", "SEQ")] <-
      paste0(prefix, names(out)[names(out) %in% c("TESTCD", "TEST", "ORRES", "ORRESU", "STRESN", "STRESU", "DTC", "SEQ")])
    rownames(out) <- NULL
    out
  }
  vs <- findings(map$vs, "VS", "VS")
  lb <- findings(map$lb, "LB", "LB")

  ae <- data.frame()
  if (!is.na(map$ae$term)) {
    rep_lab <- self$shape("long", "only", labels = TRUE, log = FALSE)
    rep_lab <- rep_lab[!is_blank(rep_lab[[map$ae$term]]), , drop = FALSE]
    if (nrow(rep_lab)) {
      cod <- self$coding()
      key <- paste(rep_lab[[id]], rep_lab$redcap_event_name %||% "", rep_lab$redcap_repeat_instance %||% "", map$ae$term, sep = "|")
      ser <- if (!is.na(map$ae$serious)) as.character(rep_lab[[map$ae$serious]]) else NA
      ae <- data.frame(STUDYID = studyid, DOMAIN = "AE", USUBJID = usubjid(rep_lab[[id]]),
                       AETERM = as.character(rep_lab[[map$ae$term]]),
                       AEDECOD = cod$term[match(key, cod$coding_key)] %||% NA,
                       AEDECODCD = cod$code[match(key, cod$coding_key)] %||% NA,
                       AESEV = if (!is.na(map$ae$severity)) toupper(as.character(rep_lab[[map$ae$severity]])) else "",
                       AESER = ifelse(is.na(ser), "", ifelse(ser %in% c("Yes", "1"), "Y", "N")),
                       VISIT = unname(ev_names[rep_lab$redcap_event_name %||% NA]), stringsAsFactors = FALSE)
      ae <- ae[order(ae$USUBJID), ]
      ae$AESEQ <- stats::ave(seq_len(nrow(ae)), ae$USUBJID, FUN = seq_along)
      rownames(ae) <- NULL
    }
  }

  # SV: one row per subject visit with data.
  sv <- data.frame()
  if ("redcap_event_name" %in% names(raw)) {
    vdate <- map$sv$date %||% NA
    sv <- data.frame(STUDYID = studyid, DOMAIN = "SV", USUBJID = usubjid(raw[[id]]),
                     VISITNUM = unname(ev_num[raw$redcap_event_name]), VISIT = unname(ev_names[raw$redcap_event_name]),
                     SVSTDTC = if (!is.na(vdate) && vdate %in% names(raw)) fmt_date(raw[[vdate]]) else "",
                     stringsAsFactors = FALSE)
    sv$SVENDTC <- sv$SVSTDTC
    sv <- sv[order(sv$USUBJID, sv$VISITNUM), ]
    rownames(sv) <- NULL
  }

  # DS: disposition. Subjects with data at the final scheduled event are
  # COMPLETED; otherwise the mapped reason field (if any) gives DSTERM/DSDECOD.
  ds <- data.frame()
  if ("redcap_event_name" %in% names(raw) && length(ev_names)) {
    last_ev <- names(ev_names)[length(ev_names)]
    done <- unique(raw[[id]][raw$redcap_event_name == last_ev])
    reason <- if (!is.na(map$ds$reason %||% NA)) as.character(get1(lab, map$ds$reason)) else rep(NA_character_, length(subj))
    ddate <- if (!is.na(map$ds$date %||% NA)) fmt_date(get1(raw, map$ds$date)) else rep("", length(subj))
    comp <- subj %in% done
    ds <- data.frame(STUDYID = studyid, DOMAIN = "DS", USUBJID = usubjid(subj), DSSEQ = 1L,
                     DSTERM = ifelse(comp, "COMPLETED", ifelse(is.na(reason), "FINAL VISIT NOT RECORDED", reason)),
                     DSDECOD = ifelse(comp, "COMPLETED", ifelse(is.na(reason), "", toupper(reason))),
                     DSCAT = "DISPOSITION EVENT", DSSTDTC = ddate, stringsAsFactors = FALSE)
  }
  list(DM = dm, VS = vs, LB = lb, AE = ae, SV = sv, DS = ds)
})

sdtm_labels <- c(
  STUDYID = "Study Identifier", DOMAIN = "Domain Abbreviation", USUBJID = "Unique Subject Identifier",
  SUBJID = "Subject Identifier for the Study", RFSTDTC = "Subject Reference Start Date/Time", SITEID = "Study Site Identifier",
  BRTHDTC = "Date/Time of Birth", AGE = "Age", AGEU = "Age Units", SEX = "Sex", ARM = "Description of Planned Arm",
  ACTARM = "Description of Actual Arm", COUNTRY = "Country", VISITNUM = "Visit Number", VISIT = "Visit Name",
  VSSEQ = "Sequence Number", VSTESTCD = "Vital Signs Test Short Name", VSTEST = "Vital Signs Test Name",
  VSORRES = "Result or Finding in Original Units", VSORRESU = "Original Units", VSSTRESN = "Numeric Result/Finding in Standard Units",
  VSSTRESU = "Standard Units", VSDTC = "Date/Time of Measurements",
  LBSEQ = "Sequence Number", LBTESTCD = "Lab Test or Examination Short Name", LBTEST = "Lab Test or Examination Name",
  LBORRES = "Result or Finding in Original Units", LBORRESU = "Original Units", LBSTRESN = "Numeric Result/Finding in Standard Units",
  LBSTRESU = "Standard Units", LBDTC = "Date/Time of Specimen Collection",
  AESEQ = "Sequence Number", AETERM = "Reported Term for the Adverse Event", AEDECOD = "Dictionary-Derived Term",
  AEDECODCD = "Dictionary-Derived Term Code", AESEV = "Severity/Intensity", AESER = "Serious Event",
  SVSTDTC = "Start Date/Time of Visit", SVENDTC = "End Date/Time of Visit",
  DSSEQ = "Sequence Number", DSTERM = "Reported Term for the Disposition Event", DSDECOD = "Standardized Disposition Term",
  DSCAT = "Category for Disposition Event", DSSTDTC = "Start Date/Time of Disposition Event")

sdtm_domain_info <- list(
  DM = list(label = "Demographics", class = "SPECIAL PURPOSE", structure = "One record per subject", keys = "STUDYID, USUBJID"),
  VS = list(label = "Vital Signs", class = "FINDINGS", structure = "One record per vital sign measurement per visit per subject", keys = "STUDYID, USUBJID, VSTESTCD, VISITNUM"),
  LB = list(label = "Laboratory Test Results", class = "FINDINGS", structure = "One record per lab test per visit per subject", keys = "STUDYID, USUBJID, LBTESTCD, VISITNUM"),
  AE = list(label = "Adverse Events", class = "EVENTS", structure = "One record per adverse event per subject", keys = "STUDYID, USUBJID, AEDECOD, AESEQ"),
  SV = list(label = "Subject Visits", class = "TRIAL DESIGN", structure = "One record per subject per actual visit", keys = "STUDYID, USUBJID, VISITNUM"),
  DS = list(label = "Disposition", class = "EVENTS", structure = "One record per disposition status per subject", keys = "STUDYID, USUBJID, DSSEQ"))

#' Write a Define-XML 2.0 document describing exported SDTM domains
#'
#' Covers dataset and variable metadata (labels, types, lengths, keys and
#' origins). Controlled-terminology code lists and value-level metadata are
#' not generated and must be added before a regulatory submission.
#'
#' @param doms Named list of SDTM domain data frames, as returned by `$sdtm()`.
#' @param file Path to a file.
#' @param studyid STUDYID value.
#' @param study_name Study name.
#' @return The file path, invisibly.
#' @export
write_define <- function(doms, file, studyid = "STUDY", study_name = studyid) {
  dtype <- function(v) if (is.integer(v)) "integer" else if (is.numeric(v)) "float" else "text"
  out <- c('<?xml version="1.0" encoding="UTF-8"?>',
           sprintf('<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3" xmlns:def="http://www.cdisc.org/ns/def/v2.0" xmlns:xlink="http://www.w3.org/1999/xlink" ODMVersion="1.3.2" FileType="Snapshot" FileOID="DEFINE.%s" CreationDateTime="%s" Originator="REDCapCDMSTAT">',
                   xml_esc(studyid), format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
           sprintf('<Study OID="%s"><GlobalVariables><StudyName>%s</StudyName><StudyDescription>%s</StudyDescription><ProtocolName>%s</ProtocolName></GlobalVariables>',
                   xml_esc(studyid), xml_esc(study_name), xml_esc(study_name), xml_esc(studyid)),
           '<MetaDataVersion OID="MDV.SDTM" Name="SDTM metadata" def:DefineVersion="2.0.0" def:StandardName="SDTMIG" def:StandardVersion="3.3">')
  items <- character()
  for (nm in names(doms)) {
    d <- doms[[nm]]
    if (!nrow(d)) next
    info <- sdtm_domain_info[[nm]] %||% list(label = nm, class = "", structure = "", keys = "")
    keys <- trimws(strsplit(info$keys, ",")[[1]])
    out <- c(out, sprintf('<ItemGroupDef OID="IG.%s" Name="%s" Domain="%s" SASDatasetName="%s" Repeating="%s" IsReferenceData="No" Purpose="Tabulation" def:Structure="%s" def:Class="%s" def:ArchiveLocationID="LF.%s">',
                          nm, nm, nm, nm, if (nm == "DM") "No" else "Yes", xml_esc(info$structure), xml_esc(info$class), nm),
             sprintf('<Description><TranslatedText xml:lang="en">%s</TranslatedText></Description>', xml_esc(info$label)))
    for (k in seq_along(d)) {
      v <- names(d)[k]
      key <- match(v, keys)
      out <- c(out, sprintf('<ItemRef ItemOID="IT.%s.%s" OrderNumber="%d" Mandatory="%s"%s/>', nm, v, k,
                            if (v %in% c("STUDYID", "DOMAIN", "USUBJID")) "Yes" else "No",
                            if (!is.na(key)) sprintf(' KeySequence="%d"', key) else ""))
      len <- max(1L, suppressWarnings(max(nchar(as.character(d[[v]])), na.rm = TRUE)))
      origin <- if (v %in% c("STUDYID", "DOMAIN", "AGEU", "DSCAT")) "Assigned" else
        if (grepl("SEQ$|^VISITNUM$|DECOD", v)) "Derived" else "Collected"
      lab <- if (v %in% names(sdtm_labels)) sdtm_labels[[v]] else v
      items <- c(items, sprintf('<ItemDef OID="IT.%s.%s" Name="%s" SASFieldName="%s" DataType="%s" Length="%d"><Description><TranslatedText xml:lang="en">%s</TranslatedText></Description><def:Origin Type="%s"/></ItemDef>',
                                nm, v, v, v, dtype(d[[v]]), len, xml_esc(lab), origin))
    }
    out <- c(out, sprintf('<def:leaf ID="LF.%s" xlink:href="%s.xpt"><def:title>%s.xpt</def:title></def:leaf>', nm, tolower(nm), tolower(nm)),
             "</ItemGroupDef>")
  }
  out <- c(out, items, "</MetaDataVersion></Study></ODM>")
  writeLines(out, file, useBytes = TRUE)
  invisible(file)
}

# Write SDTM domains (CSV, and SAS transport .xpt when 'haven' is installed)
# and the study as CDISC ODM to a folder.
CDMSTAT$set("public", "export_cdisc", function(out_dir, map = NULL, studyid = NULL, xpt = TRUE) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  doms <- self$sdtm(map, studyid)
  files <- character()
  for (nm in names(doms)) {
    d <- doms[[nm]]
    if (!nrow(d)) next
    f <- file.path(out_dir, paste0(tolower(nm), ".csv"))
    utils::write.csv(d, f, row.names = FALSE, na = "")
    files <- c(files, f)
    if (xpt && requireNamespace("haven", quietly = TRUE)) {
      fx <- file.path(out_dir, paste0(tolower(nm), ".xpt"))
      haven::write_xpt(d, fx, version = 5, name = nm)
      files <- c(files, fx)
    }
  }
  define <- file.path(out_dir, "define.xml")
  write_define(doms, define, studyid %||% (self$info$project_id %||% "STUDY"), self$info$project_title %||% "")
  files <- c(files, define)
  odm <- file.path(out_dir, "study.odm.xml")
  write_odm(list(info = self$info, metadata = self$metadata, events = self$events, mapping = self$mapping,
                 repeating = self$repeating, records = self$raw), odm)
  self$ledger$activity(self$actor, "cdisc", "export", paste(basename(c(files, odm)), collapse = ", "))
  invisible(normalizePath(c(files, odm), winslash = "/"))
})
