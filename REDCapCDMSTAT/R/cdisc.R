# CDISC SDTM export (DM, VS, LB, AE) driven by a field mapping, plus ODM.
#
# The mapping says which study fields feed which SDTM variables, so any
# project can be exported; `sdtm_default_map()` guesses one from common field
# names and is what the demo uses. Coded adverse-event terms from the medical
# coding module populate AEDECOD.

#' Guess an SDTM mapping from field names; edit the result for real studies.
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
  list(DM = dm, VS = vs, LB = lb, AE = ae)
})

#' Write SDTM domains (CSV, and SAS transport .xpt when 'haven' is installed)
#' and the study as CDISC ODM to a folder.
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
  odm <- file.path(out_dir, "study.odm.xml")
  write_odm(list(info = self$info, metadata = self$metadata, events = self$events, mapping = self$mapping,
                 repeating = self$repeating, records = self$raw), odm)
  self$ledger$activity(self$actor, "cdisc", "export", paste(basename(c(files, odm)), collapse = ", "))
  invisible(normalizePath(c(files, odm), winslash = "/"))
})
