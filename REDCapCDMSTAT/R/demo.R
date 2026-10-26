#' Build a simulated longitudinal, randomised REDCap project.
#'
#' Two arms, three events, a repeating adverse-event form and deliberately
#' planted data problems (range errors, a hidden field with data, a stale
#' calculation, a missing required value, outliers) so that every part of the
#' pipeline has something to find.
demo_project <- function(seed = 42, n = 120) {
  set.seed(seed)
  fld <- function(name, form, type, label, choices = "", vt = "", vmin = "", vmax = "",
                  branching = "", required = "", identifier = "", note = "") {
    data.frame(field_name = name, form_name = form, section_header = "", field_type = type,
               field_label = label, select_choices_or_calculations = choices, field_note = note,
               text_validation_type_or_show_slider_number = vt, text_validation_min = vmin,
               text_validation_max = vmax, identifier = identifier, branching_logic = branching,
               required_field = required, custom_alignment = "", question_number = "",
               matrix_group_name = "", matrix_ranking = "", field_annotation = "",
               stringsAsFactors = FALSE)
  }
  md <- rbind(
    fld("record_id", "enrollment", "text", "Record ID"),
    fld("consent_date", "enrollment", "text", "Date of consent", vt = "date_ymd", required = "y"),
    fld("dob", "enrollment", "text", "Date of birth", vt = "date_ymd", identifier = "y"),
    fld("age", "enrollment", "calc", "Age at consent (years)",
        choices = "rounddown(datediff([dob],[consent_date],\"y\"),0)"),
    fld("sex", "enrollment", "radio", "Sex", choices = "1, Male | 2, Female", required = "y"),
    fld("pregnant", "enrollment", "yesno", "Currently pregnant?", branching = "[sex] = '2'"),
    fld("site", "enrollment", "dropdown", "Study site",
        choices = "1, Kampala | 2, Nairobi | 3, Dar es Salaam | 4, Kigali", required = "y"),
    fld("race", "enrollment", "checkbox", "Race (tick all that apply)",
        choices = "1, African | 2, Asian | 3, European | 4, Other"),
    fld("smoker", "enrollment", "yesno", "Current smoker"),
    fld("arm", "enrollment", "radio", "Randomised treatment arm", choices = "1, Drug A | 2, Placebo",
        required = "y"),
    fld("visit_date", "vitals", "text", "Visit date", vt = "date_ymd"),
    fld("weight", "vitals", "text", "Weight (kg)", vt = "number", vmin = "30", vmax = "200", required = "y"),
    fld("height", "vitals", "text", "Height (cm)", vt = "number", vmin = "120", vmax = "220"),
    fld("bmi", "vitals", "calc", "BMI (kg/m2)", choices = "round([weight]/(([height]/100)^2),1)"),
    fld("sbp", "vitals", "text", "Systolic BP (mmHg)", vt = "integer", vmin = "70", vmax = "250"),
    fld("dbp", "vitals", "text", "Diastolic BP (mmHg)", vt = "integer", vmin = "40", vmax = "150"),
    fld("hba1c", "labs", "text", "HbA1c (%)", vt = "number", vmin = "3", vmax = "20"),
    fld("creatinine", "labs", "text", "Creatinine (umol/L)", vt = "number", vmin = "20", vmax = "1000"),
    fld("event_occurred", "outcome", "yesno", "Primary endpoint event occurred"),
    fld("event_date", "outcome", "text", "Date of endpoint event", vt = "date_ymd",
        branching = "[event_occurred] = '1'"),
    fld("followup_days", "outcome", "text", "Days of follow-up", vt = "integer", vmin = "0", vmax = "400"),
    fld("ae_term", "adverse_events", "text", "Adverse event term"),
    fld("ae_grade", "adverse_events", "radio", "Grade",
        choices = "1, Mild | 2, Moderate | 3, Severe | 4, Life-threatening"),
    fld("ae_serious", "adverse_events", "yesno", "Serious adverse event?"),
    fld("cdmstat_open_queries", "data_management", "text", "Open data queries (written by REDCap CDMSTAT)",
        vt = "integer")
  )
  forms <- unique(md$form_name)

  events <- data.frame(event_name = c("Baseline", "Week 12", "Week 24"),
                       arm_num = "1", unique_event_name = c("baseline_arm_1", "week_12_arm_1", "week_24_arm_1"),
                       day_offset = c("0", "84", "168"), stringsAsFactors = FALSE)
  map <- list(baseline_arm_1 = c("enrollment", "vitals", "labs", "data_management"),
              week_12_arm_1 = c("vitals", "labs", "adverse_events", "data_management"),
              week_24_arm_1 = c("vitals", "labs", "outcome", "adverse_events", "data_management"))
  mapping <- do.call(rbind, lapply(names(map), function(ev) {
    data.frame(arm_num = "1", unique_event_name = ev, form = map[[ev]], stringsAsFactors = FALSE)
  }))
  repeating <- data.frame(event_name = c("week_12_arm_1", "week_24_arm_1"), form_name = "adverse_events",
                          custom_form_label = "[ae_term]", stringsAsFactors = FALSE)

  cols <- c("record_id", "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")
  for (f in forms) cols <- c(cols, setdiff(field_columns(md, md$field_name[md$form_name == f]), "record_id"),
                             paste0(f, "_complete"))
  rows <- list()
  blank <- stats::setNames(rep("", length(cols)), cols)
  fmt <- function(x, d = 1) formatC(x, format = "f", digits = d)

  for (id in seq_len(n)) {
    arm <- sample(1:2, 1)
    sex <- sample(1:2, 1)
    consent <- as.Date("2025-01-06") + sample(0:300, 1)
    dob <- as.Date("1950-01-01") + sample(0:(50 * 365), 1)
    height <- stats::rnorm(1, ifelse(sex == 1, 174, 162), 7)
    w0 <- stats::rnorm(1, ifelse(sex == 1, 78, 68), 11)
    sbp0 <- stats::rnorm(1, 132, 14)
    a1c0 <- exp(stats::rnorm(1, log(6.8), 0.18))
    dropout <- stats::runif(1) < 0.08
    for (k in seq_len(nrow(events))) {
      ev <- events$unique_event_name[k]
      if (k == 3 && dropout) next
      r <- blank
      r["record_id"] <- id
      r["redcap_event_name"] <- ev
      eff <- c(0, 1, 1.6)[k] * (arm == 1)
      if (k == 1) {
        r[c("consent_date", "dob", "sex", "site", "arm", "smoker")] <-
          c(format(consent), format(dob), sex, sample(1:4, 1, prob = c(.35, .3, .2, .15)), arm, stats::rbinom(1, 1, .2))
        r["age"] <- floor(abs(as.numeric(consent - dob)) / 365.2425)
        r["pregnant"] <- if (sex == 2) stats::rbinom(1, 1, .04) else ""
        race <- sample(1:4, 1, prob = c(.7, .1, .15, .05))
        r[paste0("race___", 1:4)] <- as.integer(1:4 == race)
        r["enrollment_complete"] <- "2"
      }
      w <- round(w0 - 2.5 * eff + stats::rnorm(1, 0, 1.5), 1)
      height <- round(height, 1)
      sbp <- sbp0 - 6 * eff + stats::rnorm(1, 0, 6)
      r[c("visit_date", "weight", "height", "bmi", "sbp", "dbp")] <-
        c(format(consent + as.integer(events$day_offset[k])), fmt(w), fmt(height),
          fmt(round(w / (height / 100)^2, 1)), round(sbp), round(sbp * 0.62 + stats::rnorm(1, 0, 5)))
      r["vitals_complete"] <- sample(c("2", "1"), 1, prob = c(.92, .08))
      r[c("hba1c", "creatinine")] <- c(fmt(a1c0 * exp(-0.06 * eff + stats::rnorm(1, 0, .05))),
                                       fmt(exp(stats::rnorm(1, log(80), .25)), 0))
      if (stats::runif(1) < .05) r["hba1c"] <- ""
      r["labs_complete"] <- "2"
      if (k == 3) {
        ev_yes <- stats::rbinom(1, 1, ifelse(arm == 1, .14, .30))
        days <- if (ev_yes == 1) sample(20:168, 1) else 168
        r[c("event_occurred", "event_date", "followup_days", "outcome_complete")] <-
          c(ev_yes, if (ev_yes == 1) format(consent + days) else "", days, "2")
      }
      r["data_management_complete"] <- "0"
      rows[[length(rows) + 1]] <- r
      if (k > 1) {
        for (inst in seq_len(stats::rpois(1, 0.45))) {
          a <- blank
          a[c("record_id", "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")] <-
            c(id, ev, "adverse_events", inst)
          a[c("ae_term", "ae_grade", "ae_serious", "adverse_events_complete")] <-
            c(sample(c("Headache", "Nausea", "Dizziness", "Rash", "Fatigue"), 1),
              sample(1:4, 1, prob = c(.55, .3, .12, .03)), stats::rbinom(1, 1, .08), "2")
          rows[[length(rows) + 1]] <- a
        }
      }
    }
  }
  rec <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  rec[] <- lapply(rec, as.character)

  plant <- function(id, ev, field, value) {
    i <- which(rec$record_id == id & rec$redcap_event_name == ev & rec$redcap_repeat_instrument == "")
    rec[i, field] <<- value
  }
  plant("7", "baseline_arm_1", "weight", "450")      # range error + outlier
  plant("15", "week_12_arm_1", "sbp", "300")         # range error
  plant("22", "baseline_arm_1", "sex", "1")          # male ...
  plant("22", "baseline_arm_1", "pregnant", "1")     # ... with pregnancy recorded (hidden field)
  plant("31", "baseline_arm_1", "bmi", "45.0")       # stale calculated field
  plant("40", "baseline_arm_1", "site", "")          # required field missing
  plant("55", "week_12_arm_1", "hba1c", "19.5")      # extreme but in range
  plant("63", "baseline_arm_1", "sbp", "90")         # diastolic above systolic
  plant("63", "baseline_arm_1", "dbp", "110")
  plant("77", "week_24_arm_1", "height", "17.2")     # unit error

  # Verbatim adverse-event terms as sites type them, for the coding module.
  ae <- which(rec$redcap_repeat_instrument == "adverse_events")
  variants <- c(Headache = "severe headache", Nausea = "feeling sick", Dizziness = "Dizzyness",
                Rash = "skin rash", Fatigue = "tiredness")
  pick <- ae[seq(2, length(ae), by = 4)]
  rec$ae_term[pick] <- unname(variants[rec$ae_term[pick]])
  # Two late visits, for visit-window deviation detection.
  late <- function(id, ev, days) {
    i <- which(rec$record_id == id & rec$redcap_event_name == ev & rec$redcap_repeat_instrument == "")
    if (length(i)) rec$visit_date[i] <<- format(as.Date(rec$visit_date[i]) + days)
  }
  late("5", "week_12_arm_1", 15)
  late("18", "week_12_arm_1", 40)

  start <- format(Sys.time() - 86400 * 30, "%Y-%m-%d %H:%M")
  log <- data.frame(timestamp = start, username = "site_coordinator",
                    action = paste("Create record", unique(rec$record_id)), details = "",
                    stringsAsFactors = FALSE)

  list(
    info = list(project_id = "9999", project_title = "CDMSTAT-DEMO: Drug A vs Placebo (simulated)",
                is_longitudinal = "1", has_repeating_instruments_or_events = "1"),
    metadata = md, events = events, mapping = mapping, repeating = repeating,
    records = rec, log = log
  )
}
