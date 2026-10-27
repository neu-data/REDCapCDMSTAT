edc_demo <- function(...) {
  nx <- cdmstat_demo(dir = tempfile("edc"), actor = "dana")
  nx$drw_enable(...)
  nx
}

test_that("CDMSTAT locks are enforced inside REDCap", {
  nx <- edc_demo()
  lk <- nx$lock("10", "Database lock for interim")
  locks <- nx$backend$drw("list-locks", list(record = "10"))$locks
  expect_true(all(c("enrollment", "vitals", "labs") %in% locks$form_name))
  expect_error(nx$backend$edit("10", "weight", "99"), "locked in REDCap")
  nx$unlock(lk, "Correction approved by sponsor")
  expect_equal(nrow(nx$backend$drw("list-locks", list(record = "10"))$locks), 0)
  expect_silent(nx$backend$edit("10", "weight", "71.0"))
})

test_that("form-level locks lock only that form", {
  nx <- edc_demo()
  nx$lock("11", "Labs reviewed", event = "week_12_arm_1", form = "labs")
  l <- nx$backend$drw("list-locks", list(record = "11"))$locks
  expect_equal(nrow(l), 1)
  expect_equal(l$form_name, "labs")
  expect_error(nx$backend$edit("11", "hba1c", "6", event = "week_12_arm_1"), "locked")
  expect_silent(nx$backend$edit("11", "sbp", "120", event = "week_12_arm_1"))
})

test_that("SDV verification is mirrored to DRW field verification", {
  nx <- edc_demo()
  nx$sdv_plan(critical_fields = "arm", first_n = 1, sample_rate = 0)
  nx$sdv_verify("1|baseline_arm_1|vitals|")
  d <- nx$backend$state()$drw$status
  v <- d[d$record == "1" & d$event == "baseline_arm_1", ]
  expect_true(all(c("weight", "height", "sbp", "dbp") %in% v$field_name))
  expect_true(all(v$query_status == "VERIFIED"))
})

test_that("EDC mirroring can be switched off", {
  nx <- edc_demo(verify_sdv = FALSE, lock_in_edc = FALSE)
  nx$lock("12", "lock")
  expect_equal(nrow(nx$backend$drw("list-locks", list())$locks %||% data.frame()), 0)
  nx$sdv_plan(critical_fields = "arm", first_n = 1, sample_rate = 0)
  nx$sdv_verify("1|baseline_arm_1|vitals|")
  expect_equal(nrow(nx$backend$state()$drw$status %||% data.frame()), 0)
})

test_that("per-session clones keep their own analysis cut", {
  nx <- cdmstat_demo(dir = tempfile("clone"))
  a <- nx$clone()
  id <- nx$freeze("x")
  a$use_cut(id)
  expect_equal(a$active_cut()$id, id)
  expect_null(nx$active_cut())
})

test_that("mixed models fit repeated measures on long data", {
  nx <- cdmstat_demo(dir = tempfile("mm"))
  f <- nx$fit("sbp ~ arm * redcap_event_name + (1 | record_id)", "mixed")
  expect_equal(unname(attr(f, "groups")[1]), 120)
  expect_gt(attr(f, "n"), 300)
  expect_true("armPlacebo:redcap_event_nameweek_24_arm_1" %in% f$term)
  expect_error(nx$fit("sbp ~ arm", "mixed"), "random effect")
})

test_that("SDTM includes SV, DS and a define.xml", {
  nx <- cdmstat_demo(dir = tempfile("sd"))
  d <- nx$sdtm()
  expect_equal(nrow(d$DS), 120)
  expect_true(all(d$DS$DSDECOD %in% c("COMPLETED", "")))
  expect_true(nrow(d$SV) > 300)
  f <- nx$export_cdisc(tempfile("cd"))
  x <- xml2::read_xml(f[grepl("define.xml$", f)])
  xml2::xml_ns_strip(x)
  expect_equal(length(xml2::xml_find_all(x, "//ItemGroupDef")), 6)
})

test_that("portfolio summarises several projects", {
  a <- cdmstat_demo(dir = tempfile("pa"))
  b <- cdmstat_demo(dir = tempfile("pb"))
  a$raise_queries()
  pf <- cdmstat_portfolio(list(trial_a = a, trial_b = b$dir))
  expect_equal(pf$studies$project, c("trial_a", "trial_b"))
  expect_equal(pf$studies$open_queries, c(9, 0))
  expect_true(all(c("trial_a", "trial_b") %in% pf$sites$project))
})

test_that("models and Table 1 format as gtsummary tables", {
  skip_if_not_installed("gtsummary")
  nx <- cdmstat_demo(dir = tempfile("gt"))
  t1 <- nx$table_one_gt()
  expect_s3_class(t1, "gtsummary")
  expect_true("p.value" %in% names(t1$table_body))
  lm_tbl <- tbl_fit(nx$fit("sbp__week_24 ~ arm + sbp__baseline"))
  expect_s3_class(lm_tbl, "gtsummary")
  expect_true("Randomised treatment arm" %in% lm_tbl$table_body$label)
  cox_tbl <- tbl_fit(nx$fit("Surv(followup_days, event_occurred == 'Yes') ~ arm", "cox"))
  expect_true(cox_tbl$inputs$exponentiate)
  skip_if_not_installed("broom.mixed")
  mm_tbl <- tbl_fit(nx$fit("sbp ~ arm * redcap_event_name + (1 | record_id)", "mixed"))
  expect_true(all(c("Visit", "Week 24") %in% mm_tbl$table_body$label))
  expect_false(any(is.na(mm_tbl$table_body$p.value[mm_tbl$table_body$row_type == "level" & !mm_tbl$table_body$reference_row %in% TRUE])))
})
