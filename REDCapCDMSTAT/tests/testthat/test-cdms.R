demo <- function() cdmstat_demo(dir = tempfile("cdms"), actor = "dana")

test_that("ODM export re-imports losslessly and syncs incrementally", {
  nx <- demo()
  f <- tempfile(fileext = ".xml")
  write_odm(nx$backend$state(), f)
  o <- read_odm(paste(readLines(f), collapse = "\n"))
  expect_equal(nrow(o$records), nrow(nx$raw))
  expect_equal(o$metadata$branching_logic[o$metadata$field_name == "pregnant"], "[sex] = '2'")
  expect_equal(o$metadata$field_type[o$metadata$field_name == "bmi"], "calc")
  expect_equal(o$repeating$form_name[1], "adverse_events")
  ny <- cdmstat_odm(f, dir = tempfile("odm"))
  expect_true(any(ny$validate()$rule == "range:weight"))
  x <- readLines(f)
  i <- grep('I.weight" Value="450"', x)
  x[i] <- sub("450", "81.0", x[i])
  writeLines(x, f)
  s <- ny$sync()
  expect_equal(s$changed, 1)
  expect_false(any(ny$validate()$rule == "range:weight"))
})

test_that("generic ODM from another EDC maps types, code lists, ranges and repeats", {
  xml <- '<?xml version="1.0"?><ODM xmlns="http://www.cdisc.org/ns/odm/v1.3" ODMVersion="1.3.2">
  <Study OID="S1"><GlobalVariables><StudyName>Other EDC</StudyName></GlobalVariables>
  <MetaDataVersion OID="M1" Name="v1"><Protocol><StudyEventRef StudyEventOID="SE.SCR" OrderNumber="1"/></Protocol>
  <StudyEventDef OID="SE.SCR" Name="Screening" Repeating="No" Type="Scheduled"><FormRef FormOID="F.DM"/><FormRef FormOID="F.AE"/></StudyEventDef>
  <FormDef OID="F.DM" Name="Demographics"><ItemGroupRef ItemGroupOID="IG.DM"/></FormDef>
  <FormDef OID="F.AE" Name="Adverse Events" Repeating="Yes"><ItemGroupRef ItemGroupOID="IG.AE"/></FormDef>
  <ItemGroupDef OID="IG.DM" Name="DM"><ItemRef ItemOID="I.AGE" Mandatory="Yes"/><ItemRef ItemOID="I.SEX"/></ItemGroupDef>
  <ItemGroupDef OID="IG.AE" Name="AE" Repeating="Yes"><ItemRef ItemOID="I.AETERM"/></ItemGroupDef>
  <ItemDef OID="I.AGE" Name="AGE" DataType="integer"><Question><TranslatedText>Age</TranslatedText></Question>
    <RangeCheck Comparator="GE" SoftHard="Soft"><CheckValue>18</CheckValue></RangeCheck></ItemDef>
  <ItemDef OID="I.SEX" Name="SEX" DataType="text"><CodeListRef CodeListOID="CL.SEX"/></ItemDef>
  <ItemDef OID="I.AETERM" Name="AETERM" DataType="text"/>
  <CodeList OID="CL.SEX" Name="SEX" DataType="text"><CodeListItem CodedValue="M"><Decode><TranslatedText>Male</TranslatedText></Decode></CodeListItem>
    <CodeListItem CodedValue="F"><Decode><TranslatedText>Female</TranslatedText></Decode></CodeListItem></CodeList>
  </MetaDataVersion></Study>
  <ClinicalData StudyOID="S1" MetaDataVersionOID="M1">
   <SubjectData SubjectKey="001"><StudyEventData StudyEventOID="SE.SCR">
     <FormData FormOID="F.DM"><ItemGroupData ItemGroupOID="IG.DM"><ItemData ItemOID="I.AGE" Value="16"/><ItemData ItemOID="I.SEX" Value="F"/></ItemGroupData></FormData>
     <FormData FormOID="F.AE" FormRepeatKey="1"><ItemGroupData ItemGroupOID="IG.AE"><ItemData ItemOID="I.AETERM" Value="Headache"/></ItemGroupData></FormData>
     <FormData FormOID="F.AE" FormRepeatKey="2"><ItemGroupData ItemGroupOID="IG.AE"><ItemData ItemOID="I.AETERM" Value="Nausea"/></ItemGroupData></FormData>
   </StudyEventData></SubjectData></ClinicalData></ODM>'
  o <- read_odm(xml)
  md <- o$metadata
  expect_equal(md$field_type[md$field_name == "sex"], "radio")
  expect_equal(md$select_choices_or_calculations[md$field_name == "sex"], "M, Male | F, Female")
  expect_equal(md$text_validation_min[md$field_name == "age"], "18")
  expect_equal(md$required_field[md$field_name == "age"], "y")
  expect_equal(sum(o$records$redcap_repeat_instrument == "adverse_events"), 2)
  f <- tempfile(fileext = ".xml")
  writeLines(xml, f)
  nx <- cdmstat_odm(f, dir = tempfile("odm2"))
  expect_true(any(nx$validate()$rule == "range:age"))
})

test_that("REDCap Cloud backend authenticates and pulls ODM over REST", {
  skip_if_not_installed("callr")
  skip_if_not_installed("httpuv")
  src <- demo()
  f <- tempfile(fileext = ".xml")
  write_odm(src$backend$state(), f)
  port <- httpuv::randomPort()
  srv <- callr::r_bg(function(port, f) {
    httpuv::runServer("127.0.0.1", port, list(call = function(req) {
      ok <- identical(req$HTTP_AUTHORIZATION, "Bearer cloud-test-token") && req$PATH_INFO == "/api/v2/studies/TRIAL-7/odm"
      if (!ok) return(list(status = 401L, headers = list("Content-Type" = "text/plain"), body = "unauthorized"))
      list(status = 200L, headers = list("Content-Type" = "application/xml"), body = paste(readLines(f), collapse = "\n"))
    }))
  }, args = list(port = port, f = f))
  on.exit(srv$kill(), add = TRUE)
  for (i in 1:50) { ok <- tryCatch({ httr2::req_perform(httr2::request(sprintf("http://127.0.0.1:%d/", port)) |> httr2::req_error(is_error = function(r) FALSE)); TRUE }, error = function(e) FALSE); if (ok) break; Sys.sleep(0.2) }
  Sys.setenv(RCC_TEST_TOKEN = "cloud-test-token")
  nx <- cdmstat_cloud(sprintf("http://127.0.0.1:%d", port), "TRIAL-7", token_env = "RCC_TEST_TOKEN",
                      export_path = "/api/v2/studies/{study}/odm", dir = tempfile("cloud"))
  expect_equal(nx$summary()$source, "REDCap Cloud API (ODM)")
  expect_equal(nx$summary()$records, 120)
  expect_true(nrow(nx$table_one()) > 10)
  expect_no_match(nx$script(), "cloud-test-token")
  Sys.setenv(RCC_TEST_TOKEN = "wrong")
  expect_error(cdmstat_cloud(sprintf("http://127.0.0.1:%d", port), "TRIAL-7", token_env = "RCC_TEST_TOKEN",
                             export_path = "/api/v2/studies/{study}/odm", dir = tempfile("cloud")), "401")
})

test_that("stratified block randomisation allocates once, balanced, and writes back", {
  nx <- demo()
  nx$setup_randomization(strata = c("site", "sex"), block_sizes = 4, per_stratum = 40, seed = 7)
  for (i in 121:128) {
    nx$backend$add_record(as.character(i), list(consent_date = "2025-12-01", dob = "1980-01-01", sex = "1", site = "2",
                                                enrollment_complete = "2"))
  }
  nx$sync()
  arms <- vapply(as.character(121:128), function(r) nx$randomize(r)$arm, "")
  expect_equal(as.vector(table(arms)), c(4, 4))   # two complete blocks of 4
  expect_error(nx$randomize("121"), "already randomised")
  expect_error(nx$randomize("5"), "already has an arm")
  d <- nx$data()
  expect_true(all(!is.na(d$arm[d$record_id %in% as.character(121:128) & d$redcap_event_name == "baseline_arm_1"])))
  expect_error(nx$setup_randomization(strata = "site"), "cannot be replaced")
  nx$backend$add_record("129", list(consent_date = "2025-12-01", sex = "1"))
  nx$sync()
  expect_error(nx$randomize("129"), "Missing stratification")
})

test_that("SDV plan, verification and re-verification after edits", {
  nx <- demo()
  s <- nx$sdv_plan(critical_fields = "arm", first_n = 2, sample_rate = 0)
  expect_true(all(s$required[s$form == "enrollment"] == 1))
  k <- "1|baseline_arm_1|vitals|"
  nx$sdv_verify(k)
  expect_equal(nx$sdv_status()$status[nx$sdv_status()$unit_key == k], "verified")
  nx$backend$edit("1", "sbp", "101")
  nx$sync()
  expect_equal(nx$sdv_status()$status[nx$sdv_status()$unit_key == k], "reverify")
})

test_that("locks and signatures detect later changes", {
  nx <- demo()
  lk <- nx$lock("10", "Interim lock")
  expect_error(nx$esign("10", "approve"), "re-verified")
  nx$esign("10", "I approve", identity_verified = TRUE)
  expect_equal(nx$locks()$integrity, "intact")
  expect_equal(nx$signatures()$status, "valid")
  nx$backend$edit("10", "weight", "70.1")
  nx$sync()
  expect_equal(nx$locks()$integrity, "CHANGED AFTER LOCK")
  expect_match(nx$signatures()$status, "INVALID")
  expect_true(any(startsWith(nx$validate()$rule, "lock:")))
  expect_error(nx$unlock(lk, ""), "reason")
  nx$unlock(lk, "Correction approved by sponsor")
  expect_equal(nrow(nx$locks()), 0)
})

test_that("medical coding auto-codes exact matches and queues fuzzy ones", {
  nx <- demo()
  r <- nx$code_terms("ae_term")
  expect_gt(r$auto_coded, 0)
  expect_gt(r$for_review, 0)
  c1 <- nx$coding()
  expect_equal(unique(c1$term[c1$verbatim == "feeling sick"]), "Nausea")   # synonym
  p <- c1[c1$verbatim == "Dizzyness", ][1, ]
  expect_equal(p$status, "pending")
  expect_equal(p$suggestion_term, "Dizziness")
  nx$approve_coding(p$coding_key)
  expect_equal(nx$coding()$status[nx$coding()$coding_key == p$coding_key], "approved")
  expect_error(nx$approve_coding(p$coding_key, code = "NOPE"), "not in the dictionary")
  r2 <- nx$code_terms("ae_term")
  expect_equal(r2$auto_coded + r2$for_review, 0)   # nothing changed, nothing recoded
})

test_that("visit-window deviations are detected once and can be closed", {
  nx <- demo()
  expect_equal(nx$check_visit_windows(window = 7), 2L)
  expect_equal(nx$check_visit_windows(window = 7), 0L)
  d <- nx$deviations()
  expect_setequal(d$record_id, c("5", "18"))
  expect_equal(d$severity[d$record_id == "18"], "major")
  expect_error(nx$close_deviation(d$dev_id[1], ""), "comment")
  nx$close_deviation(d$dev_id[1], "Discussed with site")
  expect_equal(sum(nx$deviations()$status == "closed"), 1)
})

test_that("site metrics and SDTM export", {
  nx <- demo()
  nx$raise_queries()
  m <- nx$site_metrics()
  expect_true(all(c("Kampala", "Nairobi") %in% m$site))
  expect_equal(sum(m$subjects), 120)
  nx$code_terms("ae_term")
  sd <- nx$sdtm()
  expect_equal(nrow(sd$DM), 120)
  expect_true(all(sd$DM$SEX %in% c("M", "F")))
  expect_true(all(c("WEIGHT", "SYSBP") %in% sd$VS$VSTESTCD))
  expect_equal(sd$AE$AEDECOD[sd$AE$AETERM == "feeling sick"][1], "Nausea")
  files <- nx$export_cdisc(tempfile("cdisc"))
  expect_true(any(grepl("dm.csv$", files)) && any(grepl("study.odm.xml$", files)))
})
