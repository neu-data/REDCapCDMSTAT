drw_demo <- function() {
  nx <- cdmstat_demo(dir = tempfile("drw"), actor = "dana")
  nx$drw_enable()
  nx
}
thread_of <- function(nx, qid) {
  sid <- nx$drw_links()$status_id[nx$drw_links()$query_id == qid]
  d <- nx$backend$state()$drw
  list(status = d$status[d$status$status_id == sid, ], res = d$res[d$res$status_id == sid, ])
}

test_that("enabling checks the module and the DRW", {
  nx <- cdmstat_demo(dir = tempfile("drw"))
  expect_error(nx$drw_sync(), "not enabled")
  s <- nx$drw_enable()
  expect_true(s$drw_enabled)
  expect_true(isTRUE(nx$ledger$get_setting("drw")$enabled))
})

test_that("new queries are opened in the DRW once, with attribution", {
  nx <- drw_demo()
  nx$raise_queries()
  r <- nx$drw_sync()
  expect_gt(r$opened, 0)
  expect_length(r$errors, 0)
  q <- nx$queries()
  qid <- q$query_id[q$rule == "range:weight" & q$record_id == "7"]
  t <- thread_of(nx, qid)
  expect_equal(t$status$query_status, "OPEN")
  expect_equal(t$status$field_name, "weight")
  expect_equal(t$status$event, "baseline_arm_1")
  expect_match(t$res$comment[1], "^\\[CDMSTAT: dana\\] Value outside allowed range")
  r2 <- nx$drw_sync()
  expect_equal(r2$opened + r2$updated + r2$pulled + r2$imported, 0)   # idempotent
})

test_that("site responses, closures and reopenings in REDCap flow back", {
  nx <- drw_demo()
  nx$raise_queries()
  nx$drw_sync()
  q <- nx$queries()
  qid <- q$query_id[q$rule == "range:sbp" & q$record_id == "15"]
  sid <- nx$drw_links()$status_id[nx$drw_links()$query_id == qid]
  nx$backend$drw("respond", list(status_id = sid, response = "CONFIRMED_CORRECT", comment = "Value checked against source"),
                 user = "site_crc")
  r <- nx$drw_sync()
  expect_equal(r$pulled, 1)
  expect_equal(nx$queries()$status[nx$queries()$query_id == qid], "answered")
  a <- nx$query_audit(qid)
  expect_equal(utils::tail(a$actor, 1), "redcap:site_crc")
  expect_match(utils::tail(a$comment, 1), "CONFIRMED_CORRECT")
  nx$backend$drw("close-query", list(status_id = sid, comment = "Closed by DM in REDCap"), user = "dm_in_redcap")
  nx$drw_sync()
  expect_equal(nx$queries()$status[nx$queries()$query_id == qid], "resolved")
  nx$backend$drw("reopen-query", list(status_id = sid, comment = "Still wrong"), user = "dm_in_redcap")
  nx$drw_sync()
  expect_equal(nx$queries()$status[nx$queries()$query_id == qid], "open")
  expect_equal(nx$drw_sync()$pulled, 0)   # nothing applied twice
})

test_that("CDMSTAT transitions are mirrored to the DRW", {
  nx <- drw_demo()
  id <- nx$add_query("10", "weight", "Please confirm weight", event = "baseline_arm_1", source = "analysis", actor = "sam")
  nx$drw_sync()
  nx$transition_query(id, "answered", "Site confirmed by phone")
  nx$drw_sync()
  t <- thread_of(nx, id)
  expect_equal(utils::tail(t$res$response, 1), "OTHER")
  nx$transition_query(id, "resolved", "Verified")
  nx$drw_sync()
  expect_equal(thread_of(nx, id)$status$query_status, "CLOSED")
  nx$transition_query(id, "open", "Reopened after review")
  nx$drw_sync()
  expect_equal(thread_of(nx, id)$status$query_status, "OPEN")
})

test_that("queries opened directly in REDCap are imported", {
  nx <- drw_demo()
  nx$drw_sync()
  o <- nx$backend$drw("open-query", list(record = "3", field = "height", event = "week_12_arm_1",
                                         comment = "Height differs from baseline"), user = "monitor_in_redcap")
  r <- nx$drw_sync()
  expect_equal(r$imported, 1)
  q <- nx$queries()
  imp <- q[q$source == "redcap-drw", ]
  expect_equal(imp$record_id, "3")
  expect_equal(imp$created_by, "redcap:monitor_in_redcap")
  nx$transition_query(imp$query_id, "resolved", "Corrected")
  nx$drw_sync()
  expect_equal(nx$backend$state()$drw$status$query_status[nx$backend$state()$drw$status$status_id == o$status_id], "CLOSED")
})

test_that("fixing data auto-resolves and closes the DRW query on sync", {
  nx <- drw_demo()
  nx$raise_queries()
  nx$drw_sync()
  q <- nx$queries()
  qid <- q$query_id[q$rule == "range:weight" & q$record_id == "7"]
  nx$backend$edit("7", "weight", "75.0")
  nx$sync()          # sync triggers the DRW push automatically
  nx$raise_queries() # auto-resolves the fixed rule query
  nx$drw_sync()
  expect_equal(thread_of(nx, qid)$status$query_status, "CLOSED")
})

test_that("non-field queries are skipped, not retried", {
  nx <- drw_demo()
  nx$lock("10", "lock")
  nx$backend$edit("10", "weight", "70.2")
  nx$sync()
  nx$raise_queries()
  r <- nx$drw_sync()
  links <- nx$drw_links()
  expect_true(any(grepl("^skipped", links$pushed_status)))
  expect_equal(nx$drw_sync()$skipped, 0)
})

test_that("the REDCap backend calls the module through the API", {
  skip_if_not_installed("callr")
  skip_if_not_installed("httpuv")
  port <- httpuv::randomPort()
  srv <- callr::r_bg(function(port) {
    httpuv::runServer("127.0.0.1", port, list(call = function(req) {
      body <- rawToChar(req$rook.input$read())
      f <- shiny::parseQueryString(body)
      ok <- identical(f$token, "TOK") && identical(f$content, "externalModule") && identical(f$prefix, "cdmstat_drw")
      out <- if (!ok) list(error = "bad request") else if (f$action == "open-query")
        list(status_id = 12, res_id = 99, query_status = "OPEN", already_open = FALSE, echo = f) else list(echo = f)
      list(status = if (ok) 200L else 400L, headers = list("Content-Type" = "application/json"),
           body = jsonlite::toJSON(out, auto_unbox = TRUE))
    }))
  }, args = list(port = port))
  on.exit(srv$kill(), add = TRUE)
  for (i in 1:50) { ok <- tryCatch({ httr2::req_perform(httr2::request(sprintf("http://127.0.0.1:%d/", port)) |> httr2::req_error(is_error = function(r) FALSE)); TRUE }, error = function(e) FALSE); if (ok) break; Sys.sleep(0.2) }
  b <- RedcapBackend$new(sprintf("http://127.0.0.1:%d/", port), "TOK")
  r <- b$drw("open-query", list(record = "7", field = "weight", event = "baseline_arm_1", comment = "Check", instance = NA))
  expect_equal(r$status_id, 12)
  expect_equal(r$echo$action, "open-query")
  expect_equal(r$echo$record, "7")
  expect_equal(r$echo$returnFormat, "json")
  expect_null(r$echo$instance)
})
