# A small REDCap API stand-in, run in a background R process, serving the
# demo project. It implements the calls CDMSTAT makes (version, project,
# metadata, event, formEventMapping, repeatingFormsEvents, record export with
# records/fields filters, record import, log) and rejects wrong tokens the way
# REDCap does. `mock_edit` (not a REDCap call) simulates a user saving a form.
mock_token <- "0123456789ABCDEF0123456789ABCDEF"

start_mock_redcap <- function(log_access = TRUE) {
  skip_if_not_installed("callr")
  skip_if_not_installed("httpuv")
  skip_if_not_installed("shiny")
  port <- httpuv::randomPort()
  srv <- callr::r_bg(function(port, token, log_access) {
    st <- REDCapCDMSTAT::demo_project()
    json <- function(x, status = 200L) {
      list(status = status, headers = list("Content-Type" = "application/json"),
           body = as.character(jsonlite::toJSON(x, dataframe = "rows", auto_unbox = TRUE, na = "string")))
    }
    log_row <- function(action) {
      st$log <<- rbind(st$log, data.frame(timestamp = format(Sys.time(), "%Y-%m-%d %H:%M"), username = "site_user",
                                          action = action, details = "", stringsAsFactors = FALSE))
    }
    arr <- function(f, nm) unname(unlist(f[grep(paste0("^", nm, "\\["), names(f))]))
    httpuv::runServer("127.0.0.1", port, list(call = function(req) {
      f <- shiny::parseQueryString(rawToChar(req$rook.input$read()))
      if (!identical(f$token, token)) return(json(list(error = "You do not have permissions to use the API"), 403L))
      switch(f$content %||% "",
        version = list(status = 200L, headers = list("Content-Type" = "text/html"), body = "14.5.2"),
        project = json(c(st$info, in_production = "0")),
        metadata = json(st$metadata),
        event = json(st$events),
        formEventMapping = json(st$mapping),
        repeatingFormsEvents = json(st$repeating),
        log = if (log_access) {
          l <- st$log
          if (!is.null(f$beginTime)) l <- l[as.POSIXct(l$timestamp, format = "%Y-%m-%d %H:%M") >= as.POSIXct(f$beginTime, format = "%Y-%m-%d %H:%M"), ]
          json(l)
        } else json(list(error = "You do not have Logging privileges"), 403L),
        record = {
          if (!is.null(f$data)) {
            d <- jsonlite::fromJSON(f$data)
            n <- 0
            for (i in seq_len(nrow(d))) {
              idx <- which(st$records$record_id == d$record_id[i] & st$records$redcap_event_name == d$redcap_event_name[i] &
                             st$records$redcap_repeat_instrument == "")
              for (cc in setdiff(names(d), c("record_id", "redcap_event_name"))) if (nzchar(d[[cc]][i])) st$records[idx, cc] <<- d[[cc]][i]
              log_row(paste("Update record", d$record_id[i]))
              n <- n + 1
            }
            json(list(count = n))
          } else {
            r <- st$records
            ids <- arr(f, "records")
            if (length(ids)) r <- r[r$record_id %in% ids, , drop = FALSE]
            flds <- arr(f, "fields")
            if (length(flds)) r <- r[, intersect(c("record_id", "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance", flds), names(r)), drop = FALSE]
            json(r)
          }
        },
        mock_edit = {
          idx <- which(st$records$record_id == f$record & st$records$redcap_event_name == f$event & st$records$redcap_repeat_instrument == "")
          st$records[idx, f$field] <<- f$value
          log_row(sprintf("Update record %s (%s)", f$record, f$event))
          json(list(ok = TRUE))
        },
        externalModule = json(list(error = "The module is not enabled in this project"), 400L),
        json(list(error = paste("Unsupported content:", f$content)), 400L))
    }))
  }, args = list(port = port, token = mock_token, log_access = log_access))
  url <- sprintf("http://127.0.0.1:%d/api/", port)
  for (i in 1:75) {
    up <- tryCatch({
      httr2::req_perform(httr2::req_error(httr2::request(url), is_error = function(r) FALSE))
      TRUE
    }, error = function(e) FALSE)
    if (up) break
    Sys.sleep(0.2)
  }
  list(url = url, process = srv,
       edit = function(record, event, field, value) {
         redcap_call(url, mock_token, "mock_edit", record = record, event = event, field = field, value = value)
       })
}
