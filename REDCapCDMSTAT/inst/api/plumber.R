# REDCap CDMSTAT REST API. Run with REDCapCDMSTAT::run_api() and keep it on
# localhost behind the FastAPI gateway (web/gateway), which handles user
# authentication, WebSocket notifications and the public DET endpoint.
#
# When CDMSTAT_GATEWAY_SECRET is set, every request must carry it in
# X-CDMSTAT-Secret, and the gateway's X-CDMSTAT-User / X-CDMSTAT-Role headers are
# trusted for the audit trail and access control.

library(REDCapCDMSTAT)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

nx <- getOption("REDCapCDMSTAT.instance") %||% cdmstat_from_env()

# Role -> allowed actions. Data managers own the query lifecycle and data
# cuts; statisticians can raise analysis queries and comment; viewers read.
acl <- list(
  dm = c("read", "query_write", "query_raise", "comment", "sync", "cut_write", "rule_write", "writeback",
         "randomize", "rand_setup", "lock", "coding", "deviation", "sdv_plan"),
  stats = c("read", "query_raise", "comment", "sync"),
  monitor = c("read", "query_raise", "comment", "sdv", "deviation"),
  pi = c("read", "comment", "esign", "randomize"),
  viewer = c("read")
)

need <- function(req, res, action) {
  role <- req$cdmstat_role %||% "viewer"
  if (!action %in% acl[[role]]) {
    res$status <- 403
    stop(sprintf("Role '%s' may not perform '%s'.", role, action), call. = FALSE)
  }
}

df_out <- function(d) {
  d <- as.data.frame(d)
  d[] <- lapply(d, function(v) if (is.factor(v) || inherits(v, "Date")) as.character(v) else v)
  d
}

#* @apiTitle REDCap CDMSTAT API
#* @apiDescription Data management and statistical analysis engine for REDCap projects.

#* @filter auth
function(req, res) {
  secret <- Sys.getenv("CDMSTAT_GATEWAY_SECRET")
  req$cdmstat_user <- "api"
  req$cdmstat_role <- "dm"
  req$cdmstat_reauth <- FALSE
  if (nzchar(secret) && !identical(req$PATH_INFO, "/health")) {
    if (!identical(req$HTTP_X_CDMSTAT_SECRET, secret)) {
      res$status <- 401
      return(list(error = "unauthorized"))
    }
    req$cdmstat_user <- req$HTTP_X_CDMSTAT_USER %||% "unknown"
    req$cdmstat_reauth <- identical(req$HTTP_X_CDMSTAT_REAUTH, "1")
    req$cdmstat_role <- req$HTTP_X_CDMSTAT_ROLE %||% "viewer"
  }
  nx$reload_if_changed()
  # plumber handles one request at a time, so the actor can be set per request
  # and lands in the script log and audit trail.
  nx$actor <- req$cdmstat_user
  plumber::forward()
}

#* @get /health
function() list(status = "ok", time = format(Sys.time()))

#* Project summary, counts and active cut
#* @get /summary
function(req, res) {
  need(req, res, "read")
  s <- nx$summary()
  s$issues <- nrow(nx$validate())
  s$arm <- nx$arm()
  s$events <- df_out(nx$events)
  s$role <- req$cdmstat_role
  s$user <- req$cdmstat_user
  s
}

#* Data dictionary (live)
#* @get /metadata
function(req, res) {
  need(req, res, "read")
  df_out(nx$metadata)
}

#* Incremental (or full) sync from REDCap
#* @post /sync
function(req, res, full = FALSE) {
  need(req, res, "sync")
  nx$sync(full = isTRUE(as.logical(full)))
}

#* REDCap Data Entry Trigger receiver (form-encoded POST from REDCap)
#* @post /det
function(req, res, record = NULL, redcap_event_name = NULL, instrument = NULL, project_id = NULL) {
  need(req, res, "sync")
  if (is.null(record)) { res$status <- 400; return(list(error = "record is required")) }
  r <- nx$on_det(record, redcap_event_name, instrument)
  # A saved form may fix or create rule violations: keep the ledger current.
  q <- nx$raise_queries("system")
  c(r, q)
}

#* Current rule violations
#* @get /issues
function(req, res) {
  need(req, res, "read")
  nx$validate()
}

#* Custom rules
#* @get /rules
function(req, res) {
  need(req, res, "read")
  unname(nx$rules)
}

#* Add a custom rule (REDCap logic that must be TRUE)
#* @post /rules
function(req, res, id, logic, message, severity = "warning") {
  need(req, res, "rule_write")
  nx$add_rule(id, logic, message, severity)
  unname(nx$rules)
}

#* Turn new violations into queries and auto-resolve fixed ones
#* @post /queries/raise-from-rules
function(req, res) {
  need(req, res, "query_write")
  nx$raise_queries(req$cdmstat_user)
}

#* List queries
#* @get /queries
function(req, res, status = NULL) {
  need(req, res, "read")
  st <- if (is.null(status) || !nzchar(status)) NULL else strsplit(status, ",")[[1]]
  nx$queries(st)
}

#* Raise a query (e.g. from an outlier on a plot)
#* @post /queries
function(req, res, record_id, field, message, event = NA, instance = NA, value = NA, source = "analysis") {
  need(req, res, "query_raise")
  list(query_id = nx$add_query(record_id, field, message, event = event, instance = instance, value = value,
                               source = source, actor = req$cdmstat_user))
}

#* Move a query through its lifecycle (comment = reason for change)
#* @post /queries/<id>/transition
function(req, res, id, status, comment) {
  need(req, res, "query_write")
  tryCatch(list(query_id = id, status = nx$transition_query(id, status, comment, req$cdmstat_user)),
           error = function(e) { res$status <- 409; list(error = conditionMessage(e)) })
}

#* Comment on a query
#* @post /queries/<id>/comment
function(req, res, id, comment) {
  need(req, res, "comment")
  nx$comment_query(id, comment, req$cdmstat_user)
  list(ok = TRUE)
}

#* Audit trail for a query
#* @get /queries/<id>/audit
function(req, res, id) {
  need(req, res, "read")
  nx$query_audit(id)
}

#* REDCap Data Resolution Workflow module status and link state
#* @get /drw/status
function(req, res) {
  need(req, res, "read")
  cfg <- nx$ledger$get_setting("drw", list())
  links <- nx$drw_links()
  module <- tryCatch(nx$drw_status(), error = function(e) list(error = conditionMessage(e)))
  list(enabled = isTRUE(cfg$enabled), prefix = cfg$prefix %||% "cdmstat_drw", linked = sum(!is.na(links$status_id)),
       skipped = sum(grepl("^skipped", links$pushed_status)), module = module)
}

#* Enable DRW write-back (checks module, DRW and rights)
#* @post /drw/enable
function(req, res, prefix = "cdmstat_drw") {
  need(req, res, "query_write")
  tryCatch(nx$drw_enable(prefix, req$cdmstat_user), error = function(e) { res$status <- 409; list(error = conditionMessage(e)) })
}

#* Push ledger changes to the REDCap DRW and pull site activity
#* @post /drw/sync
function(req, res) {
  need(req, res, "query_write")
  tryCatch({ r <- nx$drw_sync(req$cdmstat_user); r$errors <- as.list(r$errors); r },
           error = function(e) { res$status <- 409; list(error = conditionMessage(e)) })
}

#* Query <-> DRW thread links
#* @get /drw/links
function(req, res) {
  need(req, res, "read")
  nx$drw_links()
}

#* Write open-query counts into the REDCap status field
#* @post /writeback
function(req, res, field = "cdmstat_open_queries") {
  need(req, res, "writeback")
  list(written = nx$writeback(field))
}

#* Numeric fields available for plots
#* @get /fields/numeric
function(req, res) {
  need(req, res, "read")
  md <- nx$metadata
  vt <- md$text_validation_type_or_show_slider_number
  keep <- md$field_type %in% c("calc", "slider") | (md$field_type == "text" & grepl("^(integer|number)", vt))
  data.frame(field = md$field_name[keep], label = gsub("<[^>]+>", "", md$field_label[keep]))
}

#* Scatter data with outlier flags on y
#* @get /scatter
function(req, res, x, y, event = "", method = "z", threshold = 3) {
  need(req, res, "read")
  d <- nx$shape("long", "drop", log = FALSE)
  if (nzchar(event)) d <- d[d$redcap_event_name == event, , drop = FALSE]
  o <- REDCapCDMSTAT::detect_outliers(d, y, nx$id_field(), method, as.numeric(threshold))
  out <- data.frame(record_id = o$record_id, event = o$event, x = suppressWarnings(as.numeric(d[[x]])),
                    y = o$value, score = o$score, outlier = o$outlier)
  out[!is.na(out$x) & !is.na(out$y), ]
}

#* Variables produced by a wrangling choice
#* @get /variables
function(req, res, longitudinal = "long", repeating = "drop") {
  need(req, res, "read")
  d <- nx$shape(longitudinal, repeating, log = FALSE)
  list(n_rows = nrow(d), n_cols = ncol(d), variables = attr(d, "variables"),
       preview = df_out(utils::head(d, 25)))
}

#* Metadata-driven Table 1
#* @get /table1
function(req, res, by = NULL, event = NULL) {
  need(req, res, "read")
  by <- if (is.null(by) || !nzchar(by)) NULL else by
  df_out(nx$table_one(by = by, event = event))
}

#* Longitudinal trend of a field by arm
#* @get /trend
function(req, res, field) {
  need(req, res, "read")
  df_out(nx$trend(field))
}

#* Fit a model on wide + collapsed data
#* @post /fit
function(req, res, formula, type = "linear") {
  need(req, res, "read")
  tryCatch({
    f <- nx$fit(formula, type)
    list(n = attr(f, "n"), type = type, coefficients = f)
  }, error = function(e) { res$status <- 400; list(error = conditionMessage(e)) })
}

#* Data cuts
#* @get /cuts
function(req, res) {
  need(req, res, "read")
  nx$cuts()
}

#* Freeze the current data as a hashed cut
#* @post /cuts
function(req, res, label, note = NULL) {
  need(req, res, "cut_write")
  list(cut_id = nx$freeze(label, note, req$cdmstat_user))
}

#* Sign off a cut
#* @post /cuts/<id>/signoff
function(req, res, id, comment = "Signed off") {
  need(req, res, "cut_write")
  tryCatch({ nx$sign_off(id, comment, req$cdmstat_user); list(ok = TRUE) },
           error = function(e) { res$status <- 409; list(error = conditionMessage(e)) })
}

#* Dictionary drift history
#* @get /schema/history
function(req, res) {
  need(req, res, "read")
  nx$ledger$schema_history()
}

#* Sync history
#* @get /sync/history
function(req, res) {
  need(req, res, "read")
  nx$ledger$sync_history(50)
}

#* Reproducible R script
#* @get /script
#* @serializer text
function(req, res) {
  need(req, res, "read")
  nx$script()
}

# ---- Clinical data management modules -------------------------------------

#* Randomisation configuration, balance and allocations
#* @get /randomization
function(req, res) {
  need(req, res, "read")
  r <- nx$randomization_status()
  # Allocation concealment: only data managers see who got which arm.
  if (!req$cdmstat_role %in% c("dm")) r$allocations <- NULL
  r
}

#* Generate the concealed randomisation schedule
#* @post /randomization/setup
function(req, res, strata = character(), block_sizes = c(4, 6), per_stratum = 200) {
  need(req, res, "rand_setup")
  nx$setup_randomization(strata = unlist(strata), block_sizes = as.integer(unlist(block_sizes)),
                         per_stratum = as.integer(per_stratum), actor = req$cdmstat_user)
}

#* Randomise a subject
#* @post /randomization/assign
function(req, res, record_id) {
  need(req, res, "randomize")
  tryCatch(nx$randomize(record_id, actor = req$cdmstat_user),
           error = function(e) { res$status <- 409; list(error = conditionMessage(e)) })
}

#* SDV units
#* @get /sdv
function(req, res, required = "1") {
  need(req, res, "read")
  nx$sdv_status(required_only = identical(required, "1"))
}

#* Define the risk-based SDV plan
#* @post /sdv/plan
function(req, res, critical_fields = character(), first_n = 3, sample_rate = 0.2) {
  need(req, res, "sdv_plan")
  d <- nx$sdv_plan(unlist(critical_fields), as.integer(first_n), as.numeric(sample_rate), actor = req$cdmstat_user)
  list(units = nrow(d), required = sum(d$required))
}

#* Mark SDV units verified
#* @post /sdv/verify
function(req, res, unit_keys, comment = "Verified against source") {
  need(req, res, "sdv")
  list(verified = nx$sdv_verify(unlist(unit_keys), req$cdmstat_user, comment))
}

#* Locks with integrity status
#* @get /locks
function(req, res) {
  need(req, res, "read")
  nx$locks()
}

#* Lock a record (or one event / form)
#* @post /locks
function(req, res, record_id, reason, event = NA, form = NA) {
  need(req, res, "lock")
  list(lock_id = nx$lock(record_id, reason, event, form, req$cdmstat_user))
}

#* Unlock
#* @post /locks/<id>/unlock
function(req, res, id, reason) {
  need(req, res, "lock")
  tryCatch({ nx$unlock(id, reason, req$cdmstat_user); list(ok = TRUE) },
           error = function(e) { res$status <- 409; list(error = conditionMessage(e)) })
}

#* Electronic signatures with validity
#* @get /signatures
function(req, res) {
  need(req, res, "read")
  nx$signatures()
}

#* Sign a record. The gateway must have re-verified the signer's credential.
#* @post /esign
function(req, res, record_id, meaning) {
  need(req, res, "esign")
  tryCatch(list(sig_id = nx$esign(record_id, meaning, req$cdmstat_user, identity_verified = req$cdmstat_reauth)),
           error = function(e) { res$status <- 403; list(error = conditionMessage(e)) })
}

#* Coding queue
#* @get /coding
function(req, res, status = NULL) {
  need(req, res, "read")
  nx$coding(if (is.null(status) || !nzchar(status)) NULL else strsplit(status, ",")[[1]])
}

#* Run auto-coding on a verbatim field
#* @post /coding/run
function(req, res, field = "ae_term") {
  need(req, res, "coding")
  nx$code_terms(field, actor = req$cdmstat_user)
}

#* Approve a suggestion or assign a code
#* @post /coding/approve
function(req, res, coding_key, code = NULL) {
  need(req, res, "coding")
  tryCatch(list(term = nx$approve_coding(coding_key, code, actor = req$cdmstat_user)),
           error = function(e) { res$status <- 400; list(error = conditionMessage(e)) })
}

#* Protocol deviations
#* @get /deviations
function(req, res) {
  need(req, res, "read")
  nx$deviations()
}

#* Log a deviation
#* @post /deviations
function(req, res, record_id, category, description, severity = "minor", event = NA) {
  need(req, res, "deviation")
  list(dev_id = nx$log_deviation(record_id, category, description, severity, event, actor = req$cdmstat_user))
}

#* Close a deviation
#* @post /deviations/<id>/close
function(req, res, id, comment) {
  need(req, res, "deviation")
  tryCatch({ nx$close_deviation(id, comment, req$cdmstat_user); list(ok = TRUE) },
           error = function(e) { res$status <- 409; list(error = conditionMessage(e)) })
}

#* Detect out-of-window visits
#* @post /deviations/check-windows
function(req, res, window = 7) {
  need(req, res, "deviation")
  list(new = nx$check_visit_windows(window = as.numeric(window), actor = req$cdmstat_user))
}

#* Site operational metrics
#* @get /metrics/sites
function(req, res) {
  need(req, res, "read")
  nx$site_metrics()
}

#* Cumulative enrolment by site
#* @get /metrics/enrollment
function(req, res) {
  need(req, res, "read")
  df_out(nx$enrollment_curve())
}

#* SDTM domain (DM, VS, LB, AE)
#* @get /sdtm/<domain>
function(req, res, domain) {
  need(req, res, "read")
  d <- nx$sdtm()[[toupper(domain)]]
  if (is.null(d)) { res$status <- 404; return(list(error = "Unknown domain")) }
  d
}

#* Whole study as CDISC ODM 1.3
#* @get /export/odm
#* @serializer contentType list(type = "application/xml")
function(req, res) {
  need(req, res, "read")
  write_odm(list(info = nx$info, metadata = nx$metadata, events = nx$events, mapping = nx$mapping,
                 repeating = nx$repeating, records = nx$raw))
}

#* CDMS activity trail
#* @get /activity
function(req, res, module = NULL) {
  need(req, res, "read")
  nx$activity(if (is.null(module) || !nzchar(module)) NULL else module)
}
