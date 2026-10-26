# Two-way sync between the CDMSTAT query ledger and REDCap's native Data
# Resolution Workflow (DRW), through the "CDMSTAT DRW API" External Module
# (redcap-module/cdmstat_drw_v1.0.0).
#
# Push: new CDMSTAT queries are opened in the DRW; answered / resolved /
#       closed / reopened transitions are mirrored.
# Pull: responses, closures and reopenings made by site staff in the REDCap
#       DRW pop-up are applied to the ledger, and queries opened directly in
#       REDCap are imported.
# Each linked query remembers the last DRW resolution it has seen, so nothing
# is applied twice and our own writes are not echoed back.

# CDMSTAT status <-> DRW: open -> OPEN (response requested); answered -> OPEN
# with a response; resolved / closed -> CLOSED.
drw_kind <- function(status) {
  if (status %in% c("open")) "open" else if (status %in% c("answered")) "answered" else "closed"
}

RedcapBackend$set("public", "drw", function(action, payload = list(), prefix = "cdmstat_drw") {
  payload <- payload[!vapply(payload, function(v) is.null(v) || (length(v) == 1 && is.na(v)), logical(1))]
  args <- c(list(content = "externalModule", prefix = prefix, action = action), lapply(payload, as.character))
  do.call(self$call, args)
})

# Offline stand-in for the External Module, with the same actions, rules and
# response shapes, so the demo and the tests exercise the real sync logic.
DemoBackend$set("public", "drw", function(action, payload = list(), prefix = "cdmstat_drw", user = "cdmstat_api") {
  st <- self$state()
  d <- st$drw %||% list(
    status = data.frame(status_id = integer(), record = character(), event = character(), field_name = character(),
                        instance = integer(), repeat_instrument = character(), query_status = character(),
                        assigned_to = character(), stringsAsFactors = FALSE),
    res = data.frame(res_id = integer(), status_id = integer(), ts = character(), username = character(),
                     response_requested = integer(), response = character(), comment = character(),
                     current_query_status = character(), stringsAsFactors = FALSE))
  p <- function(k, default = "") { v <- payload[[k]]; if (is.null(v) || (length(v) == 1 && is.na(v))) default else as.character(v) }
  fail <- function(msg) stop("REDCap API error: ", msg, call. = FALSE)
  add_res <- function(sid, requested, response, comment, current) {
    id <- if (nrow(d$res)) max(d$res$res_id) + 1L else 1L
    actor <- p("actor")
    d$res <<- rbind(d$res, data.frame(res_id = id, status_id = as.integer(sid), ts = now_stamp(), username = user,
                                      response_requested = as.integer(requested), response = response,
                                      comment = if (nzchar(actor)) sprintf("[CDMSTAT: %s] %s", actor, comment) else comment,
                                      current_query_status = current, stringsAsFactors = FALSE))
    id
  }
  save <- function(out) { st$drw <- d; private$save(st); out }
  target <- function() {
    if (!nzchar(p("record")) || !nzchar(p("field"))) fail("record and field are required.")
    if (!p("field") %in% st$metadata$field_name) fail(sprintf("Field '%s' is not in this project.", p("field")))
    if (!p("record") %in% st$records$record_id) fail(sprintf("Record '%s' does not exist.", p("record")))
    ev <- if (nzchar(p("event"))) p("event") else st$events$unique_event_name[1]
    inst <- max(1L, suppressWarnings(as.integer(p("instance", "1"))), na.rm = TRUE)
    rep <- p("repeat_instrument")
    i <- which(d$status$record == p("record") & d$status$event == ev & d$status$field_name == p("field") &
                 d$status$instance == inst & d$status$repeat_instrument == rep)
    list(ev = ev, inst = inst, rep = rep, i = i)
  }
  switch(action,
    "drw-status" = list(module = "cdmstat_drw", version = "1.0.0 (demo)", drw_enabled = TRUE, schema_ok = TRUE,
                        missing_columns = list(), user = user, rights_level = 3L),
    "list-queries" = {
      since <- suppressWarnings(as.integer(p("since_res_id", "0")))
      s <- d$status
      if (since > 0) s <- s[s$status_id %in% d$res$status_id[d$res$res_id > since], , drop = FALSE]
      threads <- lapply(seq_len(nrow(s)), function(k) {
        r <- d$res[d$res$status_id == s$status_id[k], , drop = FALSE]
        c(as.list(s[k, ]), list(resolutions = r))
      })
      list(threads = threads, max_res_id = if (nrow(d$res)) max(d$res$res_id) else 0L)
    },
    "open-query" = {
      t <- target()
      if (!nzchar(p("comment"))) fail("comment is required.")
      if (length(t$i) && d$status$query_status[t$i] == "OPEN") {
        return(list(status_id = d$status$status_id[t$i], query_status = "OPEN", already_open = TRUE))
      }
      if (!length(t$i)) {
        sid <- if (nrow(d$status)) max(d$status$status_id) + 1L else 1L
        d$status <- rbind(d$status, data.frame(status_id = sid, record = p("record"), event = t$ev, field_name = p("field"),
                                               instance = t$inst, repeat_instrument = t$rep, query_status = "OPEN",
                                               assigned_to = p("assign_to"), stringsAsFactors = FALSE))
      } else {
        sid <- d$status$status_id[t$i]
        d$status$query_status[t$i] <- "OPEN"
      }
      rid <- add_res(sid, 1, NA_character_, p("comment"), "OPEN")
      save(list(status_id = sid, res_id = rid, query_status = "OPEN", already_open = FALSE))
    },
    "respond" = , "close-query" = , "reopen-query" = , "add-comment" = {
      sid <- as.integer(p("status_id", "0"))
      i <- which(d$status$status_id == sid)
      if (!length(i)) fail(sprintf("No query thread %s in this project.", sid))
      if (!nzchar(p("comment"))) fail("comment is required.")
      cur <- d$status$query_status[i]
      new <- cur
      response <- NA_character_
      requested <- 0
      if (action == "respond") {
        if (cur != "OPEN") fail("Only open queries can be answered.")
        response <- toupper(p("response", "OTHER"))
      } else if (action == "close-query") {
        if (cur != "OPEN") fail("Only open queries can be closed.")
        new <- "CLOSED"
      } else if (action == "reopen-query") {
        if (cur != "CLOSED") fail("Only closed queries can be reopened.")
        new <- "OPEN"
        requested <- 1
      }
      d$status$query_status[i] <- new
      rid <- add_res(sid, requested, response, p("comment"), new)
      save(list(status_id = sid, res_id = rid, query_status = new))
    },
    "lock-form" = , "unlock-form" = , "list-locks" = {
      locks <- st$edc_locks %||% data.frame(record = character(), event = character(), form_name = character(),
                                            instance = integer(), username = character(), timestamp = character(),
                                            stringsAsFactors = FALSE)
      if (action == "list-locks") {
        l <- if (nzchar(p("record"))) locks[locks$record == p("record"), , drop = FALSE] else locks
        return(list(locks = l))
      }
      if (!nzchar(p("record")) || !nzchar(p("form"))) fail("record and form are required.")
      if (!p("form") %in% st$metadata$form_name) fail(sprintf("Form '%s' is not in this project.", p("form")))
      ev <- if (nzchar(p("event"))) p("event") else st$events$unique_event_name[1]
      inst <- max(1L, suppressWarnings(as.integer(p("instance", "1"))), na.rm = TRUE)
      hit <- which(locks$record == p("record") & locks$event == ev & locks$form_name == p("form") & locks$instance == inst)
      if (action == "lock-form") {
        if (length(hit)) return(list(locked = TRUE, already_locked = TRUE))
        st$edc_locks <- rbind(locks, data.frame(record = p("record"), event = ev, form_name = p("form"), instance = inst,
                                                username = user, timestamp = now_stamp(), stringsAsFactors = FALSE))
        private$save(st)
        return(list(locked = TRUE, already_locked = FALSE))
      }
      if (!nzchar(p("reason"))) fail("reason is required.")
      if (length(hit)) { st$edc_locks <- locks[-hit, , drop = FALSE]; private$save(st) }
      list(unlocked = length(hit) > 0)
    },
    "verify-field" = , "deverify-field" = {
      t <- target()
      new <- if (action == "verify-field") "VERIFIED" else "DEVERIFIED"
      if (length(t$i) && d$status$query_status[t$i] == "OPEN") fail("Close the open query on this value before changing its verification.")
      if (!length(t$i)) {
        sid <- if (nrow(d$status)) max(d$status$status_id) + 1L else 1L
        d$status <- rbind(d$status, data.frame(status_id = sid, record = p("record"), event = t$ev, field_name = p("field"),
                                               instance = t$inst, repeat_instrument = t$rep, query_status = new,
                                               assigned_to = "", stringsAsFactors = FALSE))
      } else {
        sid <- d$status$status_id[t$i]
        d$status$query_status[t$i] <- new
      }
      rid <- add_res(sid, 0, NA_character_, if (nzchar(p("comment"))) p("comment") else "Verified against source", new)
      save(list(status_id = sid, res_id = rid, query_status = new))
    },
    fail(sprintf("Unknown action '%s'.", action)))
})

# ---- engine methods --------------------------------------------------------

CDMSTAT$set("public", "drw_status", function(prefix = NULL) {
  private$need_backend()
  cfg <- self$ledger$get_setting("drw", list())
  self$backend$drw("drw-status", list(), prefix = prefix %||% cfg$prefix %||% "cdmstat_drw")
})

# Turn on DRW write-back after checking the module is installed, the DRW is
# enabled in the project and the token user may open and close queries.
# @param verify_sdv also mark field values verified in the DRW when SDV units
#   are verified in CDMSTAT.
# @param lock_in_edc also lock forms in REDCap when CDMSTAT locks a record or
#   form, so the EDC itself refuses edits (needs the Record Locking right).
CDMSTAT$set("public", "drw_enable", function(prefix = "cdmstat_drw", verify_sdv = TRUE, lock_in_edc = TRUE, actor = self$actor) {
  s <- self$drw_status(prefix)
  if (!isTRUE(s$drw_enabled)) stop("The Data Resolution Workflow is not enabled in this REDCap project.", call. = FALSE)
  if (!isTRUE(s$schema_ok)) stop("The module reports an unexpected DRW schema: ", paste(unlist(s$missing_columns), collapse = ", "), call. = FALSE)
  if (!identical(s$rights_level, "superuser") && !identical(as.integer(s$rights_level), 3L)) {
    stop("The API token user needs the DRW right 'Open, close, and respond to queries' (level 3); has ", s$rights_level, ".", call. = FALSE)
  }
  if (lock_in_edc && !identical(s$rights_level, "superuser") && !isTRUE(as.integer(s$lock_right %||% 1L) >= 1L)) {
    stop("lock_in_edc needs the Record Locking user right for the API token user.", call. = FALSE)
  }
  self$ledger$set_setting("drw", list(enabled = TRUE, prefix = prefix, last_max_res_id = 0L,
                                      verify_sdv = verify_sdv, lock_in_edc = lock_in_edc), actor)
  self$ledger$activity(actor, "drw", "enable", sprintf("module %s, token user %s", prefix, s$user))
  invisible(s)
})

CDMSTAT$set("public", "drw_disable", function(actor = self$actor) {
  cfg <- self$ledger$get_setting("drw", list())
  cfg$enabled <- FALSE
  self$ledger$set_setting("drw", cfg, actor)
  self$ledger$activity(actor, "drw", "disable", "")
  invisible(TRUE)
})

CDMSTAT$set("public", "drw_links", function() self$ledger$get("SELECT * FROM drw_link ORDER BY synced_at DESC"))

# Push ledger changes to the DRW and pull site activity back.
CDMSTAT$set("public", "drw_sync", function(actor = self$actor) {
  private$need_backend()
  cfg <- self$ledger$get_setting("drw", list())
  if (!isTRUE(cfg$enabled)) stop("DRW write-back is not enabled; call $drw_enable() first.", call. = FALSE)
  prefix <- cfg$prefix %||% "cdmstat_drw"
  call <- function(action, payload) self$backend$drw(action, payload, prefix = prefix)
  links <- self$ledger$get("SELECT * FROM drw_link")
  q <- self$ledger$queries()
  md <- self$metadata
  rep_forms <- self$repeating_forms()
  out <- list(opened = 0L, updated = 0L, pulled = 0L, imported = 0L, skipped = 0L, errors = character())
  set_link <- function(qid, sid, res_id, pushed) {
    self$ledger$run("INSERT OR REPLACE INTO drw_link (query_id, status_id, last_res_id, pushed_status, synced_at) VALUES (?,?,?,?,?)",
                    list(qid, if (is.na(sid)) NA_integer_ else as.integer(sid), as.integer(res_id %||% 0L), pushed, now_stamp()))
  }
  last_comment <- function(qid) {
    a <- self$ledger$audit(qid)
    a <- a[!is.na(a$comment) & nzchar(a$comment), , drop = FALSE]
    if (nrow(a)) utils::tail(a$comment, 1) else "Updated in CDMSTAT"
  }

  # ---- push
  for (i in seq_len(nrow(q))) {
    r <- q[i, ]
    if (identical(r$source, "redcap-drw") && !r$query_id %in% links$query_id) next
    l <- links[links$query_id == r$query_id, , drop = FALSE]
    res <- tryCatch({
      if (!nrow(l)) {
        if (!r$status %in% c("open", "answered")) NULL
        else if (!r$field %in% md$field_name) { set_link(r$query_id, NA, 0L, "skipped: not a project field"); out$skipped <- out$skipped + 1L; NULL }
        else {
          form <- md$form_name[md$field_name == r$field]
          inst <- suppressWarnings(as.integer(r$instance))
          o <- call("open-query", list(record = r$record_id, field = r$field, event = r$event,
                                       instance = if (is.na(inst)) NULL else inst,
                                       repeat_instrument = if (form %in% rep_forms && !is.na(inst)) form else NULL,
                                       comment = r$message, actor = r$created_by))
          rid <- o$res_id %||% 0L
          if (r$status == "answered") {
            rid <- call("respond", list(status_id = o$status_id, response = "OTHER", comment = last_comment(r$query_id),
                                        actor = actor))$res_id
          }
          set_link(r$query_id, o$status_id, rid, drw_kind(r$status))
          out$opened <- out$opened + 1L
        }
      } else if (!is.na(l$status_id) && l$pushed_status != drw_kind(r$status)) {
        from <- l$pushed_status
        to <- drw_kind(r$status)
        cm <- last_comment(r$query_id)
        rid <- l$last_res_id
        if (to == "closed") {
          rid <- call("close-query", list(status_id = l$status_id, comment = cm, actor = actor))$res_id
        } else if (from == "closed") {
          rid <- call("reopen-query", list(status_id = l$status_id, comment = cm, actor = actor))$res_id
          if (to == "answered") rid <- call("respond", list(status_id = l$status_id, response = "OTHER", comment = cm, actor = actor))$res_id
        } else if (to == "answered") {
          rid <- call("respond", list(status_id = l$status_id, response = "OTHER", comment = cm, actor = actor))$res_id
        }
        set_link(r$query_id, l$status_id, rid, to)
        out$updated <- out$updated + 1L
      }
      NULL
    }, error = function(e) paste0(r$query_id, ": ", conditionMessage(e)))
    if (!is.null(res)) out$errors <- c(out$errors, res)
  }

  # ---- pull
  links <- self$ledger$get("SELECT * FROM drw_link")
  lst <- call("list-queries", list(since_res_id = cfg$last_max_res_id %||% 0L))
  threads <- lst$threads
  if (is.data.frame(threads)) threads <- lapply(seq_len(nrow(threads)), function(k) as.list(threads[k, ]))
  for (t in threads) {
    resol <- t$resolutions
    if (!is.data.frame(resol) && is.list(resol) && length(resol) == 1 && is.data.frame(resol[[1]])) resol <- resol[[1]]
    resol <- as.data.frame(resol, stringsAsFactors = FALSE)
    if (!nrow(resol)) next
    for (cc in c("response", "comment", "username", "current_query_status")) if (!cc %in% names(resol)) resol[[cc]] <- NA_character_
    if (!nrow(resol)) next
    resol <- resol[order(as.integer(resol$res_id)), , drop = FALSE]
    l <- links[!is.na(links$status_id) & links$status_id == as.integer(t$status_id), , drop = FALSE]
    if (!nrow(l)) {
      if (!identical(t$query_status, "OPEN")) next
      # A query opened directly in REDCap: import it.
      first <- resol[1, ]
      qid <- self$ledger$add_query(t$record, t$field_name, sub("^\\[CDMSTAT: [^]]*\\] ", "", first$comment),
                                   event = t$event %||% NA, instance = t$instance %||% NA, source = "redcap-drw",
                                   actor = paste0("redcap:", first$username %||% "unknown"))
      set_link(qid, t$status_id, max(as.integer(resol$res_id)), "open")
      out$imported <- out$imported + 1L
      next
    }
    new <- resol[as.integer(resol$res_id) > l$last_res_id, , drop = FALSE]
    cur <- self$ledger$get("SELECT status FROM queries WHERE query_id = ?", list(l$query_id))$status
    for (k in seq_len(nrow(new))) {
      n <- new[k, ]
      who <- paste0("redcap:", n$username %||% "unknown")
      cm <- paste0(if (is.na(n$comment) || !nzchar(n$comment)) "(no comment)" else n$comment,
                   if (!is.na(n$response) && nzchar(n$response)) paste0(" [", n$response, "]") else "")
      if (identical(n$current_query_status, "CLOSED") && cur %in% c("open", "answered")) {
        self$ledger$transition(l$query_id, "resolved", who, paste("Closed in REDCap DRW:", cm)); cur <- "resolved"
      } else if (identical(n$current_query_status, "OPEN") && isTRUE(as.integer(n$response_requested) == 1) && cur %in% c("resolved", "closed")) {
        self$ledger$transition(l$query_id, "open", who, paste("Reopened in REDCap DRW:", cm)); cur <- "open"
      } else if (!is.na(n$response) && nzchar(n$response) && cur == "open") {
        self$ledger$transition(l$query_id, "answered", who, paste("Site response in REDCap DRW:", cm)); cur <- "answered"
      } else {
        self$ledger$comment(l$query_id, who, paste("REDCap DRW:", cm))
      }
      out$pulled <- out$pulled + 1L
    }
    if (nrow(new)) set_link(l$query_id, l$status_id, max(as.integer(new$res_id)), drw_kind(cur))
  }
  cfg$last_max_res_id <- as.integer(lst$max_res_id %||% cfg$last_max_res_id %||% 0L)
  self$ledger$set_setting("drw", cfg, actor)
  self$ledger$activity(actor, "drw", "sync", sprintf("opened %d, updated %d, pulled %d, imported %d, skipped %d, errors %d",
                                                    out$opened, out$updated, out$pulled, out$imported, out$skipped, length(out$errors)))
  invisible(out)
})
