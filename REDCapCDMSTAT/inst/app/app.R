library(shiny)
library(bslib)
library(plotly)
library(DT)
library(REDCapCDMSTAT)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

nx <- getOption("REDCapCDMSTAT.instance") %||% cdmstat_from_env()

numeric_fields <- function() {
  md <- nx$metadata
  vt <- md$text_validation_type_or_show_slider_number
  md$field_name[md$field_type %in% c("calc", "slider") | (md$field_type == "text" & grepl("^(integer|number)", vt))]
}
field_choices <- function(fields) stats::setNames(fields, paste0(fields, " - ", vapply(fields, function(f) {
  gsub("<[^>]+>", "", nx$metadata$field_label[match(f, nx$metadata$field_name)])
}, "")))
# Keep the user's current choice if it is still valid, else use the default.
keep_or <- function(current, default, choices) {
  if (!is.null(current) && current %in% choices) return(current)
  hit <- intersect(default, choices)
  if (length(hit)) hit[1] else unname(choices[1])
}
status_choices <- c("open", "answered", "resolved", "closed")

dt <- function(df, ...) {
  datatable(df, rownames = FALSE, filter = "top", selection = "single",
            options = list(pageLength = 15, scrollX = TRUE, dom = "tip"), ...)
}

ui <- page_navbar(
  title = tags$span(tags$img(src = "logo.png", class = "brand-logo", alt = ""), "REDCap CDMSTAT"),
  window_title = "REDCap CDMSTAT",
  theme = bs_theme(version = 5, primary = "#2c5d8f", "navbar-bg" = "#1f3b57", base_font = font_collection("Inter", "Segoe UI", "system-ui", "sans-serif")),
  header = tagList(
    tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png")),
    tags$style(HTML(".value-box-title{font-size:.8rem} .card{margin-bottom:1rem} pre{white-space:pre-wrap} .brand-logo{height:34px;width:34px;border-radius:6px;background:#fff;padding:2px;margin-right:10px;vertical-align:middle}"))
  ),
  nav_panel("Overview",
    layout_column_wrap(width = 1 / 4,
      value_box("Records", textOutput("vb_records")),
      value_box("Open queries", textOutput("vb_open")),
      value_box("Rule violations", textOutput("vb_issues")),
      value_box("Last sync", textOutput("vb_sync"))
    ),
    layout_columns(col_widths = c(4, 8),
      card(card_header("Project"), uiOutput("project_info"),
           actionButton("sync", "Sync now (incremental)", class = "btn-primary"),
           actionButton("sync_full", "Full re-pull", class = "btn-outline-secondary mt-2")),
      card(card_header("Queries by status and source"), plotlyOutput("q_plot", height = 260))
    ),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Data dictionary drift"), DTOutput("schema_tbl")),
      card(card_header("Sync history"), DTOutput("sync_tbl"))
    )
  ),
  nav_panel("Data quality",
    layout_sidebar(
      sidebar = sidebar(width = 320,
        actionButton("raise", "Raise queries for new issues", class = "btn-warning"),
        helpText("Rules come from the live data dictionary (ranges, required, branching, calculations) plus custom rules below. Fixed issues auto-resolve their queries."),
        hr(), h6("Add a custom rule (REDCap syntax, must be TRUE)"),
        textInput("rule_id", "Rule id", "weight_change"),
        textInput("rule_logic", "Condition", "[weight] < 180"),
        textInput("rule_msg", "Message", "Weight above 180 kg - please verify"),
        selectInput("rule_sev", "Severity", c("warning", "error")),
        actionButton("add_rule", "Add rule"),
        uiOutput("rule_list")
      ),
      card(card_header("Current rule violations"), DTOutput("issues_tbl"))
    )
  ),
  nav_panel("Queries",
    layout_sidebar(
      sidebar = sidebar(width = 300,
        checkboxGroupInput("q_status", "Status", status_choices, selected = c("open", "answered")),
        hr(), h6("Selected query"), uiOutput("q_selected"),
        selectInput("q_new_status", "Move to", status_choices),
        textAreaInput("q_comment", "Reason for change (required)", rows = 3),
        actionButton("q_apply", "Apply transition", class = "btn-primary"),
        actionButton("q_add_comment", "Add comment only", class = "btn-outline-secondary mt-2"),
        hr(), actionButton("writeback", "Write open counts to REDCap"),
        hr(), h6("REDCap Data Resolution Workflow"), uiOutput("drw_state"),
        actionButton("drw_enable", "Enable DRW write-back"),
        actionButton("drw_sync", "Sync with REDCap DRW", class = "btn-outline-primary mt-2")
      ),
      card(card_header("Query ledger"), DTOutput("q_tbl")),
      card(card_header("Audit trail of selected query"), DTOutput("audit_tbl"))
    )
  ),
  nav_panel("Explore",
    layout_sidebar(
      sidebar = sidebar(width = 300,
        selectInput("ex_x", "X axis", NULL), selectInput("ex_y", "Y axis (outlier check)", NULL),
        selectInput("ex_event", "Event", NULL),
        radioButtons("ex_method", "Outlier method", c("Robust z" = "z", "Tukey IQR" = "iqr"), inline = TRUE),
        numericInput("ex_thr", "Threshold", 3, min = 1, step = .5),
        helpText("Click any point to raise a data query on that record. The query lands in the same ledger the data managers work from.")
      ),
      card(full_screen = TRUE, card_header("Point-click-to-query scatter"), plotlyOutput("ex_plot", height = 520)),
      card(card_header("Flagged outliers on Y"), DTOutput("ex_out"))
    )
  ),
  nav_panel("Wrangle",
    layout_sidebar(
      sidebar = sidebar(width = 300,
        radioButtons("wr_long", "Longitudinal events", c("Keep long (row per visit)" = "long", "Cast wide by event" = "wide")),
        radioButtons("wr_rep", "Repeating instruments", c("Drop" = "drop", "Collapse to per-record summaries" = "collapse", "Only repeating rows" = "only")),
        downloadButton("wr_dl", "Download CSV", icon = NULL),
        helpText("The variable list updates instantly; it is what the model builder can use.")
      ),
      layout_columns(col_widths = c(5, 7),
        card(card_header(textOutput("wr_dim")), DTOutput("wr_vars")),
        card(full_screen = TRUE, card_header("Preview"), DTOutput("wr_preview"))
      )
    )
  ),
  nav_panel("Statistics",
    navset_card_tab(
      nav_panel("Table 1",
        layout_columns(col_widths = c(3, 3, 3, 3),
          selectInput("t1_by", "Compare by", NULL), selectInput("t1_event", "Event", NULL),
          radioButtons("t1_mode", "Mode", c("Exploratory (auto tests)" = "exploratory"), inline = TRUE),
          downloadButton("t1_dl", "Download CSV", icon = NULL)),
        helpText("Arm and baseline variables are detected from the dictionary. Tests are chosen per variable from normality and expected counts; the rationale column shows why. For confirmatory analysis use SAP mode via the R API (tests pre-specified)."),
        DTOutput("t1_tbl")),
      nav_panel("Longitudinal trends",
        selectInput("tr_field", "Outcome", NULL), plotlyOutput("tr_plot", height = 420), DTOutput("tr_tbl")),
      nav_panel("Model builder",
        layout_columns(col_widths = c(6, 3, 3),
          textInput("m_formula", "Formula (wide + collapsed data)", "sbp__week_24 ~ arm + sbp__baseline + age"),
          selectInput("m_type", "Model", c("linear", "logistic", "cox", "mixed", "mixed_logistic")),
          actionButton("m_run", "Fit", class = "btn-primary mt-4")),
        helpText("Cox: Surv(followup_days, event_occurred == 'Yes') ~ arm + age. Logistic: any_ae_serious ~ arm. Mixed (repeated measures on long data): sbp ~ arm * redcap_event_name + (1 | record_id)."),
        verbatimTextOutput("m_info"), DTOutput("m_tbl"))
    )
  ),
  nav_panel("Data cuts",
    layout_sidebar(
      sidebar = sidebar(width = 320,
        textInput("cut_label", "Cut label", "Interim analysis 1"), textAreaInput("cut_note", "Note", rows = 2),
        actionButton("cut_freeze", "Freeze current data", class = "btn-primary"),
        hr(), textAreaInput("cut_sign_comment", "Sign-off statement", "Data reviewed and clean for analysis", rows = 2),
        actionButton("cut_sign", "Sign off selected cut"),
        hr(), actionButton("cut_use", "Analyse selected cut"),
        actionButton("cut_live", "Back to live data", class = "btn-outline-secondary mt-2"),
        uiOutput("cut_active")
      ),
      card(card_header("Frozen, hashed analysis data cuts"), DTOutput("cuts_tbl"))
    )
  ),
  nav_panel("Script",
    card(card_header("Reproducible R script of this project's analysis steps"),
         downloadButton("script_dl", "Download .R", icon = NULL), verbatimTextOutput("script_txt"))
  ),
  nav_menu("CDMS",
    nav_panel("Randomization",
      layout_sidebar(
        sidebar = sidebar(width = 320,
          h6("Set up (once, before the first allocation)"),
          selectizeInput("rz_strata", "Stratify by", NULL, multiple = TRUE),
          textInput("rz_blocks", "Block sizes", "4, 6"),
          numericInput("rz_per", "Slots per stratum", 200, min = 10),
          actionButton("rz_setup", "Generate concealed schedule"),
          hr(), h6("Randomize a subject"),
          selectizeInput("rz_record", "Record", NULL),
          actionButton("rz_go", "Randomize", class = "btn-primary")
        ),
        card(card_header("Allocation balance by stratum"), DTOutput("rz_balance")),
        card(card_header("Allocations (audit)"), DTOutput("rz_alloc"))
      )),
    nav_panel("SDV",
      layout_sidebar(
        sidebar = sidebar(width = 320,
          selectizeInput("sdv_crit", "Critical fields (100% SDV)", NULL, multiple = TRUE),
          numericInput("sdv_first", "First N subjects per site", 3, min = 0),
          sliderInput("sdv_rate", "Random sample of remaining", 0, 1, .2, step = .05),
          actionButton("sdv_plan", "Apply SDV plan"),
          hr(), checkboxInput("sdv_req", "Required units only", TRUE),
          actionButton("sdv_verify", "Mark selected as verified", class = "btn-primary")
        ),
        card(card_header("Source data verification units (record x event x form)"), DTOutput("sdv_tbl"))
      )),
    nav_panel("Locks & e-signatures",
      layout_columns(col_widths = c(4, 8),
        card(card_header("Lock / sign"),
             selectizeInput("lk_record", "Record", NULL), textInput("lk_reason", "Lock reason", "Database lock"),
             actionButton("lk_go", "Lock record"),
             hr(), textInput("sg_meaning", "Signature meaning", "I approve the data of this record as Principal Investigator"),
             textInput("sg_user", "Re-enter your user name to sign"),
             actionButton("sg_go", "Sign record", class = "btn-primary"),
             hr(), textInput("ul_reason", "Unlock reason"), actionButton("ul_go", "Unlock selected lock")),
        tagList(card(card_header("Active locks (integrity checked against current data)"), DTOutput("lk_tbl")),
                card(card_header("Electronic signatures"), DTOutput("sg_tbl")))
      )),
    nav_panel("Medical coding",
      layout_sidebar(
        sidebar = sidebar(width = 320,
          selectInput("cd_field", "Verbatim field", NULL),
          actionButton("cd_run", "Auto-code", class = "btn-primary"),
          fileInput("cd_dict", "Load dictionary CSV (code, term, synonyms)", accept = ".csv"),
          helpText("The built-in dictionary is a demo with fictitious codes. Load your licensed MedDRA / WHODrug export."),
          hr(), actionButton("cd_approve", "Approve selected suggestion"),
          textInput("cd_code", "...or assign code"), actionButton("cd_assign", "Assign code to selected")
        ),
        card(card_header("Coding queue"), DTOutput("cd_tbl"))
      )),
    nav_panel("Deviations",
      layout_sidebar(
        sidebar = sidebar(width = 320,
          numericInput("dv_window", "Visit window (+/- days)", 7, min = 0),
          actionButton("dv_check", "Detect out-of-window visits"),
          hr(), h6("Log a deviation"),
          selectizeInput("dv_record", "Record", NULL),
          selectInput("dv_cat", "Category", c("Eligibility", "Informed consent", "Visit out of window", "Dosing", "Procedure not done", "Other")),
          textAreaInput("dv_desc", "Description", rows = 2), selectInput("dv_sev", "Severity", c("minor", "major")),
          actionButton("dv_log", "Log deviation"),
          hr(), textInput("dv_close_c", "Closing comment"), actionButton("dv_close", "Close selected")
        ),
        card(card_header("Protocol deviation log"), DTOutput("dv_tbl"))
      )),
    nav_panel("Site metrics",
      card(card_header("Operational metrics by site"), DTOutput("sm_tbl")),
      card(card_header("Cumulative enrolment by site"), plotlyOutput("sm_enrol", height = 360))),
    nav_panel("CDISC export",
      layout_columns(col_widths = c(3, 9),
        card(card_header("Export"), downloadButton("cdisc_zip", "Download SDTM + ODM (.zip)", icon = NULL),
             helpText("DM, VS, LB, AE, SV and DS domains (CSV and SAS .xpt), define.xml, and the full study as CDISC ODM 1.3. AEDECOD comes from the coding module.")),
        navset_card_tab(
          nav_panel("DM", DTOutput("sd_dm")), nav_panel("VS", DTOutput("sd_vs")),
          nav_panel("LB", DTOutput("sd_lb")), nav_panel("AE", DTOutput("sd_ae")),
          nav_panel("SV", DTOutput("sd_sv")), nav_panel("DS", DTOutput("sd_ds"))))),
    nav_panel("Activity trail", card(card_header("All CDMS actions (append-only)"), DTOutput("act_tbl")))
  ),
  nav_panel("Connection",
    layout_columns(col_widths = c(5, 7),
      card(card_header("Current data source"), uiOutput("cx_current")),
      card(card_header("Connect a REDCap project"),
        textInput("cx_url", "REDCap API URL", placeholder = "https://redcap.your-institution.org/api/"),
        passwordInput("cx_token", "API token (from the project's API page)"),
        radioButtons("cx_store", "Keep the token",
                     c("In my user .Renviron file" = "renviron", "For this session only" = "session",
                       "In the OS keyring (needs the keyring package)" = "keyring")),
        div(actionButton("cx_test", "Test connection"),
            actionButton("cx_save", "Save and connect", class = "btn-primary")),
        helpText("Testing is read-only. Saving stores the URL in the project folder; the token is kept only where you choose above. Other users of this app pick up the new connection after a restart."),
        tableOutput("cx_result"))
    )
  ),
  nav_spacer(),
  nav_item(textOutput("whoami", inline = TRUE)),
  nav_item(input_dark_mode(id = "mode"))
)

server <- function(input, output, session) {
  # Each browser session gets its own engine object, so choosing a data cut
  # to analyse affects only that user. Cache, ledger and backend are shared:
  # writes reach other sessions through the file poll below.
  nx <- nx$clone()
  actor <- session$user %||% nx$actor
  output$whoami <- renderText(paste("Signed in as", actor))
  bump <- reactiveVal(0)
  touch <- function() bump(bump() + 1)

  # Pick up changes made by other processes (the API / DET receiver).
  external <- reactivePoll(3000, session,
    checkFunc = function() {
      list(file.mtime(file.path(nx$dir, "cache.rds")), nx$ledger$stamp())
    },
    valueFunc = function() { nx$reload_if_changed(); Sys.time() })

  state <- reactive({ bump(); external(); list(t = Sys.time()) })
  issues <- reactive({ state(); nx$validate() })
  queries <- reactive({ state(); nx$queries() })

  notify_err <- function(expr) tryCatch(expr, error = function(e) {
    showNotification(conditionMessage(e), type = "error", duration = 8)
    NULL
  })

  observe({
    state()
    nf <- field_choices(numeric_fields())
    updateSelectInput(session, "ex_x", choices = nf, selected = keep_or(isolate(input$ex_x), "height", nf))
    updateSelectInput(session, "ex_y", choices = nf, selected = keep_or(isolate(input$ex_y), "weight", nf))
    updateSelectInput(session, "tr_field", choices = nf, selected = keep_or(isolate(input$tr_field), "sbp", nf))
    evs <- nx$events
    ev_ch <- if (NROW(evs)) stats::setNames(evs$unique_event_name, evs$event_name) else c("All" = "")
    updateSelectInput(session, "ex_event", choices = c("All events" = "", ev_ch), selected = keep_or(isolate(input$ex_event), "", c("", ev_ch)))
    updateSelectInput(session, "t1_event", choices = ev_ch, selected = keep_or(isolate(input$t1_event), ev_ch[1], ev_ch))
    cat_f <- nx$metadata$field_name[nx$metadata$field_type %in% c("radio", "dropdown", "yesno")]
    arm <- nx$arm()
    updateSelectInput(session, "t1_by", choices = c("No grouping" = "", field_choices(cat_f)),
                      selected = keep_or(isolate(input$t1_by), arm, cat_f))
  })

  # ---- Overview
  output$vb_records <- renderText({ state(); nx$summary()$records })
  output$vb_open <- renderText(sum(queries()$status %in% c("open", "answered")))
  output$vb_issues <- renderText(nrow(issues()))
  output$vb_sync <- renderText({ state(); format(nx$last_sync, "%d %b %H:%M") })
  output$project_info <- renderUI({
    state()
    s <- nx$summary()
    tags$dl(tags$dt("Project"), tags$dd(s$project), tags$dt("Source"), tags$dd(s$source), tags$dt("PID"), tags$dd(s$project_id),
            tags$dt("Rows / fields / events"), tags$dd(sprintf("%s / %s / %s", s$rows, s$fields, s$events)),
            tags$dt("Dictionary hash"), tags$dd(code(s$dict_hash)),
            tags$dt("Analysing"), tags$dd(if (is.na(s$active_cut)) "Live data" else paste("Cut", s$active_cut)))
  })
  observeEvent(input$sync, {
    r <- notify_err(nx$sync())
    if (!is.null(r)) showNotification(sprintf("Sync: %d record(s) patched, %d deleted%s", r$changed, r$deleted,
                                              if (r$metadata_changed) ", DATA DICTIONARY CHANGED" else ""),
                                      type = if (r$metadata_changed) "warning" else "message")
    touch()
  })
  observeEvent(input$sync_full, { notify_err(nx$sync(full = TRUE)); showNotification("Full re-pull complete"); touch() })
  output$q_plot <- renderPlotly({
    q <- queries()
    if (!nrow(q)) return(plotly_empty(type = "scatter", mode = "markers") |> layout(title = "No queries yet"))
    agg <- as.data.frame(table(status = factor(q$status, status_choices), source = q$source))
    plot_ly(agg, x = ~status, y = ~Freq, color = ~source, type = "bar") |>
      layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "Queries"))
  })
  output$schema_tbl <- renderDT({ state(); d <- nx$ledger$schema_history(); d$old_hash <- substr(d$old_hash, 1, 10); d$new_hash <- substr(d$new_hash, 1, 10); dt(d) })
  output$sync_tbl <- renderDT({ state(); dt(nx$ledger$sync_history(30)) })

  # ---- Data quality
  output$issues_tbl <- renderDT(dt(issues()[, c("severity", "rule", "record_id", "event", "instance", "field", "value", "message")]))
  observeEvent(input$raise, {
    r <- notify_err(nx$raise_queries(actor))
    if (!is.null(r)) showNotification(sprintf("%d new queries, %d auto-resolved", r$created, r$auto_resolved))
    touch()
  })
  observeEvent(input$add_rule, {
    notify_err(nx$add_rule(input$rule_id, input$rule_logic, input$rule_msg, input$rule_sev))
    touch()
  })
  output$rule_list <- renderUI({
    state()
    if (!length(nx$rules)) return(NULL)
    tagList(hr(), h6("Custom rules"), tags$ul(lapply(nx$rules, function(r) tags$li(code(r$logic), " - ", r$message))))
  })

  # ---- Queries
  q_view <- reactive({ q <- queries(); q[q$status %in% input$q_status, , drop = FALSE] })
  output$q_tbl <- renderDT(dt(q_view()[, c("query_id", "status", "severity", "source", "record_id", "event", "field", "value", "message", "created_by", "updated_at")]))
  sel_q <- reactive({ i <- input$q_tbl_rows_selected; if (length(i)) q_view()[i, ] else NULL })
  output$q_selected <- renderUI({
    q <- sel_q()
    if (is.null(q)) return(helpText("Select a row"))
    tags$small(tags$b(q$query_id), br(), sprintf("Record %s, %s, %s = %s", q$record_id, q$event, q$field, q$value),
               br(), em(q$message), br(), "Status: ", tags$b(q$status))
  })
  output$audit_tbl <- renderDT({ q <- sel_q(); state(); if (is.null(q)) return(NULL); dt(nx$query_audit(q$query_id)) })
  observeEvent(input$q_apply, {
    q <- sel_q(); req(q)
    if (!is.null(notify_err(nx$transition_query(q$query_id, input$q_new_status, input$q_comment, actor)))) {
      updateTextAreaInput(session, "q_comment", value = "")
      showNotification(paste("Query moved to", input$q_new_status))
    }
    touch()
  })
  observeEvent(input$q_add_comment, {
    q <- sel_q(); req(q, nzchar(input$q_comment))
    notify_err(nx$comment_query(q$query_id, input$q_comment, actor))
    updateTextAreaInput(session, "q_comment", value = "")
    touch()
  })
  output$drw_state <- renderUI({
    state()
    cfg <- nx$ledger$get_setting("drw", list())
    links <- nx$drw_links()
    if (!isTRUE(cfg$enabled)) return(helpText("Off. Requires the CDMSTAT DRW External Module in REDCap."))
    tags$small(sprintf("On (module %s): %d linked queries", cfg$prefix, sum(!is.na(links$status_id))))
  })
  observeEvent(input$drw_enable, {
    s <- notify_err(nx$drw_enable(actor = actor))
    if (!is.null(s)) showNotification(sprintf("DRW write-back enabled (token user %s)", s$user))
    touch()
  })
  observeEvent(input$drw_sync, {
    r <- notify_err(nx$drw_sync(actor))
    if (!is.null(r)) showNotification(sprintf("DRW: %d opened, %d updated, %d site updates pulled, %d imported%s",
      r$opened, r$updated, r$pulled, r$imported, if (length(r$errors)) paste0(", ", length(r$errors), " errors") else ""),
      type = if (length(r$errors)) "warning" else "message")
    touch()
  })
  observeEvent(input$writeback, {
    n <- notify_err(nx$writeback())
    if (!is.null(n)) showNotification(sprintf("Wrote open-query counts to %d REDCap record/event(s)", n))
    touch()
  })

  # ---- Explore
  ex_data <- reactive({
    state(); req(input$ex_x, input$ex_y)
    d <- nx$shape("long", "drop", log = FALSE)
    if (nzchar(input$ex_event %||% "")) d <- d[d$redcap_event_name == input$ex_event, , drop = FALSE]
    o <- detect_outliers(d, input$ex_y, nx$id_field(), input$ex_method, input$ex_thr)
    data.frame(record_id = d[[nx$id_field()]], event = d$redcap_event_name %||% NA,
               x = suppressWarnings(as.numeric(d[[input$ex_x]])), y = o$value, score = o$score,
               flag = ifelse(o$outlier, "Outlier", "Within range"), stringsAsFactors = FALSE)
  })
  output$ex_plot <- renderPlotly({
    d <- ex_data()
    d <- d[!is.na(d$x) & !is.na(d$y), ]
    d$key <- paste(d$record_id, d$event, sep = "|")
    plot_ly(d, x = ~x, y = ~y, color = ~flag, colors = c("Outlier" = "#d1495b", "Within range" = "#2c5d8f"),
            customdata = ~key, source = "explore", type = "scatter", mode = "markers",
            marker = list(size = 9, opacity = .8),
            text = ~sprintf("Record %s<br>%s<br>score %s", record_id, event, score), hoverinfo = "text+x+y") |>
      layout(xaxis = list(title = input$ex_x), yaxis = list(title = input$ex_y)) |>
      event_register("plotly_click")
  })
  output$ex_out <- renderDT({ d <- ex_data(); dt(d[d$flag == "Outlier", c("record_id", "event", "x", "y", "score")]) })
  observeEvent(event_data("plotly_click", source = "explore"), {
    ev <- event_data("plotly_click", source = "explore")
    key <- strsplit(ev$customdata, "|", fixed = TRUE)[[1]]
    d <- ex_data()
    row <- d[d$record_id == key[1] & d$event == key[2], ][1, ]
    showModal(modalDialog(
      title = sprintf("Raise query: record %s (%s)", key[1], key[2]),
      radioButtons("mq_field", "Field", c(input$ex_y, input$ex_x), inline = TRUE),
      textAreaInput("mq_msg", "Query text", rows = 3, value = sprintf(
        "Please verify %s = %s (outlier score %s in %s).", input$ex_y, row$y, row$score, key[2])),
      footer = tagList(modalButton("Cancel"), actionButton("mq_send", "Raise query", class = "btn-warning"))
    ))
    session$userData$mq <- list(record = key[1], event = key[2], row = row)
  })
  observeEvent(input$mq_send, {
    m <- session$userData$mq
    val <- if (input$mq_field == input$ex_y) m$row$y else m$row$x
    id <- notify_err(nx$add_query(m$record, input$mq_field, input$mq_msg, event = m$event, value = val,
                                  source = "analysis", actor = actor))
    removeModal()
    if (!is.null(id)) showNotification(paste("Query", id, "raised and sent to the DM board"), type = "warning")
    touch()
  })

  # ---- Wrangle
  wr <- reactive({ state(); nx$shape(input$wr_long, input$wr_rep, log = FALSE) })
  output$wr_dim <- renderText(sprintf("Variables: %d columns x %d rows", ncol(wr()), nrow(wr())))
  output$wr_vars <- renderDT(dt(attr(wr(), "variables")[, c("name", "type", "label", "event")]))
  output$wr_preview <- renderDT(datatable(utils::head(wr(), 200), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10)))
  output$wr_dl <- downloadHandler(
    filename = function() sprintf("cdmstat_%s_%s.csv", input$wr_long, input$wr_rep),
    content = function(file) {
      nx$log_code(sprintf("df <- nx$shape(longitudinal = %s, repeating = %s)", deparse(input$wr_long), deparse(input$wr_rep)), actor)
      utils::write.csv(wr(), file, row.names = FALSE)
    })

  # ---- Statistics
  t1 <- reactive({
    state(); req(input$t1_event)
    notify_err(nx$table_one(by = if (nzchar(input$t1_by %||% "")) input$t1_by else NULL, event = input$t1_event))
  })
  output$t1_tbl <- renderDT({
    t <- t1(); req(t)
    datatable(t[, setdiff(names(t), "variable")], rownames = FALSE,
              options = list(pageLength = 100, dom = "t", ordering = FALSE, scrollX = TRUE))
  })
  output$t1_dl <- downloadHandler(filename = "table1.csv", content = function(file) utils::write.csv(t1(), file, row.names = FALSE))
  tr <- reactive({ state(); req(input$tr_field); nx$trend(input$tr_field) })
  output$tr_plot <- renderPlotly({
    d <- tr(); req(nrow(d))
    plot_ly(d, x = ~event, y = ~mean, color = ~group, type = "scatter", mode = "lines+markers",
            error_y = ~list(array = 1.96 * se, type = "data")) |>
      layout(yaxis = list(title = paste("Mean", input$tr_field, "(95% CI)")), xaxis = list(title = ""))
  })
  output$tr_tbl <- renderDT(dt(transform(tr(), mean = round(mean, 2), sd = round(sd, 2), se = round(se, 2),
                                         lower = round(lower, 2), upper = round(upper, 2))))
  fit <- eventReactive(input$m_run, notify_err(nx$fit(input$m_formula, input$m_type)))
  output$m_info <- renderText({
    f <- fit(); req(f)
    paste0(sprintf("%s model, n = %s", attr(f, "type"), attr(f, "n")),
           if (!is.null(attr(f, "groups"))) sprintf(", %s subjects; p-values: %s", attr(f, "groups")[1], attr(f, "p_method")) else "")
  })
  output$m_tbl <- renderDT({ f <- fit(); req(f); f[] <- lapply(f, function(v) if (is.numeric(v)) signif(v, 4) else v); dt(f) })

  # ---- Cuts
  cuts <- reactive({ state(); nx$cuts() })
  output$cuts_tbl <- renderDT({ d <- cuts(); d$sha256 <- substr(d$sha256, 1, 16); dt(d[, c("cut_id", "label", "created_by", "created_at", "n_records", "n_rows", "sha256", "signed_off_by", "signed_off_at")]) })
  sel_cut <- reactive({ i <- input$cuts_tbl_rows_selected; if (length(i)) cuts()$cut_id[i] else NULL })
  observeEvent(input$cut_freeze, { id <- notify_err(nx$freeze(input$cut_label, input$cut_note, actor)); if (!is.null(id)) showNotification(paste("Frozen", id)); touch() })
  observeEvent(input$cut_sign, { req(sel_cut()); notify_err(nx$sign_off(sel_cut(), input$cut_sign_comment, actor)); touch() })
  observeEvent(input$cut_use, { req(sel_cut()); notify_err(nx$use_cut(sel_cut())); touch() })
  observeEvent(input$cut_live, { nx$use_cut(NULL); touch() })
  output$cut_active <- renderUI({
    state()
    a <- nx$active_cut()
    if (is.null(a)) return(p(class = "mt-3", "Analysing live data"))
    p(class = "mt-3", sprintf("Analysing cut %s (%s)", a$id, a$label))
  })

  # ---- Script
  output$script_txt <- renderText({ state(); nx$script() })
  output$script_dl <- downloadHandler(filename = "cdmstat_analysis.R", content = function(file) writeLines(nx$script(), file))
  # ---- CDMS modules
  records <- reactive({ state(); ids <- unique(nx$raw[[nx$id_field()]]); ids[order(suppressWarnings(as.numeric(ids)), ids)] })
  observe({
    ids <- records()
    for (inp in c("rz_record", "lk_record", "dv_record")) updateSelectizeInput(session, inp, choices = ids, server = TRUE)
    md <- nx$metadata
    cats <- md$field_name[md$field_type %in% c("radio", "dropdown", "yesno")]
    updateSelectizeInput(session, "rz_strata", choices = cats, selected = isolate(input$rz_strata))
    updateSelectizeInput(session, "sdv_crit", choices = md$field_name,
                         selected = isolate(input$sdv_crit) %||% intersect(c("arm", "ae_serious", "event_occurred"), md$field_name))
    txt <- md$field_name[md$field_type %in% c("text", "notes") & !grepl("^(integer|number|date)", md$text_validation_type_or_show_slider_number)]
    updateSelectInput(session, "cd_field", choices = txt, selected = keep_or(isolate(input$cd_field), "ae_term", txt))
  })

  rz <- reactive({ state(); nx$randomization_status() })
  output$rz_balance <- renderDT({ r <- rz(); if (!isTRUE(r$configured)) return(dt(data.frame(status = "Not configured"))); dt(r$balance) })
  output$rz_alloc <- renderDT({ r <- rz(); req(isTRUE(r$configured)); dt(r$allocations) })
  observeEvent(input$rz_setup, {
    blocks <- as.integer(strsplit(input$rz_blocks, "[, ]+")[[1]])
    if (!is.null(notify_err(nx$setup_randomization(strata = input$rz_strata %||% character(), block_sizes = blocks,
                                                   per_stratum = input$rz_per, actor = actor)))) showNotification("Schedule generated")
    touch()
  })
  observeEvent(input$rz_go, {
    r <- notify_err(nx$randomize(input$rz_record, actor = actor))
    if (!is.null(r)) showModal(modalDialog(title = "Randomized",
      sprintf("Record %s allocated to %s (stratum %s, slot %s)%s.", r$record_id, r$arm_label, r$stratum, r$seq,
              if (r$written_to_edc) " and written to the EDC" else "; record the arm in the EDC manually"), easyClose = TRUE))
    touch()
  })

  sdv <- reactive({ state(); nx$sdv_status(required_only = isTRUE(input$sdv_req)) })
  output$sdv_tbl <- renderDT(datatable(sdv()[, c("unit_key", "site", "record_id", "event", "form", "required", "reason", "status", "verified_by", "verified_at")],
                                       rownames = FALSE, filter = "top", selection = "multiple", options = list(pageLength = 15, scrollX = TRUE)))
  observeEvent(input$sdv_plan, { notify_err(nx$sdv_plan(input$sdv_crit %||% character(), input$sdv_first, input$sdv_rate, actor = actor)); touch() })
  observeEvent(input$sdv_verify, {
    i <- input$sdv_tbl_rows_selected; req(length(i))
    notify_err(nx$sdv_verify(sdv()$unit_key[i], actor = actor))
    showNotification(sprintf("%d unit(s) verified", length(i))); touch()
  })

  locks <- reactive({ state(); nx$locks() })
  output$lk_tbl <- renderDT(dt(locks()[, c("lock_id", "record_id", "event", "form", "reason", "locked_by", "locked_at", "integrity")]))
  output$sg_tbl <- renderDT({ state(); dt(nx$signatures()[, c("sig_id", "record_id", "signer", "meaning", "signed_at", "status")]) })
  observeEvent(input$lk_go, { notify_err(nx$lock(input$lk_record, input$lk_reason, actor = actor)); touch() })
  observeEvent(input$ul_go, { i <- input$lk_tbl_rows_selected; req(length(i)); notify_err(nx$unlock(locks()$lock_id[i], input$ul_reason, actor = actor)); touch() })
  observeEvent(input$sg_go, {
    if (!identical(trimws(input$sg_user), actor)) { showNotification("User name does not match the signed-in user", type = "error"); return() }
    if (!is.null(notify_err(nx$esign(input$lk_record, input$sg_meaning, actor = actor, identity_verified = TRUE)))) showNotification("Signed")
    updateTextInput(session, "sg_user", value = "")
    touch()
  })

  coding <- reactive({ state(); nx$coding() })
  output$cd_tbl <- renderDT(dt(coding()[, c("status", "verbatim", "term", "code", "suggestion_term", "suggestion_code", "score", "record_id", "event", "approved_by")]))
  observeEvent(input$cd_run, {
    r <- notify_err(nx$code_terms(input$cd_field, actor = actor))
    if (!is.null(r)) showNotification(sprintf("%d auto-coded, %d for review", r$auto_coded, r$for_review)); touch()
  })
  observeEvent(input$cd_dict, { n <- notify_err(nx$load_dictionary(input$cd_dict$datapath, actor = actor)); if (!is.null(n)) showNotification(paste(n, "terms loaded")); touch() })
  observeEvent(input$cd_approve, { i <- input$cd_tbl_rows_selected; req(length(i)); notify_err(nx$approve_coding(coding()$coding_key[i], actor = actor)); touch() })
  observeEvent(input$cd_assign, {
    i <- input$cd_tbl_rows_selected; req(length(i), nzchar(input$cd_code))
    notify_err(nx$approve_coding(coding()$coding_key[i], code = input$cd_code, actor = actor)); touch()
  })

  devs <- reactive({ state(); nx$deviations() })
  output$dv_tbl <- renderDT(dt(devs()[, c("dev_id", "status", "severity", "category", "record_id", "event", "description", "source", "reported_by", "reported_at", "closed_by")]))
  observeEvent(input$dv_check, { n <- notify_err(nx$check_visit_windows(window = input$dv_window, actor = actor)); if (!is.null(n)) showNotification(paste(n, "new deviation(s)")); touch() })
  observeEvent(input$dv_log, { req(nzchar(input$dv_desc)); notify_err(nx$log_deviation(input$dv_record, input$dv_cat, input$dv_desc, input$dv_sev, actor = actor)); touch() })
  observeEvent(input$dv_close, { i <- input$dv_tbl_rows_selected; req(length(i)); notify_err(nx$close_deviation(devs()$dev_id[i], input$dv_close_c, actor = actor)); touch() })

  output$sm_tbl <- renderDT({ state(); dt(nx$site_metrics()) })
  output$sm_enrol <- renderPlotly({
    state(); e <- nx$enrollment_curve(); req(nrow(e))
    plot_ly(e, x = ~date, y = ~cumulative, color = ~site, type = "scatter", mode = "lines", line = list(shape = "hv")) |>
      layout(yaxis = list(title = "Subjects enrolled"), xaxis = list(title = ""))
  })

  sdtm <- reactive({ state(); notify_err(nx$sdtm()) })
  output$sd_dm <- renderDT(dt(sdtm()$DM))
  output$sd_vs <- renderDT(dt(sdtm()$VS))
  output$sd_lb <- renderDT(dt(sdtm()$LB))
  output$sd_ae <- renderDT(dt(sdtm()$AE))
  output$sd_sv <- renderDT(dt(sdtm()$SV))
  output$sd_ds <- renderDT(dt(sdtm()$DS))
  output$cdisc_zip <- downloadHandler(filename = function() sprintf("cdisc_%s.zip", format(Sys.Date())), content = function(file) {
    d <- tempfile("cdisc")
    f <- nx$export_cdisc(d)
    old <- setwd(d); on.exit(setwd(old))
    utils::zip(file, basename(f), flags = "-q")
  })
  output$act_tbl <- renderDT({ state(); dt(nx$activity(n = 1000)) })

  # ---- Connection
  output$cx_current <- renderUI({
    state()
    s <- nx$summary()
    cfg <- cdmstat_connection(nx$dir)
    tags$dl(tags$dt("Source"), tags$dd(s$source),
            tags$dt("Project"), tags$dd(sprintf("%s (PID %s)", s$project, s$project_id)),
            tags$dt("API URL"), tags$dd(cfg$url %||% (if (inherits(nx$backend, "RedcapBackend")) nx$backend$url else "-")),
            tags$dt("Records / last sync"), tags$dd(sprintf("%s / %s", s$records, s$last_sync)),
            tags$dt("Data folder"), tags$dd(code(nx$dir)))
  })
  cx_check <- reactiveVal(NULL)
  observeEvent(input$cx_test, {
    req(nzchar(input$cx_url), nzchar(input$cx_token))
    cx_check(notify_err(redcap_check(input$cx_url, token = trimws(input$cx_token))))
  })
  output$cx_result <- renderTable({
    r <- cx_check(); req(r)
    data.frame(Result = r$result, Check = r$check, Detail = r$detail)
  }, striped = TRUE, width = "100%")
  observeEvent(input$cx_save, {
    req(nzchar(input$cx_url), nzchar(input$cx_token))
    new <- notify_err(withProgress(message = "Connecting and downloading data", {
      cdmstat_setup(input$cx_url, token = trimws(input$cx_token), store = input$cx_store, actor = actor)
    }))
    if (is.null(new)) return()
    nx <<- new
    updateTextInput(session, "cx_token", value = "")
    showNotification(sprintf("Connected to %s: %d records", new$info$project_title, new$summary()$records), duration = 8)
    touch()
  })

}

shinyApp(ui, server)
