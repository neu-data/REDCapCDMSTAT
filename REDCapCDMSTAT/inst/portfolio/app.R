library(shiny)
library(bslib)
library(DT)
library(plotly)
library(REDCapCDMSTAT)

dirs <- getOption("REDCapCDMSTAT.portfolio")
if (is.null(names(dirs))) names(dirs) <- basename(dirs)

dt <- function(df) datatable(df, rownames = FALSE, filter = "top", options = list(pageLength = 20, scrollX = TRUE, dom = "tip"))

ui <- page_navbar(
  title = "REDCap CDMSTAT portfolio",
  theme = bs_theme(version = 5, primary = "#2c5d8f", "navbar-bg" = "#1f3b57",
                   base_font = font_collection("Inter", "Segoe UI", "system-ui", "sans-serif")),
  nav_panel("Studies",
    card(card_header("All studies"), DTOutput("studies")),
    card(card_header("Queries by status"), plotlyOutput("qplot", height = 300))),
  nav_panel("Sites", card(card_header("Site metrics across studies"), DTOutput("sites"))),
  nav_spacer(),
  nav_item(actionButton("refresh", "Refresh", icon = icon("rotate"), class = "btn-sm"))
)

server <- function(input, output, session) {
  pf <- reactive({
    input$refresh
    invalidateLater(60000)
    cdmstat_portfolio(as.list(dirs))
  })
  output$studies <- renderDT(dt(pf()$studies))
  output$sites <- renderDT(dt(pf()$sites))
  output$qplot <- renderPlotly({
    q <- pf()$queries_by_status
    req(NROW(q))
    plot_ly(q, x = ~project, y = ~Freq, color = ~status, type = "bar") |>
      layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "Queries"))
  })
}

shinyApp(ui, server)
