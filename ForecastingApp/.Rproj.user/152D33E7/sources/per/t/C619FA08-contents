## modules/droughtModuleUI.R

droughtModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Drought Forecast & Baseflow Events"),
    
    tabsetPanel(
      id = ns("main_tabs"),
      
      tabPanel(
        "Overview & Events",
        br(),
        h4("Historical Flow (Recent)"),
        plotlyOutput(ns("historical_plot")),
        br(),
        h4("Identified Baseflow Events (Trimmed)"),
        p("Each row summarizes a trimmed baseflow recession event (GroupID)."),
        DTOutput(ns("events_table")),
        br(),
        actionButton(ns("inspect_event"), "Inspect Selected Event")
      ),
      
      tabPanel(
        "AGWRC vs Flow Regression",
        br(),
        h4("Event-Level AGWRC vs Characteristic Flow"),
        plotlyOutput(ns("agwrc_regression_plot")),
        br(),
        verbatimTextOutput(ns("regression_summary"))
      ),
      
      tabPanel(
        "Drought Forecast",
        br(),
        fluidRow(
          column(
            4,
            h4("Forecast Controls"),
            dateInput(
              ns("forecast_start"),
              label = "Projection start date (must exist in historical data):",
              value = Sys.Date()
            ),
            numericInput(
              ns("agwrc_single"),
              label = "AGWRC (single daily ratio)",
              value = 0.97,
              min = 0.0,
              max = 1.2,
              step = 0.001
            ),
            helpText("Future Q_t+Δ ≈ Q_start × AGWRC^Δ, where Δ is days."),
            tags$hr(),
            p(em("Future enhancement: allow AGWRC vs Flow matrix input ",
                 "to use flow-dependent AGWRC lookup instead of a single value."))
          ),
          column(
            8,
            h4("Historical + Projected Flow"),
            plotlyOutput(ns("forecast_plot")),
            br(),
            h4("Projected Flows"),
            DTOutput(ns("forecast_table"))
          )
        )
      )
    )
  )
}
