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
        p(em("Tip:"), " click a row in the events table to zoom the hydrograph to ~9 months before the event and up to 3 months after (capped to available data)."),
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
        
        fluidRow(
          column(6, 
                 dateRangeInput(
                   inputId = ns("reg_date_range"),
                   label   = "Filter events by date range (event overlap with window):",
                   start   = Sys.Date() - 365,
                   end     = Sys.Date(),
                   format  = "yyyy-mm-dd",
                   separator = " to "
                 )
          ),
          column(6,
                 numericInput(
                   inputId = ns("regression_flow_max"),
                   label = "Use all flows below this value (cfs):",
                   value = NA
                 )
          )
        ),
        
        h4("Population Statistics for Event AGWRC"),
        p("Summary statistics for event-level AGWRC values after applying the filters above."),
        DTOutput(ns("agwrc_population_stats")),
        br(),
        
        h4("Population Statistics for Event Median Flow"),
        p("Summary statistics for median event flows after applying the filters above."),
        DTOutput(ns("median_flow_population_stats")),
        br(),
        
        plotlyOutput(ns("agwrc_regression_plot")),
        br(),
        fluidRow(
          column(
            6,
            h4("User Regression Summary:"),
            verbatimTextOutput(ns("lm_user_summary"))
          ),
          column(
            6,
            h4("WSPA Regression Summary:"),
            verbatimTextOutput(ns("lm_WSPA_summary"))
          )
        )
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
            uiOutput(ns("baseflow_event_info")),
            ),
            helpText("Last known AGWRC"),
            verbatimTextOutput(
              ns("last_known_agwrc")
            ),
            radioButtons(
              ns("agwrc_calculation"),
              label = "Will recession coefficients be constant or variable?",
              choiceNames = c("Constant (single value)", "Variable (regression)"),
              choiceValues = c("constant","variable")
            ),
            uiOutput(ns("agwrc_inputs")),
            radioButtons(
              ns("forecast_metric"),
              label = "Plot metric:",
              choices = c(
                "Flow (cfs)" = "flow",
                "Storage (in) — computed from Flow + AGWRC" = "storage"
              ),
              selected = "flow"
            ),
            helpText("Future Q_t+Δ ≈ Q_start × AGWRC^Δ, where Δ is days."),
            tags$hr(),
            p(em("Future enhancement: allow AGWRC vs Flow matrix input ",
                 "to use flow-dependent AGWRC lookup instead of a single value."))
          ),
          column(
            8,
            h4("Historical + Projected"),
            plotlyOutput(ns("forecast_plot")),
            br(),
            h4("Projected Values"),
            DTOutput(ns("forecast_table")),
            br(),
            h4("Storage summaries (from Flow + AGWRC)"),
            DTOutput(ns("storage_event_table"))
          )
        )
      )
    )
}
