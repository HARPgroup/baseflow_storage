require(shiny)
require(plotly)

#source existing functions
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/summarize_event.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/plot_event_values.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/ReviewAppFunctions.R")

#load data
df <- results$gage$df
summary_df <- ensure_review_columns(results$gage$summary)  #make sure cols exist
analysis_df <- results$gage$analysis                       #used by summarize_event()

ui <- fluidPage(
  titlePanel("Interactive Recession Review"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("jump_to", "Jump to Event (type or select):",
                     choices = summary_df$GroupID,
                     selected = summary_df$GroupID[1],
                     options = list(placeholder = 'Type or select GroupID...', create = FALSE)),
      numericInput("buffer_days", "Days of Antecedent Buffer:", value = 5, min = 0, max = 30, step = 1),
      actionButton("prev", "Previous Event"),
      actionButton("next_btn", "Next Event"),
      br(), br(),
      checkboxGroupInput("checklist", "Checklist (mark all that apply):",
                         choices = c(
                           "AGWR consistently < 1.0",
                           "delta_AGWR near 1.0",
                           "Flow declines smoothly",
                           "AGWR and delta_AGWR converge",
                           "Event duration ≥ 14 days",
                           "No disturbance/storm influence"
                         )),
      radioButtons("overall", "Overall Review:",
                   choices = c("Looks Good", "Does Not Look Good")),
      actionButton("save", "Save Review"),
      downloadButton("download_plots", "Download Plots (JPG)"),
      br(), br(),
      downloadButton("download_event_plots", "Download Event Plots + Summary (JPG)")
    ),
    mainPanel(
      plotlyOutput("flow_plot"),
      plotlyOutput("agwr_plot"),
      br(), hr(), br(),
      h3("Detailed Event Analysis"),
      fluidRow(
        column(4, h4("Event Summary"), htmlOutput("event_text")),
        column(8, plotOutput("event_plots", height = "900px"))
      )
    )
  )
)

server <- function(input, output, session) {
  current <- reactiveVal(1)
  
  observeEvent(input$next_btn, { if (current() < nrow(summary_df)) current(current() + 1) })
  observeEvent(input$prev,     { if (current() > 1)               current(current() - 1) })
  
  observe({ updateSelectizeInput(session, "jump_to", selected = summary_df$GroupID[current()]) })
  
  observeEvent(input$jump_to, {
    i <- which(summary_df$GroupID == input$jump_to)
    if (length(i) == 1) current(i)
  })
  
  observeEvent(input$save, {
    i <- current()
    checklist_vals <- if (is.null(input$checklist)) character(0) else input$checklist
    summary_df$review_checklist[[i]] <<- checklist_vals
    summary_df$overall_review[i]     <<- input$overall
    message(paste("Saved review for group", i))
  })
  
  #data slice for current event (calls helpers)
  get_event_data <- reactive({
    i <- current()
    start_date <- summary_df$StartDate[i]
    end_date   <- summary_df$EndDate[i]
    data <- event_window(df, start_date, end_date, buffer_days = input$buffer_days)
    list(data = data, start_date = start_date, end_date = end_date)
  })
  
  #plots (call the plot builder functions)
  flow_ggplot <- reactive({
    ev <- get_event_data()
    build_flow_plot(ev$data, ev$start_date, summary_df$GroupID[current()])
  })
  
  agwr_ggplot <- reactive({
    ev <- get_event_data()
    build_agwr_plot(ev$data, ev$start_date, summary_df$GroupID[current()])
  })
  
  output$flow_plot <- renderPlotly({ ggplotly(flow_ggplot()) })
  output$agwr_plot <- renderPlotly({ ggplotly(agwr_ggplot()) })
  
  #event text (use summarize_event function)
  output$event_text <- renderUI({
    i <- current()
    ev <- tryCatch(summarize_event(analysis_df, summary_df$GroupID[i]), error = function(e) NULL)
    if (is.null(ev)) return(HTML("<p><em>No summary available for this event</em></p>"))
    
    HTML(event_summary_text(summary_df$StartDate[i], summary_df$EndDate[i], ev$AGWR, ev$R2))
  })
  
  #stacked event plots
  output$event_plots <- renderPlot({
    gid <- summary_df$GroupID[current()]
    ev <- tryCatch(summarize_event(analysis_df, gid), error = function(e) NULL)
    if (!is.null(ev)) suppressWarnings(plot_event_values(analysis_df, gid))
  })
  
  #download detailed plots + summary footer
  output$download_event_plots <- downloadHandler(
    filename = function() paste0("Event_", summary_df$GroupID[current()], "_detailed_plots.jpg"),
    content = function(file) {
      gid <- summary_df$GroupID[current()]
      ev  <- summarize_event(analysis_df, gid)
      plots <- suppressWarnings(plot_event_values(analysis_df, gid))
      
      start <- summary_df$StartDate[current()]
      end   <- summary_df$EndDate[current()]
      footer <- event_summary_footer(gid, start, end, ev$AGWR, ev$R2)
      
      jpeg(file, width = 1600, height = 2000, res = 150)
      gridExtra::grid.arrange(plots, bottom = grid::textGrob(footer, gp = grid::gpar(fontsize = 12)))
      dev.off()
    }
  )
  
  #download original two plots
  output$download_plots <- downloadHandler(
    filename = function() paste0("Event_", summary_df$GroupID[current()], "_plots.jpg"),
    content = function(file) {
      g1 <- flow_ggplot() + labs(color = "Flow Point Status") + theme(legend.position = "right")
      g2 <- agwr_ggplot() + labs(color = "Threshold Status", shape = "Threshold Status") + theme(legend.position = "right")
      combined <- cowplot::plot_grid(g1, g2, ncol = 1, rel_heights = c(1, 1.1))
      
      jpeg(file, width = 1400, height = 1600, res = 150)
      grid::grid.draw(combined)
      dev.off()
    }
  )
}

shinyApp(ui, server)
