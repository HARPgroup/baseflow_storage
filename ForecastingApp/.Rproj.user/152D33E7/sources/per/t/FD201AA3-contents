## ui.R

ui <- fluidPage(
  titlePanel("Baseflow Events & Drought Outlook (Prototype)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select USGS Gage"),
      radioButtons(
        "site_choice",
        label   = NULL,
        choices = c("Cootes Store", "Mount Jackson", "Strasburg"),
        selected = "Cootes Store",
        inline  = FALSE
      ),
      tags$hr(),
      helpText("This prototype expects daily flow (Q) with AGWR/AGWRC analysis.",
               "Historical data are from *_original_analysis_df.",
               "Event stats are from *_trimmed_event_results.")
    ),
    
    mainPanel(
      droughtModuleUI("droughtModule")
    )
  )
)

