## ui.R

ui <- fluidPage(
  titlePanel("Baseflow Events & Drought Outlook (Prototype)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select Data Source"),
      radioButtons(
        "data_source",
        label   = NULL,
        choices = c("Model", "Gage"),
        selected = "Gage",
        inline  = TRUE
      ),
      radioButtons(
        "site_choice",
        label   = NULL,
        choices = c("Cootes Store", "Mount Jackson", "Strasburg"),
        selected = "Cootes Store",
        inline  = FALSE
      ),
      tags$hr(),
      helpText(
        "This prototype can load:",
        tags$ul(
          tags$li("Raw data (for historical flow overlays): Model flows from GitHub; Gage flows from USGS (dataRetrieval)."),
          tags$li("Analyzed event data (for baseflow events/AGWRC): pulled from GitHub."),
          tags$li("Switch between Model and Gage to compare behavior across sources.")
        )
      )
    ),
    
    mainPanel(
      droughtModuleUI("drought")
    )
  )
)


