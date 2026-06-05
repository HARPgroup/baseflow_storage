## ui.R

ui <- fluidPage(
  titlePanel("Baseflow Events & Drought Outlook (Prototype)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select Data Source"),
      selectInput("site_choice","Select USGS Gage:",
                  choices = paste(all_usgs_gages$site_no,"-",all_usgs_gages$station_nm),
                  selected = "01631000 - S F SHENANDOAH RIVER AT FRONT ROYAL, VA"),
      tags$hr(),
      helpText(
        "This prototype can load:",
        tags$ul(
          tags$li("Raw data (for historical flow overlays): Gage flows from USGS (dataRetrieval)."),
          tags$li("Analyzed event data (for baseflow events/AGWRC): pulled from GitHub.")
        )
      )
    ),
    
    mainPanel(
      droughtModuleUI("drought")
    )
  )
)


