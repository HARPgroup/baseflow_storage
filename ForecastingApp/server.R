#server.R
server <- function(input, output, session) {
  
  # map the site choice to a gage_id (used for gage raw data + both analyses)
  gage_obj <- reactiveVal(NULL)
  observe({
    gage_id <- gsub("^([0-9]+) -.*","\\1",input$site_choice)
    gage <- hydrotools::WaterGageBase$new(
      ds_in = ds,
      gage_id = gage_id
    )
    #Add drainage area
    gage$load_sf_da()
    gage_obj(gage)
  })

  droughtModuleServer(
    id          = "drought",
    gage_obj     = gage_obj
  )
}