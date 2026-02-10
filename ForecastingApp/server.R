#server.R
server <- function(input, output, session) {
  
  # map the site choice to a gage_id (used for gage raw data + both analyses)
  selected_gage <- reactive({
    switch(input$site_choice,
           "Cootes Store"   = "01632000",
           "Mount Jackson"  = "01633000",
           "Strasburg"      = "01634000"
    )
  })
  
  selected_source <- reactive({
    # normalize to lower-case internally
    if (identical(input$data_source, "Model")) "model" else "gage"
  })
  
  droughtModuleServer(
    id          = "drought",
    gage_id     = selected_gage,
    data_source = selected_source,
    site_choice = reactive(input$site_choice)
  )
}

