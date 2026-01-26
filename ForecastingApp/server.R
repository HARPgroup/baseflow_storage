#server.R
server <- function(input, output, session) {
  
  # map the radio button choice to a gage_id
  selected_gage <- reactive({
    switch(input$site_choice,
           "Cootes Store"   = "01632000",
           "Mount Jackson"  = "01633000",
           "Strasburg"      = "01634000"
    )
  })
  
  # call module, passing the reactive gage_id in
  droughtModuleServer(
    id      = "drought",
    gage_id = selected_gage
  )
}
