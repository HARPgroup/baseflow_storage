## server.R

server <- function(input, output, session) {
  
  # Reactive list with $original and $trimmed for the chosen site
  site_data <- reactive({
    req(input$site_choice)
    site_data_list[[input$site_choice]]
  })
  
  # Call the drought module
  droughtModuleServer("droughtModule", site_data)
}
