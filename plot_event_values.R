# Function to produce plots of data from selected days from desired event
# analysis_data = dataframe produced by MainAnalysis.R script (l. 161-163)
# event_number = desired event for plots
# dAGWR_range = range, in either direction from 1, that is a valid dAGWR
plot_event_values <- function(analysis_data,
                              event_number,
                              dAGWR_range = 0.03){
  data <- analysis_data
  eventnum <- event_number
  
  # Get event data using previous function
  event <- summarize_event(data, event_number)[[1]]
  
  # log flow plot
  a <- ggplot(data = event, mapping = aes(x = Date, y = log(Flow)))+
    geom_point(color = "dodgerblue4")+
    geom_line(color = "dodgerblue3")+
    theme_bw()
  
  # AGWR valid plot
  b <- ggplot(data = event, mapping = aes(x = Date, y = AGWR))+
    geom_point(color = "firebrick4")+
    geom_line(color = "firebrick")+
    theme_bw()
  
  # dAGWR valid plot
  c <- ggplot(data = event, mapping = aes(x = Date, y = delta_AGWR))+
    geom_point(color = "darkolivegreen")+
    geom_line(color = "darkolivegreen4")+
    theme_bw()
  
  d <- gridExtra::grid.arrange(a, b, c, ncol=1, top=paste0("Event ", event_number, " Selected Dates"))
  
  return(d)
  
}

# Example of how to run function
# plot.event.values(analysis_S, 99)
