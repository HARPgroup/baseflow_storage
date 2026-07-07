# Function to produce plots and summary statistics
# analysis_data = dataframe produced by MainAnalysis.R script (l. 161-163)
# event_number = desired event for summary, or leave blank to get summary data for all events
# dAGWR_range = range, in either direction from 1, that is a valid dAGWR
summarize_event <- function(analysis_data,
                            event_number = 0,
                            dAGWR_range = 0.03){
  # source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/bf_event_stats.R")
  # source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/calc_event_stats.R")
  
  require(sqldf)
  
  data <- analysis_data
  eventnum <- event_number
  dAGWRmax <- 1 + dAGWR_range
  dAGWRmin <- 1 - dAGWR_range
  
  #new (for integration with app)
  max_gid <- suppressWarnings(max(data$GroupID, na.rm = TRUE))
  #slight change (for integration with app)
  if (is.na(eventnum) || is.na(max_gid) || eventnum > max_gid) {
    return(NULL)
  }
  
  # Warning for event number not existing in data
  if(eventnum > max(data$GroupID)){
    stop("Data does not contain enough events, choose a lower number")
  }
  
  # COndition to produce rsquared and agwr for all events
  if(eventnum == 0){
    # Create empty dataframe
    event_data <- data.frame(site_no = character(), Date = character(),
                             Flow = numeric(), AGWR  = numeric(),
                             delta_AGWR = numeric(), Year = integer(),
                             Month = integer(), Day = integer(),
                             season = character(), GroupID = integer(),
                             calc_AGWRC = numeric(), R_squared = numeric(),
                             stringsAsFactors = FALSE)
    
    for(i in (1:max(data$GroupID))){
      
      new_row <- calc_event_stats(data, event_num = i, dAGWRmax, dAGWRmin)
      
      event_data <- rbind(event_data, new_row)
      
    }
    
  } else {
    
    event_data <- calc_event_stats(data, event_number, dAGWRmax, dAGWRmin)
  
    names(event_data)[names(event_data)=="GroupID"] <- "i"
    
    
  }
  
  return(event_data)
  
}