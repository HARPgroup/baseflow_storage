#'@title summarize_event
#'@name
#'summarize_event
#'@description
#'Creates new df with analysis_data, AGWRC and R.squared
#'@details
#'Uses \code{calc_event_stats()} and, ultimately, \code{bf_event_stats()} to
#'format event data to remove out-of-range data and calculate log-linear
#'regression of baseflow recession for one or all events in the user provided
#'data frame analysis_data
#'@param analysis_data df with Date, Flow, AGWR, delta_AGWR as minimum present
#'  columns often calculated by \code{agws::analyze_recession()}
#'@param event_number Numeric. The event to analyze. If 0, this function will
#'  analyze all events via \code{for} loop
#'@param dAGWR_range numeric. The allowable range of the change in AGWR (the
#'  ratio of today's and yesterday's flow). This value is added and subtracted
#'  from 1.0 to set the range e.g. the default value of 0.03 allows dAGWR from
#'  0.97 to 1.03
#'@return data.frame with all columns of analysis_data and now appended with
#'  fields of calc_AGWRC as the slope of the event log-linear regression of
#'  log(Q) ~ Date and the corresponding R squared
#'@export
summarize_event <- function(analysis_data,
                            event_number = 0,
                            dAGWR_range = 0.03){
  data <- analysis_data
  eventnum <- event_number
  dAGWRmax <- 1 + dAGWR_range
  dAGWRmin <- 1 - dAGWR_range

  #new (for integration with app)
  max_gid <- max(data$GroupID, na.rm = TRUE)

  #Error checking
  if (is.na(eventnum) || is.na(max_gid) || eventnum > max_gid) {
    return(NULL)
  }

  # Warning for event number not existing in data
  if(eventnum > max(data$GroupID)){
    stop("Event does not exist, choose a lower number")
  }

  # Condition to produce rsquared and agwr for all events
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
