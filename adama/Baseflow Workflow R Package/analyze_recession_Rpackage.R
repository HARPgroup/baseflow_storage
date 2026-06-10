#'@title analyze_recession
#'@name
#'analyze_recession
#'@description
#'Consolidates flow_csv data into recession events
#'@details
#'Separates recession events and finds start and end date for each event.
#'Duration and days between are calculated. Events are categorized by group ID. 
#'Output df only includes recession events, non-events are removed.
#'
#'@param df Applicable df with RecessionDay column || flow_csv df with RecessionDay column
#'@param site_name Char value with site name
#'@param min_len Num value for minimum number of consecutive RecessionDays to be consider an event, default is 0
#'@param max_len Num value for maximum number of consecutive RecessionDays to be consider an event, default is Inf
#'@return Outputs summarized df including only recession events grouped by ID
#'@author 
#'@examples 
#'
#'@export
analyze_recession <- function(
  #df with RecessionDay column  
  df, 
  #Char with site name
  site_name = "", 
  #Num value for minimum number of consecutive RecessionDays to be considered an event, default is 0
  min_len = 0, 
  #Num value for maximum number of consecutive RecessionDays to be considered an event, default is Inf
  max_len = Inf) {
  #Creates lengths and value columns based on consecutive Recession Days
  rle_out <- rle(df$RecessionDay)
  lengths <- rle_out$lengths
  values <- rle_out$values
  #Cumulative summation of lengths column, assigned to ends
  ends <- cumsum(lengths)
  #First starts value is 1, then next starts value is 1 + ends[1], for length of ends column. Assigned to starts
  starts <- c(1, head(ends, -1) + 1)
  
  #Creates column of NA for amount of rows in df, assigned to group_id
  group_id <- rep(NA, nrow(df))
  #Assigns value of 1 to group_counter
  group_counter <- 1
  
  #For all indices of lengths
  for (i in seq_along(lengths)) {
  #If index values is true and index lengths is >= min_len & <= max_len
    if (values[i] && lengths[i] >= min_len && lengths[i] <= max_len) {
  #Creates sequence of indices from starts[i] to ends[i], assigns group_counter value to group_id
      group_id[starts[i]:ends[i]] <- group_counter
  #Adds 1 to group_counter every separate recession event
      group_counter <- group_counter + 1
    }
  }
  #group_id values assigned to GroupID column in df
  df$GroupID <- group_id
  
  #Looks for all non-NA values in group_id[starts] and keeps those indices for each starts index 
  recession_starts <- starts[!is.na(group_id[starts])]
  #Looks for all non-NA values in group_id[starts] and keeps those indices for each ends index 
  recession_ends   <- ends[!is.na(group_id[starts])]
  
  #Creates a df including unique, non-NA group_id, StartDates and EndDates for each group_id
  recession_event_df <- data.frame(
    GroupID   = unique(na.omit(group_id)),
    StartDate = df$Date[recession_starts],
    EndDate   = df$Date[recession_ends]
  )
  
  #Calculates duration of recession events through differnce of EndDate and StartDate + 1
  recession_event_df$Duration <- as.numeric(recession_event_df$EndDate - recession_event_df$StartDate) + 1
  #Calculates days between recession events through finding difference in days between StartDate and EndDate.
  #First and last indices are excluded respectively
  recession_event_df$DaysBetween <- c(NA, as.numeric(difftime(recession_event_df$StartDate[-1],
                                                              recession_event_df$EndDate[-nrow(recession_event_df)],
                                                              units = "days")))
  
  #Finds highest number value from Duration columns, assigned to max_len
  max_len <- max(recession_event_df$Duration, na.rm = TRUE)
  #Finds index where longest recession event occurs in Duration, assigned to longest
  longest <- which.max(recession_event_df$Duration)
  #Prints to console
  cat("\n===== Recession Analysis for", site_name, "=====\n")
  cat("Longest recession lasted", max_len, "days\n")
  cat("From", as.character(recession_event_df$StartDate[longest]), "to",
      as.character(recession_event_df$EndDate[longest]), "\n")
  
  #Creates data structure
  list(df = df, summary = recession_event_df)
}

