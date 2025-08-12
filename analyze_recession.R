analyze_recession <- function(df, site_name = "", min_len = 0, max_len = Inf) {
  rle_out <- rle(df$RecessionDay)
  lengths <- rle_out$lengths
  values <- rle_out$values
  ends <- cumsum(lengths)
  starts <- c(1, head(ends, -1) + 1)
  
  group_id <- rep(NA, nrow(df))
  group_counter <- 1
  
  for (i in seq_along(lengths)) {
    if (values[i] && lengths[i] >= min_len && lengths[i] <= max_len) {
      group_id[starts[i]:ends[i]] <- group_counter
      group_counter <- group_counter + 1
    }
  }
  require(tidyr)
  require(purrr)
  require(dplyr)
  df$GroupID <- group_id
  
  recession_starts <- starts[!is.na(group_id[starts])]
  recession_ends   <- ends[!is.na(group_id[starts])]
  
  recession_event_df <- data.frame(
    GroupID   = unique(na.omit(group_id)),
    StartDate = df$Date[recession_starts],
    EndDate   = df$Date[recession_ends]
  )
  recession_event_df$Duration <- as.numeric(recession_event_df$EndDate - recession_event_df$StartDate) + 1
  recession_event_df$DaysBetween <- c(NA, as.numeric(difftime(recession_event_df$StartDate[-1],
                                                              recession_event_df$EndDate[-nrow(recession_event_df)],
                                                              units = "days")))
  
  max_len <- max(recession_event_df$Duration, na.rm = TRUE)
  longest <- which.max(recession_event_df$Duration)
  cat("\n===== Recession Analysis for", site_name, "=====\n")
  cat("Longest recession lasted", max_len, "days\n")
  cat("From", recession_event_df$StartDate[longest], "to", recession_event_df$EndDate[longest], "\n")
  
  list(df = df, summary = recession_event_df)
}