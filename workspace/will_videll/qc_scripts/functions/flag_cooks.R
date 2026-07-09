flag_cooks <- function(model, event_df) {
  # define the variables
  cooks_d <- cooks.distance(model)
  n <- length(cooks_d)
  threshold <- 4 / n
  # determine flagged values
  flagged_cooks <- which(cooks_d > threshold)
  # output messages depending on values being flagged
  if (length(flagged_cooks) > 0) {
    flagged_cooks_msg <- paste0(
      "The values flagged by Cook's distance as being influential on the model relationship between log flow and AGWRC are: ",
      paste(event_df$logQ[flagged_cooks], collapse = ", ")
    )
  } else {
    flagged_cooks_msg <- "No values were flagged as being influential on the model relationship between log flow and AGWRC"
  }
  # output message in the console
  cat(flagged_cooks_msg, "\n\n")
  # create a new column for cooks distance for each data point
  event_df <- event_df %>%
    mutate(cooks_distance = cooks_d)
  # create a new column with T/F for flagged points
  event_df <- event_df %>%
    mutate(cooks_flagged = cooks_d > threshold)
  return(event_df)
}
