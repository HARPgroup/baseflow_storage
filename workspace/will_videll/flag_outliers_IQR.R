#' @title flag_outliers_IQR
#' @name flag_outliers_IQR
#' @details
#' Adds column to flag events outside acceptable IQR range.
#'
#' @param event_df df from step 06 of DEQ bf workflow
#'
#' @returns event_df data frame of recorded
#' @export flag_outliers_IQR
flag_outliers_IQR <- function(event_df) {
  # calculate Q1, Q3, and IQR
  Q1 = quantile(event_df$logQ, 0.25, na.rm = TRUE)
  Q3 = quantile(event_df$logQ, 0.75, na.rm = TRUE)
  IQR = Q3 - Q1
  lower = Q1 - 1.5 * IQR
  upper = Q3 + 1.5 * IQR
  # return the number of values flagged as outliers
  n_flagged_msg <- paste0("IQR-based detection determines ",
                          sum(event_df$logQ < lower |
                                event_df$logQ > upper,
                              na.rm = TRUE),
                          " log median flow value(s) as outlier(s)"
  )
  # create a string of flagged flow values
  flagged_values <- event_df$logQ[
    event_df$logQ < lower |
      event_df$logQ > upper
  ]
  # create a second message to convey flagged values if applicable
  if (length(flagged_values) > 0) {
    flagged_val_msg <- paste0("The value(s) flagged as outlier(s) are: ",
                              paste(flagged_values, collapse = ", ")
    )
  } else {
    flagged_val_msg <- ""
  }
  # output both messages in the console
  cat(n_flagged_msg, "\n\n")
  cat(flagged_val_msg, "\n")
  # create a new column in event_df that lists T/F for outliers
  event_df <- event_df %>%
    dplyr::mutate(IQR_flagged_outlier = logQ < lower |
             logQ > upper)
  return(event_df)
}
