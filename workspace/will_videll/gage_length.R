### This function determines the gage length ###
#' @title gage_length
#' @name gage_length
#' @details
#' This function calculates the total number of daily stream flow records from
#' a given USGS gage.
#'
#' @param daily_df df of average daily stream flow values from a given USGS gage.
#'
#' @returns value representing total number of entries in the data frame.
#' @export gage_length
gage_length <- function(daily_df){
  result <- paste0(
    "This gage has ",
    length(daily_df$Flow),
    " observations."
  )

  length <- length(daily_df$Flow)
  cat(result, "\n\n")
  return(length)
}
