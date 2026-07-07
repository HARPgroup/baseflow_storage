### This function determines the gage length ###

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