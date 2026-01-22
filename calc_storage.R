#Function to caluclate groundwater storage in inches using flow in inches and AGWRC
calc_storage <- function(data, flow_col, AGWRC_col){

  df <- data
  # Calculate Storage
  df$Storage_in <- df[[flow_col]]/(1-df[[AGWRC_col]])
  
  return(df)
}
