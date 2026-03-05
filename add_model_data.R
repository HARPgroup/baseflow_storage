add_model_data <- function(timeseries_data, land_type_code, model_col, scenario="subsheds", site=omsite) {
# Timeseries data must have year, month, and day columns
   
  source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/make_model_daily.R")
  
  # Get model data from given land code and land type
  model_data <- read.csv(paste0(site,"/p6/out/land/", scenario, "/pwater/", land_type_code,"_pwater.csv"))
  
  # Make model data daily using func make_model_daily
  model_data <- make_model_daily(model_data, "index")
  
  
  # select date and column from model data
  m_col <- sqldf(sprintf(
    "select year, month, day, %s from model_data"
  , model_col))
  
  #combine m_col and original data
  final_df <- sqldf(sprintf(
    "select a.*, b.%s from
    timeseries_data as a
    left outer join m_col as b
    on( a.Year = b.year and
        a.Month = b.month and
        a.Day = b.day)"
  , model_col))

  return(final_df) 
  
}
