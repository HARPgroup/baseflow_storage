# #Local Testing
# library(tidyverse)
# 
# GageID = "02039500" #Appomattox River at Farmville, VA
# flow_csv <- read_csv(paste0("https://deq1.bse.vt.edu/usgs/agws/", GageID, "-flow.csv"))
# baseflow_csv <- read_csv(paste0("https://deq1.bse.vt.edu/usgs/agws/baseflow_trimmed_stats_", GageID, ".csv"))
# precip_daily_csv <- read_csv(paste0("https://deq1.bse.vt.edu/met/PRISM/precip/usgs_ws_", GageID, "_precip_daily.csv"))
# 
# baseflow_csv <- baseflow_csv %>%
#   mutate(Date = as.Date(Date)) %>%
#   select(Date, Flow, GroupID, AGWRC)
# 
# precip_daily_csv <- precip_daily_csv %>%
#   mutate(obs_date = as.Date(obs_date)) %>%
#   select(obs_date, precip_in, precip_cfs, area_sqmi)
#
#'@title join_precip_data
#'@name
#'join_precip_data
#'@description
#'Joins in PRISM precip data to the baseflow trimmed stats by date and summerizes the 
#'precip stats for each baseflow event by groupID. 
#'@details
#'A 3-day antecedent precipitation is also calculated for each baseflow event 
#'and classifed as dry, moderate, and wet beased on the amount of precip. 
#'@param baseflow_csv data.frame with Date, Flow, GroupID, and AGWRC
#'@param precip_daily_csv data.frame with obs_Date, precip_in, precip_cfs, area_sqmi
#'@return list with 2 data.frame. join_precip_df with Date, Flow, GroupID, AGWRC, 
#'precip_in, precip_cfs, and area_sqmi and event_summary with GroupID, start_date, end_date, duration,
#'total_precip, max_precip, event_AGWRC, antecedent_3_day, and precip_class
#'@export
join_precip_data <- function(baseflow_csv, precip_daily_csv) {
  
  join_precip_df <- left_join(
    baseflow_csv,
    precip_daily_csv,
    by = c("Date" = "obs_date")
  ) %>%
    filter(!is.na(precip_in))
  
  #precip by baseflow event  
  event_summary <- join_precip_df %>%
    group_by(GroupID) %>%
    summarize(
      start_date = min(Date),
      end_date = max(Date),
      duration = n(),
      
      total_precip = sum(precip_in, na.rm = TRUE),
      max_precip = max(precip_in, na.rm = TRUE),
      mean_precip = mean(precip_in, na.rm = TRUE),
      event_AGWRC = mean(AGWRC, na.rm = TRUE),
      .groups = "drop"
    )
  # Calculate 3-day antecedent precipitation
  event_summary <- event_summary %>%
    rowwise() %>%
    mutate(
      antecedent_3_day = sum(
        precip_daily_csv$precip_in[precip_daily_csv$obs_date >= start_date - 3 & precip_daily_csv$obs_date < start_date],
        na.rm = TRUE
      )
    ) %>%
    ungroup()
  
  event_summary <- event_summary %>%
    mutate(
      precip_class = case_when(
        antecedent_3_day < 0.10 ~ "Dry",
        antecedent_3_day < 0.25 ~ "Moderate",
        TRUE ~ "Wet"
      )
    )
  
  return(list(join_precip_df = join_precip_df,event_summary = event_summary))
}

# #Local tesing
# results <- join_precip_data(baseflow_csv, precip_daily_csv)
# join_precip_df <- results$join_precip_df
# event_summary <- results$event_summary
