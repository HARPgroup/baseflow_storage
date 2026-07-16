##Local Testing
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

join_precip_daily <- function(baseflow_csv, precip_daily_csv) {
  
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

##Local tesing 
# results <- join_precip_daily(baseflow_csv, precip_daily_csv)
# 
# join_precip_df <- results$join_precip_df
# event_summary <- results$event_summary



# #plotting Flow and Precip on the same graph
# library(dplyr)
# library(ggplot2)
# 
# start_date <- as.Date("2008-05-15")
# end_date <- as.Date("2008-06-15")   
# 
# # Filter and join
# plot_data <- flow_csv %>%
#   mutate(obs_date = as.Date(obs_date)) %>%
#   filter(obs_date >= start_date,
#          obs_date <= end_date) %>%
#   select(obs_date, obs_flow) %>%
#   left_join(
#     precip_daily_csv %>%
#       mutate(obs_date = as.Date(obs_date)) %>%
#       select(obs_date, precip_in),
#     by = "obs_date"
#   )
# 
# scale_factor <- max(plot_data$obs_flow, na.rm = TRUE) /
#   max(plot_data$precip_in, na.rm = TRUE)
# 
# ggplot(plot_data, aes(x = obs_date)) +
#   geom_line(aes(y = obs_flow),
#             color = "black",
#             linewidth = 0.8) +
#   geom_col(aes(y = precip_in * scale_factor),
#            fill = "blue",
#            alpha = 0.5,
#            width = 1) +
#   scale_y_continuous(
#     name = "Streamflow (cfs)",
#     sec.axis = sec_axis(
#       ~ . / scale_factor,
#       name = "Precipitation (in)"
#     )
#   ) +
#   labs(
#     title = "Daily flow (cfs) vs. Daily Precipitation (in) for 02039500",
#     x = "Date"
#   ) +
#   
#   theme_bw()
