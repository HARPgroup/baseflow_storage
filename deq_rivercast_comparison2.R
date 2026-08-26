library(dplyr)
library(lubridate)
library(purrr)
library(tidyverse)

basepath <- '/var/www/R'
source('/var/www/R/config.R')

#Read in the data
gage_obj <- hydrotools::WaterGageDaily$new(gage_id = "02059500", ds = ds)
deq_forecast <- gage_obj$baseflow_forecast("2026-07-07",AGWRC = "lm_variable")

GageID = "02059500" # Goose Creek 
flow_csv <- read_csv(paste0("https://deq1.bse.vt.edu/usgs/agws/", GageID, "-flow.csv"))

date <- "2026-07-07"
rivercast <- read_csv(paste0("https://deq1.bse.vt.edu/usgs/rivercast/USGS_rc_", date, ".csv"))
rivercast <- rivercast %>% 
  filter(StaID == "02059500") %>% 
  distinct(issue_date, forecast_week, .keep_all = TRUE) %>% 
  select(issue_date, StaID, forecast_date, forecast_week, median_pct, 
         pred_interv_05_pct, pred_interv_95_pct)

# Create pct based on historic flow data
flow_csv_pct  <- flow_csv %>%
  mutate(j_day = format(obs_date, "%j")) %>%
  group_by(j_day) %>%
  reframe(
    prob = seq(0.01, 1, by = 0.01),
    quant = quantile(
      obs_flow,
      probs = seq(0.01, 1, by = 0.01),
      na.rm = TRUE
    )
  )
    
# Convert predicted flow to percentile
flow_to_percentile <- function(flow, day, flow_csv_pct) {
  
  ref <- flow_csv_pct %>%
    filter(j_day == day) %>%
    group_by(quant) %>%
    summarise(
      prob = mean(prob),
      .groups = "drop"
    ) %>%
    arrange(quant)
  
  approx(
    x = ref$quant,
    y = ref$prob * 100,
    xout = flow,
    rule = 2
  )$y
}

deq_forecast_pct <- deq_forecast %>%
  filter(Day %in% seq(7, 91, by = 7)) %>%
  mutate(
    forecast_week = Day / 7,
    forecast_date = Date,
    j_day = format(forecast_date, "%j"),
    
# forcasted flow to percentile
deq_median_pct = map2_dbl(
  Forecast,
  j_day,
  flow_to_percentile,
  flow_csv_pct = flow_csv_pct
  ),

# obs_flow to percentile 
obs_pct = map2_dbl(
  obs_flow,
  j_day,
  flow_to_percentile,
  flow_csv_pct = flow_csv_pct
  )
)

comparison_df <- rivercast %>%
  inner_join(
  deq_forecast_pct,
  by = c("forecast_date", "forecast_week")
      )
    
comparison_df <- comparison_df %>% 
  mutate(
  rivercast_error = median_pct - obs_pct, 
  deq_error = deq_median_pct - obs_pct
      )
    
accuracy_df <- comparison_df %>%
  group_by(forecast_week) %>% 
  summarise(
    Rivercast_RMSE = sqrt(mean(rivercast_error^2, na.rm = TRUE)),
    DEQ_RMSE = sqrt(mean(deq_error^2, na.rm = TRUE)),
    .groups = "drop"
  )
