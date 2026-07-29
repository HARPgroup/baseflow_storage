basepath <- "/var/www/R"
source("/var/www/R/config.R")
library(hydrotools)
library(agws)
library(tidyverse)
gage_obj <- WaterGageDaily$new(gage_id = "03524000", ds_in = ds)
var <- gage_obj$baseflow_workflow_data(omsite)
event_df <- var$trimmed_events_df
#' @title min_flow_accuracy
#' @name min_flow_accuracy
#' @description
#' uses agws::forwardForecast() to create a 90-day forecast for identified
#' events at a given USGS gage location. Absolute error and absolute percent error
#' are calculated at the minimum observed flow value from the 90-day forecast.
#'
#' @param gage_obj an R6 gage object from VDEQ baseflow workflow
#' @param event_df df of identified baseflow events from step 02 of VDEQ baseflow workflow.
#' Requires `GroupID` and `Date` columns.
#'
#' @returns df with GroupID, start_date, obs_flow, proj_flow, abs_err, abs_pcnt_err.
#' @export min_flow_accuracy
AGWRC <- "lm_variable"
i <- 1
min_flow_accuracy <- function(gage_obj, event_df, AGWRC = c("lm_constant", "lm_variable")){

  #group event data by GroupID, 1 row per event.Create empty columns for data.
  event_df1 <- event_df |>
    dplyr::group_by(GroupID) |>
    dplyr::summarize(start_date = min(Date),
                     local_min1_date = NA,
                     days_overestimated = NA,
                     trough_cnt = NA,
                     min_obs_flow = NA,
                     min_for_flow = NA,
                     min_flow_date = NA)

  #loop through each event
  for(i in 1:nrow(event_df1)){

    #run agws::forwardForecast() at event start date and slice the day with the lowest observed flow.
    forecast <- gage_obj$baseflow_forecast(start_date = event_df1$start_date[i], AGWRC = AGWRC,
                                           use_limits = TRUE)

    obs_min_data <- forecast |>
      dplyr::slice_min(obs_flow)

    forecast$overestimate[forecast$Forecast > forecast$obs_flow] <- TRUE
    forecast$trough <- zoo::rollapply(forecast$obs_flow, width = 7, function(x) x[4] <= min(x[-4]), fill = FALSE)

    troughs <- forecast |>
      dplyr::filter(trough) |>
      group_by(run_id = consecutive_id(obs_flow)) |>
      slice_tail(n = 1) |>
      ungroup() |>
      select(Date, trough)

    forecast$trough <- FALSE # reset trough column
    forecast <- forecast |>
      rows_update(troughs, by = "Date") # update trough column with filtered troughs

    forecast$upper_bound <- forecast$obs_flow * 1.10
    forecast$lower_bound <- forecast$obs_flow * .90
    forecast <- forecast |>
      mutate(is_accurate = trough * (Forecast > lower_bound) & (Forecast < upper_bound))

    troughs <- forecast |>
      filter(trough)
    longest_streak <- with(rle(troughs$is_accurate), if(any(values)) max(lengths[values]) else 0)

    #fill empty data columns with values from sliced forecast
    event_df1$local_min1_date[i] <- troughs$Date[1]
    event_df1$local_min1_date <- as.Date(event_df1$local_min1_date)
    event_df1$days_overestimated[i] <- sum(forecast$overestimate, na.rm = TRUE)
    event_df1$trough_cnt[i] <- sum(forecast$trough)
    event_df1$accurate_troughs[i] <- sum(forecast$is_accurate)
    event_df1$longest_acc_streak[i] <- longest_streak
    event_df1$min_obs_flow[i] <- obs_min_data$obs_flow[1]
    event_df1$min_for_flow[i] <- obs_min_data$Forecast[1]
    event_df1$min_flow_date[i] <- obs_min_data$Date[1]
  }

  return(event_df1)
}

test2 <- min_flow_accuracy(gage_obj, event_df, AGWRC = "lm_variable")


## Make this a function

plot_event_minima <- function(gage_obj, start_date){
  forecast1 <- gage_obj$baseflow_forecast(start_date = start_date, AGWRC = "lm_variable",
                                       use_limits = TRUE)
  min_flow <- forecast1 |> dplyr::slice_min(obs_flow)
  forecast1$troughs <- zoo::rollapply(forecast1$obs_flow, width = 7, function(x) x[4] <= min(x[-4]), fill = FALSE)

  forecast_trimmed <- forecast1 |>
    dplyr::filter(troughs) |>
    group_by(run_id = consecutive_id(obs_flow)) |>
    slice_tail(n = 1) |>
    ungroup() |>
    filter(obs_flow == cummin(obs_flow))

  plot <- gage_obj$plot_baseflow_forecast(start_date = start_date, include_days_before = 60)+
   geom_point(aes(x = forecast_trimmed$Date, y = forecast_trimmed$obs_flow))+
    geom_point(aes(x = min_flow$Date, y = min_flow$obs_flow), color = "red")

  return(plot)
}

start_date <- "1922-05-23"
plot_event_minima(gage_obj, start_date)


## Temp

test |>
  ggplot(aes(min_obs_flow, min_for_flow))+
  geom_point()+
  geom_abline()+
  geom_smooth(method = "lm")+
  labs(x = "Minimum Observed Flow", y = "Forecasted Flow", title = "Default Start Dates")+
  theme_minimal()

model <- lm(min_for_flow ~ min_obs_flow, data = test)

minus30 = which(gage_obj$gage_data$Date == (as.Date(start_date))) - 30
if (length(minus30) == 0) {
  # we are outside the date range of the gage, skip
  next
}
last30 = gage_obj$gage_data[minus30:(minus30 + 30),]
Q0 = min(last30$Flow)
start_date = max(last30[last30$Flow == Q0,]$Date)

model2 <- lm(min_for_flow ~ min_obs_flow, data = test2)

test2 |>
  ggplot(aes(min_obs_flow, min_for_flow))+
  geom_point()+
  geom_abline()+
  geom_smooth(method = "lm")+
  labs(x = "Minimum Observed Flow", y = "Forecasted Flow", title = "Changed Start Dates")+
  theme_minimal()
