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
#' 
min_flow_accuracy <- function(gage_obj, event_df, AGWRC = c("lm_constant", "lm_variable")){
  
  #group event data by GroupID, 1 row per event.Create empty columns for data.
  event_df1 <- event_df |> 
    dplyr::group_by(GroupID) |> 
    dplyr::summarize(start_date = min(Date),
                     local_min1_date = NA,
                     days_overestimated = NA,
                     trough_cnt = NA)
  
  #loop through each event
  for(i in 1:nrow(event_df1)){
    
    #run agws::forwardForecast() at event start date and slice the day with the lowest observed flow.
    forecast <- gage_obj$baseflow_forecast(start_date = event_df1$start_date[i], AGWRC = AGWRC,
                                           use_limits = TRUE) 
    
    forecast$overestimate[forecast$Forecast > forecast$obs_flow] <- TRUE
    forecast$trough <- rollapply(forecast$obs_flow, width = 7, function(x) x[4] <= min(x[-4]), fill = FALSE)
    
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
  }
  
  return(event_df1)
}

test <- min_flow_accuracy(gage_obj, event_df, AGWRC = "lm_variable")


## Make this a function

plot_event_minima <- function(gage_obj, start_date){
  forecast1 <- gage_obj$baseflow_forecast(start_date = start_date, AGWRC = "lm_variable",
                                       use_limits = TRUE)
  forecast1$troughs <- rollapply(forecast1$obs_flow, width = 7, function(x) x[4] <= min(x[-4]), fill = FALSE)  
  
  forecast_trimmed <- forecast1 |> 
    dplyr::filter(troughs) |> 
    group_by(run_id = consecutive_id(obs_flow)) |> 
    slice_tail(n = 1) |> 
    ungroup() 
  
  plot <- gage_obj$plot_baseflow_forecast(start_date = start_date, include_days_before = 60)+
   geom_point(aes(x = forecast_trimmed$Date, y = forecast_trimmed$obs_flow)) 
 
  return(plot)
}

start_date <- "1936-05-03"
plot_event_minima(gage_obj, start_date)

