#basepath <- "/var/www/R"
#source("/var/www/R/config.R")
#library(hydrotools)
#library(agws)
#library(tidyverse)
#gage_obj <- WaterGageDaily$new(gage_id = "03524000", ds_in = ds)
#var <- gage_obj$baseflow_workflow_data(omsite)
#event_df <- var$trimmed_events_df

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

min_flow_accuracy <- function(gage_obj, event_df, AGWRC = c("lm_constant", "lm_variable")){
  
  # Group event data by GroupID, 1 row per event. Create empty columns for data.
  event_df1 <- event_df |> 
    dplyr::group_by(GroupID) |> 
    dplyr::summarize(start_date = min(Date),
                     new_start_date = NA,
                     #local_min1_date = NA,
                     days_overestimated = NA,
                     trough_cnt = NA,
                     min_obs_flow = NA,
                     min_for_flow = NA,
                     min_flow_date = NA,
                     mean_weighted_error = NA)
  
  # Loop through each event
  for(i in 1:nrow(event_df1)){
    
    # Find new start date, minimum flow from previous 30 days
    minus30 = which(gage_obj$gage_data$time == (as.Date(event_df1$start_date[i]))) - 30
    if (length(minus30) == 0) {
      # we are outside the date range of the gage, skip
      next
    }
    last30 = gage_obj$gage_data[minus30:(minus30 + 30),]
    Q0 = min(last30$value)
    min30start_date = as.Date(max(last30[last30$value == Q0,]$time))
    
    # Run agws::forwardForecast() at event start date and slice the day with the lowest observed flow.
    forecast <- gage_obj$baseflow_forecast(start_date = min30start_date, AGWRC = AGWRC,
                                           use_limits = TRUE) 
    
    # Pull row with lowest observed flow (90 day minimum)
    obs_min_data <- forecast |> 
      dplyr::slice_min(obs_flow)
    
    # Set overestimates to TRUE
    forecast$overestimate[forecast$Forecast > forecast$obs_flow] <- TRUE
    
    # Initial trough selection
    forecast$trough <- zoo::rollapply(forecast$obs_flow, width = 7, function(x) x[4] <= min(x[-4]), fill = FALSE)
    
    # Filter troughs with cumulative min
    troughs <- forecast |> 
      dplyr::filter(trough) |> 
      dplyr::group_by(run_id = consecutive_id(obs_flow)) |> 
      dplyr::slice_tail(n = 1) |> 
      dplyr::ungroup() |> 
      dplyr::filter(obs_flow == cummin(obs_flow)) |> 
      dplyr::select(Date, trough)
      
    forecast$trough <- FALSE # reset trough column
    forecast <- forecast |> 
      dplyr::rows_update(troughs, by = "Date") |> # update trough column with filtered troughs
      dplyr::mutate(abs_err = trough * abs(obs_flow - Forecast),
                    weight_factor = Day / 90,
                    weighted_error = abs_err * weight_factor)
    
    #fill empty data columns with values from sliced forecast
    #event_df1$local_min1_date[i] <- troughs$Date[1]
    #event_df1$local_min1_date <- as.Date(event_df1$local_min1_date)
    event_df1$days_overestimated[i] <- sum(forecast$overestimate, na.rm = TRUE)
    event_df1$trough_cnt[i] <- sum(forecast$trough)
    #event_df1$accurate_troughs[i] <- sum(forecast$is_accurate)
    #event_df1$longest_acc_streak[i] <- longest_streak
    event_df1$min_obs_flow[i] <- obs_min_data$obs_flow[1]
    event_df1$min_for_flow[i] <- obs_min_data$Forecast[1]
    event_df1$min_flow_date[i] <- obs_min_data$Date[1]
    event_df1$min_flow_date <- as.Date(event_df1$min_flow_date)
    event_df1$new_start_date[i] <- min30start_date
    event_df1$new_start_date <- as.Date(event_df1$new_start_date)
    event_df1$mean_weighted_error[i] <- sum(forecast$weighted_error) / sum(forecast$trough)
  }
  event_df1 <- event_df1 |> 
    dplyr::mutate(abs_error_90d = abs(min_obs_flow - min_for_flow),
           abs_pcnt_err90d = (abs_error_90d / min_obs_flow) * 100)
  
  return(event_df1)
}

test2 <- min_flow_accuracy(gage_obj, event_df, AGWRC = "lm_variable")

plot_event_minima <- function(gage_obj, start_date){
  forecast1 <- gage_obj$baseflow_forecast(start_date = start_date, AGWRC = "lm_variable",
                                          use_limits = TRUE)
  min_flow <- forecast1 |> dplyr::slice_min(obs_flow)
  forecast1$troughs <- zoo::rollapply(forecast1$obs_flow, width = 7, function(x) x[4] <= min(x[-4]), fill = FALSE)  
  
  forecast_trimmed <- forecast1 |> 
    dplyr::filter(troughs) |> 
    dplyr::group_by(run_id = consecutive_id(obs_flow)) |> 
    dplyr::slice_tail(n = 1) |> 
    dplyr::ungroup()|> 
    dplyr::filter(obs_flow == cummin(obs_flow)) 
  
  plot <- gage_obj$plot_baseflow_forecast(start_date = start_date, include_days_before = 60)+
    geom_point(aes(x = forecast_trimmed$Date, y = forecast_trimmed$obs_flow))+
    geom_point(aes(x = last(min_flow$Date), y = min_flow$obs_flow), color = "red")
  
  return(plot)
}

#start_date <- "1998-08-31"
#plot_event_minima(gage_obj, start_date)


