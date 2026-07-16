#basepath <- "/var/www/R"
#source("/var/www/R/config.R")
#library(hydrotools)
#library(agws)
#library(tidyverse)
#gage_obj <- WaterGageDaily$new(gage_id = "03524000", ds_in = ds)
#event_df <- read_csv("C:/HARP/baseflow_storage/step2_03524000.csv")
#var <- gage_obj$baseflow_workflow_data(omsite)
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
min_flow_accuracy <- function(gage_obj, event_df){
  
  #group event data by GroupID, 1 row per event.Create empty columns for data.
  event_df1 <- event_df |> 
    dplyr::group_by(GroupID) |> 
    dplyr::summarize(start_date = min(Date),
                     obs_min_flow = NA,
                     proj_flow = NA,
                     min_flow_date = NA)
  
  #loop through each event
  for(i in 1:nrow(event_df1)){
    
    #run agws::forwardForecast() at event start date and slice the day with the lowest observed flow.
    forecast <- gage_obj$baseflow_forecast(start_date = event_df1$start_date[i], AGWRC = "lm_variable",
                                           use_limits = TRUE) |> 
      dplyr::slice_min(obs_flow)
    
    #fill empty data columns with values from sliced forecast
    event_df1$obs_min_flow[i] <- forecast$obs_flow[1]
    event_df1$proj_flow[i] <- forecast$Forecast[1]
    event_df1$min_flow_date[i] <- forecast$Date[1]
    event_df1$min_flow_date <- as.Date(event_df1$min_flow_date)
  }
  
  #calculate error
  event_df1 <- event_df1 |> 
    dplyr::mutate(days_after_start = min_flow_date - start_date,
                  abs_err = abs(proj_flow - obs_min_flow),
                  abs_pcnt_err = abs(abs_err/obs_min_flow) * 100)
  return(event_df1)
}

#test <- min_flow_accuracy(gage_obj, event_df)

troughs <- rollapply(df$y, width = 7, function(x) x[4] < min(x[-4]), fill = FALSE)
