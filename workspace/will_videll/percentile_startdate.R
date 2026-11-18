library(hydrotools)
library(agws)
library(tidyverse)
basepath <- "/var/www/R"
source("/var/www/R/config.R")
gage_obj <- WaterGageDaily$new(gage_id = "02055000", ds_in = ds)

get_percentile_startdate <- function(gage_obj, year){
  gage_data <- gage_obj$gage_data |>

    # filter flows to the given year in months April to July
    filter(year(time) == year & month(time) %in% c(4,5,6,7))

  gage_data$percentile <- gage_obj$calculate_value_percentile(gage_data$value, gage_data$time, percentile_agg = "month")

  sd <- gage_data$time[which.min(gage_data$percentile)]
  return(sd)
}

## Change this variable for testing
year <- 2002

#Start date chosen by min flow
gage_obj$plot_baseflow_forecast(as.Date(paste0(year,"-11-04")), adjust_start_date = c(paste0(year, "-04-01"), paste0(year, "-07-31")), forecast_days = 0:120, AGWRC = )

#Start date chosen by percentile
percentile_sd <- get_percentile_startdate(gage_obj, year)
gage_obj$plot_baseflow_forecast(percentile_sd, forecast_days = 0:120)





































gage_data <- gage_obj$gage_data

gage_data$percentiles <- gage_obj$calculate_value_percentile(gage_data$value, gage_data$time)

# Find new start date, minimum percentile flow from previous 30 days
minus30 = which(gage_data$time == (last(gage_obj$gage_data$time))) - 30
#if (length(minus30) == 0) {
#  # we are outside the date range of the gage, skip
#  next
#}
last30 = gage_data[minus30:(minus30 + 30),]
Q0 = min(last30$value[last30$percentiles == min(last30$percentiles)])
min30start_date = as.Date(max(last30[last30$value == Q0,]$time))

gage_obj$plot_baseflow_forecast("2026-03-01")
