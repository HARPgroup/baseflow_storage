suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(dataRetrieval)
})

#--------------------------------------------------
# 1) helper: get drainage area in square miles. mirrors the role of da_sqmi(gage_id()) used by the app
#--------------------------------------------------
get_drain_area_sqmi <- function(site_num) {
  if (is.null(site_num) || length(site_num) == 0 || is.na(site_num)) {
    return(NA_real_)
  }
  
  site <- tryCatch(
    dataRetrieval::readNWISsite(as.character(site_num)),
    error = function(e) NULL
  )
  
  if (is.null(site) || !("drain_area_va" %in% names(site))) {
    return(NA_real_)
  }
  
  da <- suppressWarnings(as.numeric(site$drain_area_va[1]))
  if (is.na(da) || da <= 0) return(NA_real_)
  
  da
}

#--------------------------------------------------
# 2) main function: standalone version of forecast_results()
#
# required inputs:
#   raw_daily        : data frame with Date and Flow
#   forecast_start   : Date or date-like value
#   agwrc_single     : scalar AGWRC value
#
# optional inputs:
#   forecast_metric  : "flow" or "storage"
#   storage_points   : data frame with Date and Storage_in
#                      (and ideally Flow_in if you want parity with the app)
#   gage_id          : used only for drainage-area-based fallback conversion
#   forecast_horizons: defaults to c(15, 30, 45, 90)
#
# returns:
#   tibble with horizon_days, forecast_date, AGWRC,
#   proj_flow_cfs, proj_flow_in_day, proj_storage_in
#--------------------------------------------------
forecast_results_standalone <- function(raw_daily,
                                        forecast_start,
                                        agwrc_single,
                                        forecast_metric = c("flow", "storage"),
                                        storage_points = NULL,
                                        gage_id = NULL,
                                        forecast_horizons = c(15, 30, 45, 90)) {
  
  forecast_metric <- match.arg(forecast_metric)
  
  #-----------------------------
  # input checks
  #-----------------------------
  if (missing(raw_daily) || is.null(raw_daily) || nrow(raw_daily) == 0) {
    stop("raw_daily must be a non-empty data frame.")
  }
  if (!all(c("Date", "Flow") %in% names(raw_daily))) {
    stop("raw_daily must contain columns 'Date' and 'Flow'.")
  }
  
  forecast_start <- as.Date(forecast_start)
  if (is.na(forecast_start)) {
    stop("forecast_start could not be parsed as a Date.")
  }
  
  agwrc <- suppressWarnings(as.numeric(agwrc_single))
  if (is.na(agwrc)) {
    stop("agwrc_single must be numeric.")
  }
  
  raw_daily <- raw_daily %>%
    mutate(
      Date = as.Date(Date),
      Flow = as.numeric(Flow)
    ) %>%
    arrange(Date)
  
  #-----------------------------
  # pull starting flow (Q0)
  #-----------------------------
  Q0 <- raw_daily$Flow[raw_daily$Date == forecast_start]
  if (length(Q0) == 0) {
    stop("Selected forecast start date has no flow record in raw_daily.")
  }
  Q0 <- as.numeric(Q0[1])
  
  #-----------------------------
  # project flow at fixed horizons
  # same core logic as app:
  # proj_flow <- Q0 * (agwrc ^ forecast_horizons)
  #-----------------------------
  proj_flow <- Q0 * (agwrc ^ forecast_horizons)
  
  #convert projected flow to inches/day when possible
  da <- get_drain_area_sqmi(gage_id)
  sp_conv <- if (!is.na(da) && da > 0) {
    ((86400 * 12) / (5280 * 5280)) / da
  } else {
    NA_real_
  }
  
  proj_flow_in <- proj_flow * sp_conv
  
  #-----------------------------
  # storage projection logic
  #-----------------------------
  if (forecast_metric == "storage") {
    
    if (is.null(storage_points) || nrow(storage_points) == 0) {
      stop("storage_points must be supplied and non-empty when forecast_metric = 'storage'.")
    }
    if (!all(c("Date", "Storage_in") %in% names(storage_points))) {
      stop("storage_points must contain at least 'Date' and 'Storage_in'.")
    }
    
    storage_points <- storage_points %>%
      mutate(
        Date = as.Date(Date),
        Storage_in = as.numeric(Storage_in)
      ) %>%
      arrange(Date)
    
    if (!(forecast_start %in% storage_points$Date)) {
      stop("forecast_start must exist in storage_points$Date when forecast_metric = 'storage'.")
    }
    
    S0 <- storage_points$Storage_in[storage_points$Date == forecast_start][1]
    if (is.na(S0)) {
      stop("Storage_in at forecast_start is NA; cannot project storage.")
    }
    
    #same app logic: S_t+Δ = S0 * AGWRC^Δ
    proj_storage_in <- S0 * (agwrc ^ forecast_horizons)
    
  } else {
    
    #flow mode keeps a derived storage estimate for display
    proj_storage_in <- if (isTRUE(agwrc < 0.9999) && !is.na(sp_conv)) {
      proj_flow_in / (1 - agwrc)
    } else {
      rep(NA_real_, length(proj_flow))
    }
  }
  
  #-----------------------------
  # return in the same structure
  # as the app reactive
  #-----------------------------
  tibble(
    horizon_days      = forecast_horizons,
    forecast_date     = forecast_start + horizon_days,
    AGWRC             = agwrc,
    proj_flow_cfs     = proj_flow,
    proj_flow_in_day  = proj_flow_in,
    proj_storage_in   = proj_storage_in
  )
}

#--------------------------------------------------
# 3) optional example usage
#--------------------------------------------------
# raw_daily_example <- tibble(
#   Date = seq.Date(as.Date("2020-01-01"), as.Date("2020-06-30"), by = "day"),
#   Flow = seq(100, 40, length.out = 182)
# )
# 
# storage_points_example <- tibble(
#   Date = raw_daily_example$Date,
#   Storage_in = seq(5, 2, length.out = 182)
# )
# 
# forecast_results_standalone(
#   raw_daily = raw_daily_example,
#   forecast_start = as.Date("2020-06-01"),
#   agwrc_single = 0.97,
#   forecast_metric = "flow",
#   gage_id = "01634000"
# )
# 
# forecast_results_standalone(
#   raw_daily = raw_daily_example,
#   forecast_start = as.Date("2020-06-01"),
#   agwrc_single = 0.97,
#   forecast_metric = "storage",
#   storage_points = storage_points_example,
#   gage_id = "01634000"
# )