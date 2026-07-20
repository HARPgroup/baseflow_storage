#'@title attach_event_stats
#'@name
#'attach_event_stats
#'@description
#'Add baseflow event statistics, including recession coefficient, to a data
#'frame of baseflow events calculated from \code{agws::analyze_recession()}
#'@details
#'Calculates baseflow recession coefficients after filtering data for in range
#'data points via \code{summarize_event()} via a log-linear regression using
#'\code{summarize_event()} and adds these regression statistics to the input
#'data frame of recession events. Optionally filters for events with regressions
#'with coefficients of determination (R squared) greater than user input r_lim
#'@param analysis_data df with Date, Flow, AGWR, delta_AGWR as minimum present
#'  columns.
#'@param r_lim numeric. Minimum R.squared limit, default 0
#'@return df with site_no, Date, Flow, AGWR, delta_AGWR, Year, Month,
#'Day, season, GroupID, calc_AGWRC, R_squared
#'@export
attach_event_stats <- function(analysis_data, r_lim =0){
  event_stats <- summarize_event(analysis_data)

  combined_data <- sqldf::sqldf(sprintf(
    "select a.*, b.AGWR as calc_AGWR, b.R_squared
    from analysis_data as a
    left outer join event_stats as b
    on a.GroupID = b.GroupID
    AND a.Date = b.Date
    where b.R_squared > '%f'
    ", r_lim)
  )

  return(combined_data)
}
