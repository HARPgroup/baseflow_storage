#'@title flag_stable_baseflow
#'@name
#'flag_stable_baseflow
#'@description
#'Evaluates recession days based on flow parameters
#'@details
#'Asesses if each day in a flow time series is considered "in recession" by
#'evaluating if the ratio of today's flow and yesterday's (AGWR) is less than 1
#'and if the change in AGWR is within 0.97 - 1.03. Recession days are excluded
#'if flow is greater than 1.15 times the mean flow. \code{gap_fill(max_gap)} is
#'used to allow small excursions beyond the AGWR and dAGWR thresholds
#'@param df data.frame with flow_col, AGWR, and delta_AGWR column present, often
#'  calculated by \code{analyze_recession()}
#'@param flow_col character, flow_col with Flow values from df
#'@param AGWR_col character, AGWR_col is AGWR column from df
#'@param delta_col character, delta_col is delta_AGWR column from df
#'@param delta_thresh Numeric threshold value for day-to-day change in
#'  delta_AGWR. Defaults to 0.03, meaning the change in AGWR may be outside
#'  exceed 0.97 - 1.03 range except during \code{gap_fill()} periods
#'@param max_gap Numeric. The time allowed between recession events to pass to
#'  \code{gap_fill()}. value for acceptable gap of false values, default is 3
#'@return data.frame with all original fields and now a RecessionDay field that
#'  contains logical for if a day is considered in recession
#'@examples
#'#Sample data for flow_col, AGWR, and delta_AGWR
#'obs_flow <- c(580, 544, 508, 544, 508, 473, 900, 403)
#'AGWR <- c(0.8430233, 0.9379310, 0.9338235, 1.0708661, 0.9338235,
#' 0.9311024, 0.9260042, 0.9200913)
#'delta_AGWR <- c(0.8871349, 1.1125803, 0.9956207, 1.1467543,
#' 0.8720264, 0.9970860, 0.9945246, 0.9936146)
#'#Assigning data frame of flow_col, AGWR, and delta_AGWR to df
#'df <- data.frame(obs_flow, AGWR, delta_AGWR)
#'flow_col <- "obs_flow"
#'#Running flag_stable_baseflow with df and flow_col
#'df <- flag_stable_baseflow(df, df[[flow_col]])
#'@export
flag_stable_baseflow <- function(
  df,
  flow_col, AGWR_col = "AGWR",
  delta_col = "delta_AGWR", delta_thresh = 0.03,
  max_gap = 3) {

  #Assigns AGWR_col and delta_col to new obj
  AGWR <- df[[AGWR_col]]
  delta <- df[[delta_col]]

  #Creates logical vector of recession events
  is_stable <- abs(delta - 1.0) < delta_thresh & AGWR < 1.0
  #Runs gap_fill function to find missed recession days
  df$RecessionDay <- gap_fill( is_stable, max_gap)

  #Runs for loop for all indices of length of df$RecessionDay
  for(i in 1:length(df$RecessionDay)) {
  #Compare flow at index position to overall mean flow
    if (flow_col[i] >= 1.15 * mean(flow_col, na.rm = TRUE)) {
      #Set RecessionDay at index position to false
      df$RecessionDay[i] <- FALSE

    }

    #Returns updated df
    return(df)
  }
}

