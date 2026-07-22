#'@title site_factors
#'@name site_factors
#' @details A function that prepares data to be turned into a lookup table via
#'   \code{build_lookup()}
#' @param da_sqmi numeric value of the drainage area in square miles for a gage
#' @param flow_vec numeric flow vector for a gage in cfs
#' @param m Numeric. slope value for the gage in the relationship of AGWRC ~
#'   log(Q)
#' @param b Numeric. intercept value for the gage in the relationship of AGWRC ~
#'   log(Q)
#' @return A data frame of data that has a C, m, b, drainage area, flow vector,
#'   and a vector for regression that is ready a lookup table can be built from
#' @export
site_factors <- function(da_sqmi, flow_vec, m, b){
  #convert flow fron cfs to watershed inches per day
  Qin <- convert.flow(flow_vec, da_sqmi)

  #calculate AGWRC with recession
  C <- b + (m * log(Qin))

  #bound C by 0 and 1
  C <- pmin(pmax(C, 0.001), 0.999)

  return(data.frame(flow_vec, Qin, C))
}

#'@title build_lookup
#'@name build_lookup
#' @details A function that takes a vector of flows and create a lookup table to
#'   estimate AGWRC from flows using user provided log-linear regression
#'   coeffiicents (often provided by \code{fit_agwrc_regression()})
#' @param obs_flow Numeric. Vector of flow values to use as lookup column for
#'   AGWRC
#' @param da_sqmi Numeric. Value of the drainage area for the gage in square miles
#' @param m Numeric. slope value for the gage in the relationship of AGWRC ~
#'   log(Q)
#' @param b Numeric. intercept value for the gage in the relationship of AGWRC ~
#'   log(Q)
#' @param n_steps numeric. number of rows/steps required in the lookup table,
#'   ex. 25 is 25 steps, a larger value is more precise but has a longer compute
#'   time
#' @return A data frame containing a lookup table based on AGWRC, m, b, and flow
#'   values that can be used to predict variables when missing some but having
#'   others
#' @export
build_lookup <- function(obs_flow, da_sqmi, m, b, n_steps = 25) {
  #only baseflow periods
  min_flow <- min(obs_flow, na.rm = TRUE)
  max_flow <- max(obs_flow, na.rm = TRUE)

  Qcfs <- seq(min_flow, max_flow, length.out = n_steps)

  # Flow conversion and C calculation
  lookup <- site_factors(da_sqmi, flow_vec = Qcfs, m = m, b = b)

  # Derive storage and storage ratio
  lookup$S  <- lookup$Qin / (1 - lookup$C)
  lookup$dS <- c(lookup$S[-1] / lookup$S[-nrow(lookup)], NA)

  Svar <- lookup[lookup$dS > 1 & lookup$S > 0, ]

  list(lookup = lookup, Svar = Svar)
}

# Usage
# result    <- build_lookup(df_clean, da_sqmi, m = m, b = b)
# lookupdata <- result$lookup
# Svar       <- result$Svar
