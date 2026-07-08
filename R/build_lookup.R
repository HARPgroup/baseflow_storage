#'@title site_factors
#'@name site_factors
#' @details A function...
#' @param da_sqmi numeric
#' @param flow_vec numeric
#' @param vec_for_reg numeric
#' @param m numeric
#' @param b numeric
#' @return A data frame...
#' @export
site_factors <- function(da_sqmi, flow_vec, vec_for_reg = NULL, m, b){
  #convert flow fron cfs to watershed inches per day
  Qin <- convert.flow(flow_vec, da_sqmi)

  if (is.null(vec_for_reg)) {
    vec_for_reg <- Qin
  }

  #calculate AGWRC with recession
  C <- b + (m * log(vec_for_reg))

  #bound C by 0 and 1
  C <- pmin(pmax(C, 0.001), 0.999)

  return(data.frame(flow_vec, Qin, C))
}

#'@title build_lookup
#'@name build_lookup
#' @details A function...
#' @param df_clean data frame
#' @param da_sqmi numeric
#' @param m numeric
#' @param b numeric
#' @param n_steps numeric
#' @param vec_for_reg numeric
#' @return A data frame...
#' @export
build_lookup <- function(obs_flow, da_sqmi, m, b, n_steps = 25, vec_for_reg = NULL) {
  #only baseflow periods
  min_flow <- min(obs_flow, na.rm = TRUE)
  max_flow <- max(obs_flow, na.rm = TRUE)

  Qcfs <- seq(min_flow, max_flow, length.out = n_steps)

  # Flow conversion and C calculation
  lookup <- site_factors(da_sqmi, flow_vec = Qcfs, vec_for_reg = vec_for_reg, m = m, b = b)

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
