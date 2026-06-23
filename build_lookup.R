source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ben_bf_workflow/site_factors.R")


build_lookup <- function(df_clean, da_sqmi, m, b, n_steps = 25, vec_for_reg = NULL) {
  
  #only baseflow periods
  obs_flow <- df_clean$Flow[!is.na(df_clean$AGWRC)]
  
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