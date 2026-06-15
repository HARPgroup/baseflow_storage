source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ben_bf_workflow/site_factors.R")


build_lookup <- function(df_clean, da_sqmi, m, b, by = 0.1, vec_for_reg = NULL) {
  
  Qcfs <- seq(
    min(df_clean$Flow, na.rm = TRUE),
    max(df_clean$Flow, na.rm = TRUE),
    by = by
  )
  
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