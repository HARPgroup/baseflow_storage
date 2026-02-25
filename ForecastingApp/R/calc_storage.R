# R/calc_storage.R
# Groundwater storage equivalent in inches.
# Vendored from HARPgroup/baseflow_storage (ih_model_calcs/calc_storage.R) on 2026-02-23.
# NOTE: This is a simple algebraic conversion: Storage_in = Flow_in / (1 - AGWRC)

calc_storage <- function(data, flow_col, AGWRC_col) {
  df <- data

  if (!(flow_col %in% names(df))) stop("calc_storage: missing flow_col '", flow_col, "'.")
  if (!(AGWRC_col %in% names(df))) stop("calc_storage: missing AGWRC_col '", AGWRC_col, "'.")

  denom <- 1 - as.numeric(df[[AGWRC_col]])
  denom[is.na(denom) | denom == 0] <- NA_real_

  df$Storage_in <- as.numeric(df[[flow_col]]) / denom
  df
}
