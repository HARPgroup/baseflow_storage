# R/add_storage_cols.R
# Helpers to add Flow_in (watershed inches/day) and Storage_in (inches) to any
# timeseries table with Date, Flow (cfs), and AGWRC (unitless).
add_storage_cols <- function(df,
                             data_obj,
                             site_num,
                             flow_col = "Flow",
                             agwrc_col = "AGWRC") {
  stopifnot(!is.null(df), nrow(df) > 0)

  if (!("Date" %in% names(df))) stop("add_storage_cols: df must contain a 'Date' column.")
  if (!(flow_col %in% names(df))) stop("add_storage_cols: df missing flow column '", flow_col, "'.")
  if (!(agwrc_col %in% names(df))) stop("add_storage_cols: df missing AGWRC column '", agwrc_col, "'.")

  df$Date <- as.Date(df$Date)

  da_sqmi <- data_obj()$drainage_area

  # Convert cfs -> inches/day over watershed area (sq mi)
  conversion <- (86400 * 12) / (5280 * 5280)
  sp_conv <- conversion / da_sqmi

  df$Flow_in <- as.numeric(df[[flow_col]]) * sp_conv

  # Storage_in via calc_storage()
  df <- calc_storage(df, "Flow_in", agwrc_col)

  df
}
