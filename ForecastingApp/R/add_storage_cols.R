# R/add_storage_cols.R
# Helpers to add Flow_in (watershed inches/day) and Storage_in (inches) to any
# timeseries table with Date, Flow (cfs), and AGWRC (unitless).

get_drain_area_sqmi <- function(site_num) {
  site <- tryCatch(dataRetrieval::readNWISsite(site_num), error = function(e) NULL)

  if (!is.null(site) && nrow(site) > 0) {
    # Preferred NWIS field used in colleague script
    if ("drain_area_va" %in% names(site) && !is.na(site$drain_area_va[1])) return(as.numeric(site$drain_area_va[1]))

    # Common alternates (field names can vary)
    candidates <- c("drain_area_va", "drain_area", "drain_area_sqmi", "drain_area_sq_mi")
    for (nm in candidates) {
      if (nm %in% names(site) && !is.na(site[[nm]][1])) return(as.numeric(site[[nm]][1]))
    }
  }

  stop("Could not determine drainage area (sq mi) from NWIS site metadata for site ", site_num, ".")
}

add_storage_cols <- function(df,
                             site_num,
                             flow_col = "Flow",
                             agwrc_col = "AGWRC") {
  stopifnot(!is.null(df), nrow(df) > 0)

  if (!("Date" %in% names(df))) stop("add_storage_cols: df must contain a 'Date' column.")
  if (!(flow_col %in% names(df))) stop("add_storage_cols: df missing flow column '", flow_col, "'.")
  if (!(agwrc_col %in% names(df))) stop("add_storage_cols: df missing AGWRC column '", agwrc_col, "'.")

  df$Date <- as.Date(df$Date)

  da_sqmi <- get_drain_area_sqmi(site_num)

  # Convert cfs -> inches/day over watershed area (sq mi)
  conversion <- (86400 * 12) / (5280 * 5280)
  sp_conv <- conversion / da_sqmi

  df$Flow_in <- as.numeric(df[[flow_col]]) * sp_conv

  # Storage_in via calc_storage()
  df <- calc_storage(df, "Flow_in", agwrc_col)

  df
}
