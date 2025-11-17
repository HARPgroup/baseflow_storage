library(purrr)
library(dplyr)

# Define your gages and MK alpha thresholds
gage_ids <- c("01634000", "01632000", "01634500")
alpha_values <- c(0.1, 0.2, 0.3)

# Create a data frame of all combinations to test
param_grid <- expand.grid(gage_id = gage_ids, alpha = alpha_values)

# A helper to safely run one analysis and record whether it breaks
test_mk_threshold <- function(gage_id, alpha) {
  message("Running gage ", gage_id, " with alpha = ", alpha)
  
  # Read flow data from USGS
  flow_csv <- dataRetrieval::readNWISdv(gage_id, parameterCd = "00060") %>%
    renameNWISColumns()
  
  flow_csv$AGWR <- calc_AGWR(flow_csv$Flow)
  flow_csv$delta_AGWR <- calc_delta_AGWR(flow_csv$AGWR)
  flow_csv <- add_month_season(flow_csv)
  flow_csv <- flag_stable_baseflow(flow_csv, flow_csv$Flow)
  
  flow_csv$Year <- lubridate::year(flow_csv$Date)
  flow_csv$Day <- lubridate::day(flow_csv$Date)
  
  # Run analyze_recession safely
  safe_result <- purrr::safely(analyze_recession)(flow_csv, gage_id, min_len = 14)
  
  # If it failed completely
  if (is.null(safe_result$result)) {
    return(tibble(
      gage_id = gage_id,
      alpha = alpha,
      total_groups = NA,
      broken_groups = NA,
      error_msg = safe_result$error$message
    ))
  }
  
  df <- safe_result$result$df
  
  # Count how many distinct groups exist and how many broke
  total_groups <- length(unique(df$GroupID))
  broken_groups <- sum(is.na(df$AGWR) | is.na(df$delta_AGWR) | is.na(df$Flow))
  
  tibble(
    gage_id = gage_id,
    alpha = alpha,
    total_groups = total_groups,
    broken_groups = broken_groups,
    percent_broken = round(100 * broken_groups / total_groups, 1)
  )
}

# Run the analysis for all combinations
mk_diagnostics <- purrr::pmap_dfr(param_grid, test_mk_threshold)

# Save or view results
print(mk_diagnostics)
write.csv(mk_diagnostics, "MK_threshold_diagnostics_raw.csv", row.names = FALSE)

find_broken_groups <- function(gage_id, alpha) {
  flow_csv <- readNWISdv(gage_id, parameterCd = "00060") %>%
    renameNWISColumns()
  
  flow_csv$AGWR <- calc_AGWR(flow_csv$Flow)
  flow_csv$delta_AGWR <- calc_delta_AGWR(flow_csv$AGWR)
  flow_csv <- add_month_season(flow_csv)
  flow_csv <- flag_stable_baseflow(flow_csv, flow_csv$Flow)
  
  flow_csv$Year <- lubridate::year(flow_csv$Date)
  flow_csv$Day <- lubridate::day(flow_csv$Date)
  
  result <- analyze_recession(flow_csv, gage_id, min_len = 14)
  df <- result$df
  
  df %>%
    group_by(GroupID) %>%
    summarize(has_NA = any(is.na(Flow) | is.na(AGWR) | is.na(delta_AGWR))) %>%
    filter(has_NA)
}

broken_groups_list <- purrr::map2(param_grid$gage_id, param_grid$alpha, find_broken_groups)
names(broken_groups_list) <- paste(param_grid$gage_id, param_grid$alpha, sep = "_")

broken_groups_list

