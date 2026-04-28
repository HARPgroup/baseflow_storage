# FINAL REGRESSION SCRIPT
# Standalone AGWRC Regression Workflow
# Uses Q in inches/day
# Output: site, m, b, Landseg

#
#Install/load packages
#

needed_pkgs <- c("dplyr", "readr", "httr", "dataRetrieval")

to_install <- needed_pkgs[
  !vapply(needed_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(to_install) > 0) {
  install.packages(to_install)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(httr)
  library(dataRetrieval)
})

#
#Convert flow from cfs to inches/day
#

convert.flow <- function(flow_col, area_sqmi) {
  cfs <- flow_col
  
  conversion <- (86400 * 12) / (5280 * 5280)
  sp_conv <- conversion / area_sqmi
  
  flow_in <- cfs * sp_conv
  return(flow_in)
}

#
#Get drainage area from USGS
#

get_drainage_area_sqmi <- function(gage_id) {
  site <- dataRetrieval::readNWISsite(as.character(gage_id))
  
  if (!("drain_area_va" %in% names(site))) {
    stop("readNWISsite() did not return drain_area_va for gage_id = ", gage_id)
  }
  
  area_sqmi <- suppressWarnings(as.numeric(site$drain_area_va[1]))
  
  if (is.na(area_sqmi) || area_sqmi <= 0) {
    stop("Invalid drainage area returned for gage_id = ", gage_id)
  }
  
  area_sqmi
}

#
#GitHub loading helpers
#Uses main branch:
#https://raw.githubusercontent.com/HARPgroup/baseflow_storage/main/bf_events_01632000.csv
#

bf_github_raw_url <- function(
    gage_id,
    branch = "main",
    owner = "HARPgroup",
    repo = "baseflow_storage"
) {
  url <- sprintf(
    "https://raw.githubusercontent.com/%s/%s/%s/bf_events_%s.csv",
    owner, repo, branch, gage_id
  )
  
  ok <- tryCatch({
    resp <- httr::HEAD(url, httr::timeout(10))
    httr::status_code(resp) == 200
  }, error = function(e) FALSE)
  
  if (!ok) {
    stop("Could not find analyzed CSV at: ", url)
  }
  
  url
}

#
#Standardize analysis data
#

bf_standardize_analysis_df <- function(df, gage_id) {
  df$site_no <- as.character(gage_id)
  
  if (!("site_name" %in% names(df))) {
    df$site_name <- NA_character_
  }
  
  required <- c("GroupID", "Date", "Flow", "AGWR", "delta_AGWR", "kept", "met_alpha")
  missing <- setdiff(required, names(df))
  
  if (length(missing) > 0) {
    stop("Analysis CSV missing required columns: ", paste(missing, collapse = ", "))
  }
  
  if (!("AGWRC" %in% names(df)) && "trimmed_AGWRC" %in% names(df)) {
    df <- dplyr::rename(df, AGWRC = trimmed_AGWRC)
  }
  
  if (!("AGWRC" %in% names(df))) {
    stop("Analysis CSV does not contain AGWRC or trimmed_AGWRC.")
  }
  
  df %>%
    mutate(
      Date = as.Date(Date),
      GroupID = as.integer(GroupID),
      Flow = as.numeric(Flow),
      AGWR = as.numeric(AGWR),
      delta_AGWR = as.numeric(delta_AGWR),
      AGWRC = as.numeric(AGWRC),
      kept = as.logical(kept),
      met_alpha = as.logical(met_alpha)
    )
}

load_analysis_points <- function(gage_id) {
  url <- bf_github_raw_url(gage_id = gage_id)
  message("Reading analyzed CSV from: ", url)
  
  df <- readr::read_csv(url, show_col_types = FALSE)
  
  bf_standardize_analysis_df(df, gage_id = gage_id)
}

#
#Build event-level regression dataframe
#

make_event_regression_df <- function(points_df, area_sqmi, flow_col = "Flow") {
  required <- c("GroupID", "Date", flow_col, "AGWRC", "kept", "met_alpha")
  missing <- setdiff(required, names(points_df))
  
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  
  if (is.na(area_sqmi) || !is.numeric(area_sqmi) || area_sqmi <= 0) {
    stop("area_sqmi must be a single positive numeric value.")
  }
  
  points_df %>%
    mutate(
      Date = as.Date(Date),
      flow_in_day = convert.flow(.data[[flow_col]], area_sqmi)
    ) %>%
    filter(
      kept == TRUE,
      met_alpha == TRUE,
      !is.na(GroupID),
      !is.na(flow_in_day),
      !is.na(AGWRC),
      flow_in_day > 0
    ) %>%
    group_by(GroupID) %>%
    summarise(
      start_date = min(Date, na.rm = TRUE),
      end_date = max(Date, na.rm = TRUE),
      n_days = dplyr::n(),
      median_flow_cfs = median(.data[[flow_col]], na.rm = TRUE),
      median_flow_in_day = median(flow_in_day, na.rm = TRUE),
      event_AGWRC = dplyr::first(AGWRC),
      .groups = "drop"
    ) %>%
    filter(
      !is.na(median_flow_in_day),
      !is.na(event_AGWRC),
      median_flow_in_day > 0
    ) %>%
    arrange(start_date)
}

#
#Fit AGWRC ~ log(Q inches/day)
#

fit_agwrc_regression_in_day <- function(event_df) {
  required <- c("GroupID", "median_flow_in_day", "event_AGWRC")
  missing <- setdiff(required, names(event_df))
  
  if (length(missing) > 0) {
    stop("event_df is missing required columns: ", paste(missing, collapse = ", "))
  }
  
  if (nrow(event_df) < 2) {
    stop("Need at least 2 valid events to fit regression.")
  }
  
  reg_df <- event_df %>%
    mutate(logQ = log(median_flow_in_day))
  
  model <- lm(event_AGWRC ~ logQ, data = reg_df)
  
  data.frame(
    m = unname(coef(model)[["logQ"]]),
    b = unname(coef(model)[["(Intercept)"]])
  )
}

#
#One-site regression wrapper
#

run_one_site_regression <- function(gage_id) {
  points_df <- load_analysis_points(gage_id = gage_id)
  area_sqmi <- get_drainage_area_sqmi(gage_id)
  
  event_df <- make_event_regression_df(
    points_df = points_df,
    area_sqmi = area_sqmi,
    flow_col = "Flow"
  )
  
  coeffs <- fit_agwrc_regression_in_day(event_df)
  
  data.frame(
    site_no = as.character(gage_id),
    m = coeffs$m,
    b = coeffs$b
  )
}

#
#Site lookup table
#

site_lookup <- data.frame(
  site_no = c("01632000", "01633000", "01634000"),
  site = c("Cootes Store", "Mount Jackson", "Strasburg"),
  Landseg = c("N51165", "N51171", "N51187")
)

#
#Run all regressions
#

regression_results <- dplyr::bind_rows(
  run_one_site_regression("01632000"),
  run_one_site_regression("01633000"),
  run_one_site_regression("01634000")
)

#
#Final comparison table
#

final_regression_table <- regression_results %>%
  left_join(site_lookup, by = "site_no") %>%
  select(site, m, b, Landseg)

print(final_regression_table)

#Optional export
readr::write_csv(
  final_regression_table,
  "agwrc_regression_coefficients_inches.csv"
)