# ============================================================
# Standalone AGWRC regression workflow
# Uses flow in inches/day
# Compatible with updated convert.flow(flow_col, area_sqmi)
# ============================================================

# -----------------------------
# 0) Install/load packages
# -----------------------------
needed_pkgs <- c("dplyr", "readr", "httr", "dataRetrieval")

to_install <- needed_pkgs[!vapply(needed_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) {
  install.packages(to_install)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(httr)
  library(dataRetrieval)
})

# -----------------------------
# 1) Updated convert.flow
# -----------------------------
convert.flow <- function(flow_col, area_sqmi) {
  cfs <- flow_col
  
  conversion <- (86400 * 12) / (5280 * 5280)
  sp_conv <- conversion / area_sqmi
  
  flow_in <- cfs * sp_conv
  return(flow_in)
}

# -----------------------------
# 2) Helper to get drainage area
# -----------------------------
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

# -----------------------------
# 3) GitHub loading helpers
# -----------------------------
bf_github_raw_url <- function(
    gage_id,
    kind = c("model", "gage"),
    branch = "ben_bf_csvs",
    owner = "HARPgroup",
    repo = "baseflow_storage"
) {
  kind <- match.arg(kind)
  
  templates <- if (kind == "model") {
    c("bf_model_events_{gage_id}.csv", "bf_events_{gage_id}.csv")
  } else {
    c("bf_gage_events_{gage_id}.csv", "bf_events_{gage_id}.csv")
  }
  
  for (tpl in templates) {
    path <- gsub("\\{gage_id\\}", as.character(gage_id), tpl)
    url <- sprintf(
      "https://raw.githubusercontent.com/%s/%s/%s/%s",
      owner, repo, branch, path
    )
    
    ok <- tryCatch({
      resp <- httr::HEAD(url, httr::timeout(10))
      httr::status_code(resp) == 200
    }, error = function(e) FALSE)
    
    if (ok) return(url)
  }
  
  stop(
    "Could not find analyzed CSV for gage_id = ", gage_id,
    " and kind = ", kind, "."
  )
}

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
  
  if (!("R_squared" %in% names(df)) && "trimmed_R2" %in% names(df)) {
    df <- dplyr::rename(df, R_squared = trimmed_R2)
  }
  
  if ("R_squared" %in% names(df)) {
    df <- dplyr::rename(df, R2 = R_squared)
  } else if (!("R2" %in% names(df))) {
    df$R2 <- NA_real_
  }
  
  df %>%
    mutate(
      Date = as.Date(Date),
      GroupID = as.integer(GroupID),
      Flow = as.numeric(Flow),
      AGWR = as.numeric(AGWR),
      AGWRC = as.numeric(AGWRC),
      kept = as.logical(kept),
      met_alpha = as.logical(met_alpha)
    )
}

load_analysis_points <- function(gage_id, kind = c("model", "gage")) {
  kind <- match.arg(kind)
  url <- bf_github_raw_url(gage_id = gage_id, kind = kind)
  message("Reading analyzed CSV from: ", url)
  
  df <- readr::read_csv(url, show_col_types = FALSE)
  bf_standardize_analysis_df(df, gage_id = gage_id)
}

# -----------------------------
# 4) Build event-level regression table
# -----------------------------
make_event_regression_df <- function(points_df, area_sqmi, flow_col = "Flow") {
  required <- c("GroupID", "Date", flow_col, "AGWRC", "kept", "met_alpha")
  missing <- setdiff(required, names(points_df))
  
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  
  if (is.na(area_sqmi) || !is.numeric(area_sqmi) || area_sqmi <= 0) {
    stop("area_sqmi must be a single positive numeric value.")
  }
  
  df <- points_df %>%
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
    )
  
  event_df <- df %>%
    group_by(GroupID) %>%
    summarise(
      start_date         = min(Date, na.rm = TRUE),
      end_date           = max(Date, na.rm = TRUE),
      n_days             = dplyr::n(),
      median_flow_cfs    = median(.data[[flow_col]], na.rm = TRUE),
      median_flow_in_day = median(flow_in_day, na.rm = TRUE),
      min_flow_in_day    = min(flow_in_day, na.rm = TRUE),
      max_flow_in_day    = max(flow_in_day, na.rm = TRUE),
      event_AGWRC        = dplyr::first(AGWRC),
      .groups = "drop"
    ) %>%
    arrange(start_date)
  
  event_df
}

# -----------------------------
# 5) Fit regression and store outputs
# -----------------------------
fit_agwrc_regression_in_day <- function(event_df, n_grid = 100) {
  required <- c("GroupID", "median_flow_in_day", "event_AGWRC")
  missing <- setdiff(required, names(event_df))
  
  if (length(missing) > 0) {
    stop("event_df is missing required columns: ", paste(missing, collapse = ", "))
  }
  
  reg_df <- event_df %>%
    filter(
      !is.na(median_flow_in_day),
      !is.na(event_AGWRC),
      median_flow_in_day > 0
    )
  
  if (nrow(reg_df) < 2) {
    stop("Need at least 2 valid events to fit regression.")
  }
  
  model <- lm(event_AGWRC ~ log(median_flow_in_day), data = reg_df)
  
  reg_df <- reg_df %>%
    mutate(
      fitted_AGWRC = predict(model, newdata = reg_df),
      residual = event_AGWRC - fitted_AGWRC
    )
  
  flow_grid <- seq(
    min(reg_df$median_flow_in_day, na.rm = TRUE),
    max(reg_df$median_flow_in_day, na.rm = TRUE),
    length.out = n_grid
  )
  
  curve_df <- data.frame(
    median_flow_in_day = flow_grid,
    predicted_AGWRC = predict(
      model,
      newdata = data.frame(median_flow_in_day = flow_grid)
    )
  )
  
  list(
    model = model,
    coefficients = coef(model),
    model_summary = summary(model),
    event_values = reg_df,
    regression_curve = curve_df
  )
}

# -----------------------------
# 6) One-call wrapper
# -----------------------------
run_agwrc_regression_workflow <- function(gage_id, kind = c("model", "gage")) {
  kind <- match.arg(kind)
  
  points_df <- load_analysis_points(gage_id = gage_id, kind = kind)
  area_sqmi <- get_drainage_area_sqmi(gage_id)
  
  event_df <- make_event_regression_df(
    points_df = points_df,
    area_sqmi = area_sqmi,
    flow_col = "Flow"
  )
  
  reg_out <- fit_agwrc_regression_in_day(event_df)
  
  list(
    gage_id = gage_id,
    kind = kind,
    area_sqmi = area_sqmi,
    points_df = points_df,
    event_df = event_df,
    regression = reg_out
  )
}

# -----------------------------
# 7) Example run
# -----------------------------
out <- run_agwrc_regression_workflow(
  gage_id = "01633000",
  kind = "gage"
)

print(out$area_sqmi)
print(out$regression$coefficients)
print(head(out$regression$event_values))
print(head(out$regression$regression_curve))
print(out$regression$model_summary)
