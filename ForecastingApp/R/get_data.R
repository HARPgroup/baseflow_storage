## Raw Gage Data ####
gage_raw_daily <- function(gage_id, analysis_points, ds){
  req(gage_id())
  
  pts <- analysis_points()
  start_date <- min(pts$Date, na.rm = TRUE)
  gage_obj <- tryCatch({
    test <- hydrotools::WaterGageBase$new(
      ds_in = ds,
      gage_id = gage_id(),
      start_date = as.character(start_date)
    )},
    error = function(e) {
      showNotification(paste("USGS daily flow download failed:", e$message), type = "error", duration = NULL)
      return(NULL)
    }
  )
  
  dv <- gage_obj$gage_data
  
  req(!is.null(dv), nrow(dv) > 0)
  
  dv <- dv %>%
    dplyr::transmute(
      Date = as.Date(.data[[gage_obj$date_col]]),
      Flow = as.numeric(.data[[gage_obj$flow_col]])
    ) %>%
    dplyr::arrange(Date)
  
  return(dv)
}


## Raw model Data ####
#Needs update: maybe a river seg selector that pulls model data via
#watershednode obj?
model_raw_daily <- function(site_choice){
  req(site_choice())
  
  url_map <- c(
    "Cootes Store"  = "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/data/PS2_5550_5560_flows_11.csv",
    "Mount Jackson" = "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/data/PS2_5560_5100_flows_11.csv",
    "Strasburg"     = "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/data/PS3_5100_5080_flows_11.csv"
  )
  
  url <- unname(url_map[site_choice()])
  req(!is.na(url), nzchar(url))
  
  raw <- tryCatch(
    readr::read_csv(url, show_col_types = FALSE),
    error = function(e) {
      showNotification(paste("Model raw CSV load failed:", e$message), type = "error", duration = NULL)
      return(NULL)
    }
  )
  req(!is.null(raw), nrow(raw) > 0)
  
  flow_col <- if ("Qout" %in% names(raw)) "Qout" else if ("Flow" %in% names(raw)) "Flow" else NA_character_
  req(!is.na(flow_col))
  
  date_vec <- NULL
  if ("thisdate" %in% names(raw)) date_vec <- raw$thisdate
  if (is.null(date_vec) && "Date" %in% names(raw)) date_vec <- raw$Date
  
  need_build <- is.null(date_vec) || all(is.na(date_vec)) || all(trimws(as.character(date_vec)) == "")
  
  if (need_build) {
    req(all(c("year", "month", "day") %in% names(raw)))
    date_vec <- sprintf("%04d-%02d-%02d", raw$year, raw$month, raw$day)
  }
  
  out <- tibble::tibble(
    Date = as.Date(date_vec),
    Flow = as.numeric(raw[[flow_col]])
  ) %>%
    dplyr::filter(!is.na(Date)) %>%
    dplyr::arrange(Date)
  
  if (nrow(out) == 0) {
    showNotification(
      "Model raw data loaded, but no valid dates were parsed (Date ended up empty). Check date fields in the model CSV.",
      type = "error", duration = NULL
    )
  } else {
    message(
      "Model raw_daily: rows=", nrow(out),
      " min=", min(out$Date, na.rm = TRUE),
      " max=", max(out$Date, na.rm = TRUE)
    )
  }
  
  out
}
