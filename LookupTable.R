source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/main/add_model_data.R")

analysis_df <- read.csv(
  "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/bf_events_01632000.csv"
)

get_event_storage_agwrc_debug <- function(event_data,
                                          selected_group_id,
                                          land_type_code = "forN51171",
                                          scenario = "subsheds2",
                                          site = "https://deq1.bse.vt.edu:443") {
  
  required_cols <- c("Date", "GroupID", "AGWRC")
  missing_cols <- setdiff(required_cols, names(event_data))
  
  if (length(missing_cols) > 0) {
    stop("event_data is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  event_data <- event_data %>%
    mutate(Date = as.Date(Date))
  
  event_subset <- event_data %>%
    filter(GroupID == selected_group_id) %>%
    arrange(Date)
  
  if (nrow(event_subset) == 0) {
    stop("No rows found for selected_group_id = ", selected_group_id)
  }
  
  cat("\n--- EVENT SUBSET SUMMARY ---\n")
  cat("Rows in event_subset:", nrow(event_subset), "\n")
  cat("Date range:", as.character(min(event_subset$Date, na.rm = TRUE)), "to",
      as.character(max(event_subset$Date, na.rm = TRUE)), "\n")
  cat("Non-NA AGWRC rows:", sum(!is.na(event_subset$AGWRC)), "\n")
  
  event_with_agws <- add_model_data(
    event_subset,
    land_type_code = land_type_code,
    "AGWS",
    scenario = scenario,
    site = site
  )
  
  cat("\n--- OUTPUT FROM add_model_data() ---\n")
  cat("Rows returned:", nrow(event_with_agws), "\n")
  cat("Columns returned:\n")
  print(names(event_with_agws))
  
  if (!("AGWS" %in% names(event_with_agws))) {
    stop("AGWS column was not returned by add_model_data().")
  }
  
  cat("Non-NA AGWS rows:", sum(!is.na(event_with_agws$AGWS)), "\n")
  
  if ("Date" %in% names(event_with_agws)) {
    event_with_agws$Date <- as.Date(event_with_agws$Date)
    cat("Returned date range:",
        as.character(min(event_with_agws$Date, na.rm = TRUE)), "to",
        as.character(max(event_with_agws$Date, na.rm = TRUE)), "\n")
  }
  
  cat("\n--- FIRST FEW ROWS BEFORE FINAL FILTER ---\n")
  print(
    event_with_agws %>%
      select(any_of(c("Date", "GroupID", "AGWRC", "AGWS"))) %>%
      head(15)
  )
  
  paired_df <- event_with_agws %>%
    mutate(Date = as.Date(Date)) %>%
    select(Date, GroupID, AGWRC, AGWS, everything()) %>%
    filter(!is.na(Date), !is.na(AGWRC), !is.na(AGWS)) %>%
    arrange(Date)
  
  cat("\n--- FINAL PAIRED OUTPUT ---\n")
  cat("Rows with both AGWRC and AGWS:", nrow(paired_df), "\n")
  
  return(paired_df)
}

event_storage_lookup_df <- get_event_storage_agwrc_debug(
  event_data = analysis_df,
  selected_group_id = 136,
  land_type_code = "forN51171"
)
