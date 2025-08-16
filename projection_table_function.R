library(dplyr)

project_baseflow <- function(url, value_col = "calc_AGWR", 
                             proj_days = c(30, 60, 120, 240, 360, 540)) {
  
  df <- read.csv(url, stringsAsFactors = FALSE)
  
  if (!(value_col %in% colnames(df))) {
    stop(paste("Column", value_col, "not found in file"))
  }
  
  require(dplyr)
  
  df$Date <- as.Date(df$Date)
  
  last_days <- df %>%
    group_by(GroupID) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  # Add projected columns
  for (d in proj_days) {
    colname <- paste0("proj_", d)
    last_days[[colname]] <- last_days$Flow * (last_days[[value_col]] ^ d)
  }
  
  return(last_days)
}

# List of URLs
urls <- c(
  "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/cootes_store_event_dataset.csv",
  "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/mount_jackson_event_dataset.csv",
  "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/strasburg_event_dataset.csv"
)

# Run for all URLs and combine
all_results <- lapply(urls, project_baseflow) %>%
  bind_rows()

# Save combined CSV
write.csv(all_results, "all_gages_proj_summary.csv", row.names = FALSE)

# Optional: preview
#head(all_results)
