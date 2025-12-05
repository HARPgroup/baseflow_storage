## global.R

library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(DT)
library(lubridate)
library(purrr)

# ---- source modules ----
source("modules/droughtModuleUI.R")
source("modules/droughtModuleServer.R")

# ---- helper to read and clean a site pair ----
read_site_data <- function(original_path, trimmed_path, site_label) {
  original <- readr::read_csv(original_path, show_col_types = FALSE) %>%
    mutate(
      Date = as.Date(Date),
      site_name = site_label
    )
  
  trimmed  <- readr::read_csv(trimmed_path, show_col_types = FALSE) %>%
    mutate(
      Date = as.Date(Date)
    )
  
  list(original = original, trimmed = trimmed)
}

# ---- read all three sites ----
cs_data <- read_site_data(
  "data/CS_original_analysis_df.csv",
  "data/CS_trimmed_event_results.csv",
  "Cootes Store"
)

mj_data <- read_site_data(
  "data/MJ_original_analysis_df.csv",
  "data/MJ_trimmed_event_results.csv",
  "Mount Jackson"
)

s_data <- read_site_data(
  "data/S_original_analysis_df.csv",
  "data/S_trimmed_event_results.csv",
  "Strasburg"
)

# ---- master list for easy switching ----
site_data_list <- list(
  "Cootes Store"  = cs_data,
  "Mount Jackson" = mj_data,
  "Strasburg"     = s_data
)
