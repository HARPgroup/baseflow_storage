args <- commandArgs(trailingOnly = T)
if (length(args) != 4){
  message("Missing or extra arguments. Usage: flow_path flow_col 'gage_name' manual_opt end_path")
  q()
}

flow_csv <- read.csv(paste0(args[1]))
flow_csv$Date <- as.Date(flow_csv$Date)
flow_col <- "Flow"
gage_name <- "Strasburg"
manual_opt <- F

library(dplyr)
library(lubridate)
library(dataRetrieval)

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/main/MainAnalysisFunctionsPt1.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/analyze_recession.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/attach_event_stats.R")
# Load in stream data from USGS
flow_csv <- readNWISdv("01634000", parameterCd = "00060") %>% renameNWISColumns()

suppressPackageStartupMessages(library(purrr))

#calculate AGWR and delta_AGWR
flow_csv$AGWR <- calc_AGWR(flow_csv[[flow_col]])
flow_csv$delta_AGWR <- calc_delta_AGWR(flow_csv$AGWR)

flow_csv <- add_month_season(flow_csv)

if(manual_opt == TRUE){
  flow_csv$GroupID <- 1
  flow_csv$RecessionDay <- TRUE
}else{
  flow_csv <- flag_stable_baseflow(flow_csv, flow_csv[[flow_col]])
}

#remove NAs
flow_csv <- flow_csv[!is.na(flow_csv$RecessionDay), ]
flow_csv$Year <- year(flow_csv$Date)
flow_csv$Day <- day(flow_csv$Date)

#apply to gage of interest
sites <- list(
  gage = list(data = flow_csv, name = paste0(gage_name))
)

results <- imap(sites, function(site, abbrev) {
  result <- analyze_recession(site$data, site$name, min_len = 14)
  df <- result$df
  summary_df <- result$summary
  
  analysis_df <- df %>%
    filter(!is.na(GroupID)) %>%
    select(site_no, Date, Flow, AGWR, delta_AGWR, Year, Month, Day, Season, GroupID)
  
  list(df = df, summary = summary_df, analysis = analysis_df, name = site$name)
})

# extract the analysis dataframe from results
S_original_analysis_df <- results$gage$analysis

# Trim each event
S_trimmed_analysis_df <- S_original_analysis_df %>%
  group_by(GroupID) %>%
  group_modify(~ trim_event_mk(.x)) %>%
  ungroup()

# Keep only the rows marked as kept = TRUE
S_trimmed_kept <- S_trimmed_analysis_df %>%
  filter(kept == TRUE)

# Recalculate AGWR & delta_AGWR after trimming
S_trimmed_kept <- S_trimmed_kept %>%
  mutate(
    AGWR = calc_AGWR(Flow),
    delta_AGWR = calc_delta_AGWR(AGWR)
  )

# Attach event statistics
attach_event_stats <- function(analysis_data, r_lim = 0) {
  require(sqldf)
  source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/summarize_event.R")
  
  event_stats <- summarize_event(analysis_data)
  
  combined_data <- sqldf(sprintf(
    "SELECT a.*, 
            b.AGWR AS calc_AGWR, 
            b.R_squared AS event_R_squared
     FROM analysis_data AS a
     LEFT OUTER JOIN event_stats AS b
     ON a.GroupID = b.event_num
     WHERE b.R_squared > %f",
    r_lim
  ))
  
  return(combined_data)
}


S_original_analysis_df <- attach_event_stats(S_original_analysis_df, r_lim = 0)

S_trimmed_analysis_df <- attach_event_stats(S_trimmed_kept, r_lim = 0) %>%
  rename(
    trimmed_calc_AGWR = calc_AGWR,
    trimmed_event_R_squared = event_R_squared
  )

# Add AGW model data
#
# analysis_df <- add_model_data(analysis_df, land_type_code, "AGWI")
# 
# analysis_df <- add_model_data(analysis_df, land_type_code, "AGWET")
# 
# analysis_df <- add_model_data(analysis_df, land_type_code, "AGWO")
# 
# # Write final csvs out
# write.csv(analysis_df, end_path)