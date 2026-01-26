#args <- commandArgs(trailingOnly = T)
#if (length(args) != 4){
  #message("Missing or extra arguments. Usage: flow_path flow_col 'gage_name' manual_opt end_path")
  #q()
#}

#flow_csv <- read.csv(paste0(args[1]))
#flow_csv$Date <- as.Date(flow_csv$Date)
#flow_col <- paste0(args[2])
#gage_name <- as.character(args[3])
#land_type_code <- as.character(args[4]) # for example, "forN51171"
#manual_opt <- as.logical(args[5])
#end_path <- paste0(args[6])

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/main/MainAnalysisFunctionsPt1.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/analyze_recession.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/attach_event_stats.R")
library(dataRetrieval)
library(dplyr)
library(tidyr)
# Load in stream data from USGS
flow_csv <- readNWISdv("01634000", parameterCd = "00060") %>% renameNWISColumns()

#Or load in model data
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/make_model_daily.R")
CS_model_data <- read.csv(paste0("https://deq1.bse.vt.edu:443/p6/out/land/subsheds2/pwater/forN51165_pwater.csv"))
MJ_model_data <- read.csv(paste0("https://deq1.bse.vt.edu:443/p6/out/land/subsheds2/pwater/forN51171_pwater.csv"))
S_model_data <- read.csv(paste0("https://deq1.bse.vt.edu:443/p6/out/land/subsheds2/pwater/forN51187_pwater.csv"))

#CS forN51165
#MJ forN51171
#S forN51187

CS_model_data <- make_model_daily(CS_model_data, "index")
MJ_model_data <- make_model_daily(MJ_model_data, "index")
S_model_data <- make_model_daily(S_model_data, "index")

names(S_model_data)[names(S_model_data) == "AGWO"] <- "Flow_in"
#MJ_model_data <- MJ_model_data %>%
  #rename(Flow = AGWO)
#S_model_data <- S_model_data %>%
  #rename(Flow = AGWO)

flow_csv <- S_model_data
flow_col <- "Flow_in"
manual_opt <- FALSE
gage_name <- "Strasburg"

suppressPackageStartupMessages(library(purrr))

#calculate AGWR and delta_AGWR
flow_csv$AGWR <- calc_AGWR(flow_csv[[flow_col]])
flow_csv$delta_AGWR <- calc_delta_AGWR(flow_csv$AGWR)

flow_csv$AGWR[!is.finite(flow_csv$AGWR)] <- NA
flow_csv$delta_AGWR[!is.finite(flow_csv$delta_AGWR)] <- NA

flow_csv <- add_month_season(flow_csv)

extra <- flow_csv

if(manual_opt == TRUE){
  flow_csv$GroupID <- 1
  flow_csv$RecessionDay <- TRUE
}else{
  flow_csv <- flag_stable_baseflow(flow_csv, flow_col)
}


#remove NAs
flow_csv <- flow_csv[!is.na(flow_csv$RecessionDay), ]
flow_csv$Year <- year(flow_csv$Date)
flow_csv$Day <- day(flow_csv$Date)

flow_csv$site_no <- "01634000"

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
    select(site_no, Date, Flow_in, AGWR, delta_AGWR, Year, Month, Day, Season, GroupID)
  
  list(df = df, summary = summary_df, analysis = analysis_df, name = site$name)
})


#extract
analysis_df <- results$gage$analysis

analysis_df <- attach_event_stats(analysis_df, r_lim = 0)

# Write final csvs out
write.csv(analysis_df, "S_model_original_analysis.csv", row.names = FALSE)
