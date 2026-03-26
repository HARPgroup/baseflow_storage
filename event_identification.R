args <- commandArgs(trailingOnly = T)
if (length(args) < 7){
  message("Use Rscript event_identification.R input_file date_column flow_column site_name luname_seg manual(TRUE/FALSE) output_file [SITE_URL=FALSE]")
  q()
}

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/main/MainAnalysisFunctionsPt1.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/analyze_recession.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/attach_event_stats.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/add_model_data.R")
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(stringr))

# get arguments
input_file <- paste0(args[1])
input_file <- str_replace_all(input_file, '\"', '') # quotes coming in give troubles
date_col <- paste0(args[2])
flow_col <- paste0(args[3])
gage_name <- as.character(args[4])
land_type_code <- as.character(args[5]) # for example, "forN51171"
manual_opt <- as.logical(args[6])
end_path <- paste0(args[7])
if (length(args) > 7) {
  SITE_URL <- paste0(args[8])
} else {
  SITE_URL=FALSE
}
message(paste0("DEBUG with: args <- c('",paste(args,collapse="', '")),"')")

message(paste("Reading", input_file))

flow_csv <- read.csv(input_file)
flow_csv$Date <- as.Date(flow_csv[[date_col]])
flow_csv$Flow <- flow_csv[[flow_col]]

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

#extract
analysis_df <- results$gage$analysis

analysis_df <- attach_event_stats(analysis_df, r_lim = 0)

# Add AGW model data

if (!is.logical(SITE_URL)) {
  analysis_df <- add_model_data(analysis_df, land_type_code, "AGWI", site=SITE_URL)
  analysis_df <- add_model_data(analysis_df, land_type_code, "AGWET", site=SITE_URL)
  analysis_df <- add_model_data(analysis_df, land_type_code, "AGWO", site=SITE_URL)
}
# Write final csvs out
write.csv(analysis_df, end_path)
