#A script to calculate AGWRC and delta AGWRC from gage data (using todays and
#yesterday's flow). From there, identifies days in flow recession that may later
#quality as baseflow events
#For local testing:
# commandArgs <- function(...){
#   c("strasGage.csv", "Date", "Flow", FALSE, "strasEvent.csv")
# }
args <- commandArgs(trailingOnly = T)
if (length(args) < 5){
  message("Use Rscript event_identification.R input_file date_column flow_column manual(TRUE/FALSE) output_file")
  q()
}

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/main/MainAnalysisFunctionsPt1.R")
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(stringr))
#Used in add_month_season()
suppressPackageStartupMessages(library(dplyr))

# get arguments
input_file <- paste0(args[1])
input_file <- str_replace_all(input_file, '\"', '') # quotes coming in give troubles
date_col <- paste0(args[2])
flow_col <- paste0(args[3])
manual_opt <- as.logical(args[4])
end_path <- paste0(args[5])

message(paste0("DEBUG with: args <- c('",paste(args,collapse = "', '")),"')")

message(paste("Reading", input_file))

#Read in flow csv and ensure standard names
flow_csv <- read.csv(input_file)
flow_csv$Date <- as.Date(flow_csv[[date_col]])
flow_csv$Flow <- flow_csv[[flow_col]]

#calculate AGWR and delta_AGWR
flow_csv$AGWR <- calc_AGWR(flow_csv[[flow_col]])
flow_csv$delta_AGWR <- calc_delta_AGWR(flow_csv$AGWR)
#Add the season in
flow_csv <- add_month_season(flow_csv)


if(manual_opt == TRUE){
  flow_csv$GroupID <- 1
  flow_csv$RecessionDay <- TRUE
}else{
  #Identify when flow is in recession
  flow_csv <- flag_stable_baseflow(flow_csv, flow_csv[[flow_col]])
}

#remove NAs
flow_csv <- flow_csv[!is.na(flow_csv$RecessionDay), ]
flow_csv$Year <- year(flow_csv$Date)
flow_csv$Day <- day(flow_csv$Date)

# Write final csvs out
write.csv(flow_csv, end_path,row.names = FALSE)


