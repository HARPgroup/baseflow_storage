#A script  to summarize the recession coefficients from a flow gage data frame
#with identified recession coefficients
#For local testing:
# commandArgs <- function(...){
#   c("strasTrimStats.csv", "01634000", "strasSummaryStats.csv")
# }
args <- commandArgs(trailingOnly = T)
if (length(args) < 2){
  message("Use Rscript baseflow_regression_df.R input_file output_file gage_id [regression_flow_col='Flow'] [add_inches_day=FALSE]")
  q()
}

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(hydrotools))

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/main/FinalRegression.R")

# get arguments
input_file <- paste0(args[1])
input_file <- str_replace_all(input_file, '\"', '') # quotes coming in give troubles
gage_id <- args[2]
end_path <- paste0(args[3])
#Check if regression_flow_col was input or if default should be used
if (length(args) > 3) {
  regression_flow_col <- paste0(args[4])
} else {
  regression_flow_col <- "Flow"
}
#Check if add_inches_day was input or if default should be used
if (length(args) > 4) {
  add_inches_day <- as.logical(paste0(args[5]))
} else {
  add_inches_day <- FALSE
}

#Read in baseflow recession event stats (either pre- or post-event trimming)
points_df <- read.csv(input_file)
#Ensure standard column data types
points_df <- bf_standardize_analysis_df(points_df, gage_id = gage_id)

#Convert flow to inches per day on user request
if (add_inches_day || regression_flow_col == "flow_in_day") {
  #Get drainage area from hydrotools::WaterGageBase()
  area_sqmi <- get_drainage_area_sqmi(gage_id)
  
  points_df <- add_flow_in_day(
    points_df = points_df,
    area_sqmi = area_sqmi,
    source_flow_col = "Flow",
    new_col = "flow_in_day"
  )
}
#Summarize regression events
event_df <- make_event_regression_df(
  points_df = points_df,
  regression_flow_col = regression_flow_col
)

# Write final csvs out
write.csv(event_df, end_path,row.names = FALSE)
