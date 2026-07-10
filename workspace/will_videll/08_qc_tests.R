# A script that will take in the gage object and gage
# summary data and run QC checks at various points.
#For local testing:
# commandArgs <- function(...){
#   c(gage_obj, "lynnSummaryStats.csv", "lynnGage.csv", "lynnQC.csv", gageID)
# }

library(lmtest)
suppressPackageStartupMessages(library(tidyverse))
library(nhdplusTools)

args <- commandArgs(trailingOnly = T)
if (length(args) < 5){
  message("Use Rscript 08_qc.R gage_obj input_06_file input_01_file output_file gageID")
  q()
}

# get arguments
gage_obj <- paste0(args[1])
input_06_file <- paste0(args[2])
input_06_file <- str_replace_all(input_06_file, '\"', '')
input_01_file <- paste0(args[3])
input_01_file <- str_replace_all(input_01_file, '\"', '')
output_file <- paste0(args[4])
gageID <- paste0(args[5])

message(paste0("DEBUG with: args <- c('",paste(args,collapse="', '")),"')")

message(paste("Reading", input_file))


### This script contains a list of QC functions to be added to ###
### the baseflow workflow ###

# read in the data and use the same name as bf workflow output
event_df <- read_csv(input_06_file, col_types = cols())
daily_df <- read_csv(input_01_file, col_types = cols())

# get event counts (monthly and gage total)
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/willv_bf_workflow/workspace/will_videll/event_counts.R")
monthly_events <- bf_monthly_events_n(event_df, gageID)

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/willv_bf_workflow/workspace/will_videll/gage_length.R")
gage_length <- gage_length(daily_df)

# get basin slope
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/willv_bf_workflow/workspace/will_videll/basin_slope.R")
slope <- suppressWarnings(get_basin_slope(gage_obj))

info <- data.frame(
  gage_length = gage_length,
  slope = as.numeric(slope)
)

### This function quantifies heteroscedasticity ###

# create a new column for the log median flow
event_df <- event_df |>
  mutate(logQ = log(median_flow))

# create a linear model
model <- lm(event_AGWRC ~ logQ, data = event_df)

# run function
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/willv_bf_workflow/workspace/will_videll/heteroscedasticity.R")
hetero_df <- heteroscedasticity(model)

### These two functions flags outliers ###
# IQR uses boundaries to detect unusually high or low values in the data
# Cook's distance detects influential observations that affect the model fit

# create a new column for the log median flow
event_df <- event_df |>
  mutate(logQ = log(median_flow))

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/willv_bf_workflow/workspace/will_videll/flag_outliers_IQR.R")
event_df <- flag_outliers_IQR(event_df)

# create a function that determines influential observations with Cook's
# distance

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/willv_bf_workflow/workspace/will_videll/flag_cooks.R")
event_df <- flag_cooks(model, event_df)

### Write csvs ###

#Monthly event total
write.csv(monthly_events,
          paste0("step08_", gageID, "_eventCount.csv"),
          row.names = FALSE)
#Heteroscedsticity
write.csv(hetero_df,
          paste0("step08_", gageID, "_hetero.csv"),
          row.names = FALSE)
#Gage length and slope
write.csv(info,
          paste0("step08_", gageID, "_info.csv"),
          row.names = FALSE)
#Flags
write.csv(event_df,
          paste0("step08_", gageID, "_flagged.csv"),
          row.names = FALSE)


