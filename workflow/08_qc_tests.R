# A script that will take in the gage object and gage
# summary data and run QC checks at various points.
#For local testing:
# commandArgs <- function(...){
#   c("01634000", paste0("step1_","01634000", ".csv"), paste0("step6_","01634000", ".csv"),
#      "obs_date","obs_flow","QC.csv")
# }

suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(agws))

args <- commandArgs(trailingOnly = T)
if (length(args) < 6){
  stop("Use Rscript 08_qc.R gageID input_06_file input_01_file date_col flow_col output_file")
}

# get arguments
gageID <- args[1]
input_01_file <- args[2]
input_06_file <- args[3]
date_col <- args[4]
flow_col <- args[5]
output_file <- args[6]

message(paste0("DEBUG with: args <- c('",paste(args,collapse="', '")),"')")

message(paste("Reading", input_01_file))


### This script contains a list of QC functions to be added to ###
### the baseflow workflow ###

# read in the data and use the same name as bf workflow output
reg_df <- read.csv(input_06_file)
daily_df <- read.csv(input_01_file)

# get event counts (monthly and gage total)
monthly_events <- agws::bf_monthly_events_n(reg_df, gageID,
                                      date_col = "start_date", group_col = "GroupID")


gage_length <- sum(!is.na(daily_df[,flow_col]))

# get basin slope
slope <- agws::get_basin_slope(gageID)

info <- data.frame(
  gage_length = gage_length,
  slope = as.numeric(slope)
)


# Write csvs
#Monthly event total
write.csv(monthly_events,
          paste0("step08_", gageID, "_eventCount.csv"),
          row.names = FALSE)
#Gage length and slope
write.csv(info,
          paste0("step08_", gageID, "_info.csv"),
          row.names = FALSE)


