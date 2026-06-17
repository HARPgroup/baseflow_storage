### This script performs iterative linear regression one data point ###
### at a time ###

#For local testing:
# commandArgs <- function(...){
#   c("LuraySummaryStats.csv", "LurayIterativeLm.csv", "LurayHighestRSquared")
# }

args <- commandArgs(trailingOnly = T)
if (length(args) < 3){
  message("Use Rscript iterative_lm.R input_file output_file_1 output_file_2")
  q()
}

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(hydrotools))

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/Ella_bf_workflow/Ella_baseflow_storage/iterative_lm_function.R")

# get arguments
input_file <- paste0(args[1])
end_path_1 <- paste0(args[2])
end_path_2 <- paste0(args[3])

# read in CSV from Step_06
event_df <- read.csv(input_file)

# run iterative lm function
iterative_lm_df <- iterative_lm(event_df = event_df)

# write csv for final dataframe
write.csv(iterative_lm_df, end_path_1, row.names = FALSE)

# write csv for top 5 R-squared values and for which iterations those values
# occurred 
write.csv(top5, end_path_2, row.names = FALSE)

