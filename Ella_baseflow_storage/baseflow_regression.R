#A script  to develop a regression of log(Q) vs AGWRC from a series of
#summarized baseflow events (with estimated recession coefficients already
#incorporated)
#For local testing:
# commandArgs <- function(...){
#   c("strasSummaryStats.csv", "01634000", "strasAGWRCRegression.csv")
# }
args <- commandArgs(trailingOnly = T)
if (length(args) < 2){
  message("Use Rscript baseflow_regression_df.R input_file output_file gage_id [regression_flow_col='Flow']")
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

#Read in summarized baseflow recession stats 
event_df <- read.csv(input_file)
#Create regression from all events
model <- fit_agwrc_regression(event_df)
model_summary <- summary(model)

#Output data frame
out <- data.frame(
  site_no = as.character(gage_id),
  flow_metric = regression_flow_col,
  m = coef(model)[2],
  b = coef(model)[1],
  m_pvalue = model_summary$coefficients[2,4],
  b_pvalue = model_summary$coefficients[1,4],
  Rsq = model_summary$r.squared
)
message(paste0("Found coefficients m = ",out$m," and b = ",out$b," with R Squared of ",out$Rsq))
# Write final csvs out
write.csv(out, end_path,row.names = FALSE)
