#A script that will take in a flow file containing AGWRCs, delta AGWRCs, and a
#flag for recession days and identify potential baseflow periods and assign
#group ids
#For local testing:
# commandArgs <- function(...){
#   c("strasEvent.csv", "Date", "Flow", "Strasburg", "strasBF.csv")
# }

args <- commandArgs(trailingOnly = T)
if (length(args) < 5){
  message("Use Rscript baseflow_events.R input_file date_column flow_column site_name output_file")
  q()
}

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/analyze_recession.R")
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(dplyr))
# get arguments
input_file <- paste0(args[1])
input_file <- str_replace_all(input_file, '\"', '') # quotes coming in give troubles
date_col <- paste0(args[2])
flow_col <- paste0(args[3])
gage_name <- as.character(args[4])
end_path <- paste0(args[5])
site_no_col <- paste0(args[6])

message(paste0("DEBUG with: args <- c('",paste(args,collapse="', '")),"')")

message(paste("Reading", input_file))

flow_csv <- read.csv(input_file)
flow_csv$Date <- as.Date(flow_csv[[date_col]])
flow_csv$Flow <- flow_csv[[flow_col]]
flow_csv$site_no <- flow_csv[[site_no_col]]

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

# Write final csvs out
write.csv(analysis_df, end_path, row.names = FALSE)

