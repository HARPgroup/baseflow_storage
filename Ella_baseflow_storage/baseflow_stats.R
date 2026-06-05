#A script that will take in a flow file that has baseflow periods identified and
#then run linear regression to identify potential recession coefficients from
#each event
#For local testing:
# commandArgs <- function(...){
#   c("strasBF.csv", "for", "strasstats.csv")
# }

args <- commandArgs(trailingOnly = T)
if (length(args) < 3){
  message("Use Rscript baseflow_stats.R input_file luname_seg output_file [SITE_URL=FALSE]")
  q()
}
suppressPackageStartupMessages(library(stringr))
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/attach_event_stats.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/add_model_data.R")

# get arguments
input_file <- paste0(args[1])
input_file <- str_replace_all(input_file, '\"', '') # quotes coming in give troubles
land_type_code <- as.character(args[2]) # for example, "forN51171"
end_path <- paste0(args[3])
if (length(args) > 3) {
  SITE_URL <- paste0(args[4])
} else {
  SITE_URL=FALSE
}
message(paste0("DEBUG with: args <- c('",paste(args,collapse="', '")),"')")

message(paste("Reading", input_file))

analysis_df <- read.csv(input_file)
#Coerce character back to date
analysis_df$Date <- as.Date(analysis_df$Date)

analysis_df <- attach_event_stats(analysis_df, r_lim = 0)

# Add AGW model data

if (!is.logical(SITE_URL)) {
  analysis_df <- add_model_data(analysis_df, land_type_code, "AGWI", site=SITE_URL)
  analysis_df <- add_model_data(analysis_df, land_type_code, "AGWET", site=SITE_URL)
  analysis_df <- add_model_data(analysis_df, land_type_code, "AGWO", site=SITE_URL)
}
# Write final csvs out
write.csv(analysis_df, end_path, row.names = FALSE)
