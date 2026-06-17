# A script that will take in the gage object and gage
# summary data and run QC checks at various points.
#For local testing:
# commandArgs <- function(...){
#   c(gage_obj, "lynnSummaryStats.csv", "lynnQC.csv")
# }

library(lmtest)
library(tidyverse)
library(nhdplusTools)

args <- commandArgs(trailingOnly = T)
if (length(args) < 4){
  message("Use Rscript 08_qc.R gage_obj input_file output_file")
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


### This script contains a list of QC functions to potentially be added to ###
### the baseflow workflow ###

# read in the data and use the same name as bf workflow output
event_df <- read_csv(input_06_file, col_types = cols())
daily_df <- read_csv(input_01_file, col_types = cols())

### This function determines the total number of events ###

bf_events_n <- function(event_df){
  result <- paste0("This gage has ", n_distinct(event_df$GroupID), " recorded events.")
  
  cat(result, "\n\n")
}
bf_events_n(event_df)

### This function determines the monthly total number of events ### 

bf_monthly_events_n <- function(event_df, gage_ID){
  monthly_event_count <- event_df |>
    mutate(month = month(as.Date(start_date, format = "%y-%m-%d"))) |>
    group_by(month) |>
    summarise(event_cnt = n_distinct(GroupID))
  
  cat(paste0("Monthly event totals saved to 'step08_", gageID, ".csv'")
  return(monthly_event_count)
}
bf_monthly_events_n(event_df)

### This function determines the gage length ###

gage_length <- function(daily_df){
  result <- paste0(
    "This gage has ", 
    length(daily_df$Flow),
    " observations."
  )
  
  cat(result, "\n\n")
}
gage_length(daily_df)

### This function quantifies heteroscedasticity ###

# create a new column for the log median flow 
event_df <- event_df |>
  mutate(logQ = log(median_flow))

# create a linear model
model <- lm(event_AGWRC ~ logQ, data = event_df)

# create a function that runs the Breusch-Pagan test and the White test
# the ouput is a message that gives p-value and interpretation
# flag values that have p-value < 0.1
heteroscedasticity <- function(model) {
  # run the Breusch-Pagan test 
  bp_test <- bptest(model)
  
  # write message with interpretation
  bp_msg <- paste0(
    "Breusch-Pagan test p-value = ",
    round(bp_test$p.value, 4),
    if(bp_test$p.value < 0.1) {
      ". Heteroscedasticity is likely causing some uncertainty in the model."
    }
    else{
      ". Heteroscedasticity is likely not a concern."
    }
  )
  
  # run the White test
  white_test <- bptest(model, ~fitted(model) + I(fitted(model)^2))
  
  # write message with interpretation
  white_msg <- paste0(
    "White test p-value = ",
    round(white_test$p.value, 4),
    if(white_test$p.value < 0.1){
      ". Heteroscedasticity is likely causing some uncertainty in the model."
    } else {
      ". Heteroscedasticity is likely not a concern."
    }
  )
  
  # output both messages in the console
  cat(bp_msg, "\n\n")
  cat(white_msg, "\n\n")
  
}
heteroscedasticity(model)

### This function calculates the slope ###

get_basin_slope <- function(gage_obj){
  
  site <- get_nldi_feature(list(featureSource = "nwissite",
                                featureID = paste0("USGS-", gageID)))
  gage_comid <- site$comid
  
  watershed_slope <- get_catchment_characteristics(
    varname = "TOT_BASIN_SLOPE", 
    ids = gage_comid
  )
  
  result <- (paste0("The slope is ", watershed_slope[,3], "%"))
  cat(result, "\n\n")
}
suppressWarnings(get_basin_slope(gage_obj))

### These two functions flags outliers ###

# IQR uses boundaries to detect unusually high or low values in the data
# Cook's distance detects influential observations that affect the model fit

# create a new column for the log median flow 
event_df <- event_df |>
  mutate(logQ = log(median_flow))

flag_outliers_IQR <- function(event_df) {
  # calculate Q1, Q3, and IQR
  Q1 = quantile(event_df$logQ, 0.25, na.rm = TRUE)
  Q3 = quantile(event_df$logQ, 0.75, na.rm = TRUE)
  IQR = Q3 - Q1
  lower = Q1 - 1.5 * IQR
  upper = Q3 + 1.5 * IQR
  # return the number of values flagged as outliers
  n_flagged_msg <- paste0("IQR-based detection determines ",
                          sum(event_df$logQ < lower | 
                                event_df$logQ > upper,
                              na.rm = TRUE),
                          " log median flow value(s) as outlier(s)"
  )
  # create a string of flagged flow values
  flagged_values <- event_df$logQ[
    event_df$logQ < lower |
      event_df$logQ > upper
  ]
  # create a second message to convey flagged values if applicable
  if (length(flagged_values) > 0) {
    flagged_val_msg <- paste0("The value(s) flagged as outlier(s) are: ",
                              paste(flagged_values, collapse = ", ")
    )
  } else {
    flagged_val_msg <- ""
  }
  # output both messages in the console
  cat(n_flagged_msg, "\n\n")
  cat(flagged_val_msg, "\n")
  # create a new column in event_df that lists T/F for outliers
  event_df <- event_df %>%
    mutate(IQR_flagged_outlier = logQ < lower | 
             logQ > upper)
}
event_df <- flag_outliers_IQR(event_df)

# create a function that determines influential observations with Cook's 
# distance
flag_cooks <- function(model, event_df) {
  # define the variables
  cooks_d <- cooks.distance(model)
  n <- length(cooks_d)
  threshold <- 4 / n
  # determine flagged values
  flagged_cooks <- which(cooks_d > threshold)
  # output messages depending on values being flagged
  if (length(flagged_cooks) > 0) {
    flagged_cooks_msg <- paste0(
      "The values flagged by Cook's distance as being influential on the model relationship between log flow and AGWRC are: ",
      paste(event_df$logQ[flagged_cooks], collapse = ", ")
    )
  } else {
    flagged_cooks_msg <- "No values were flagged as being influential on the model relationship between log flow and AGWRC"
  }
  # output message in the console
  cat(flagged_cooks_msg, "\n\n")
  # create a new column for cooks distance for each data point
  event_df <- event_df %>%
    mutate(cooks_distance = cooks_d)
  # create a new column with T/F for flagged points
  event_df <- event_df %>%
    mutate(cooks_flagged = cooks_d > threshold)
}
event_df <- flag_cooks(model, event_df)

# Write csv
write.csv(monthly_event_count, output_file, row.names = FALSE)
