### This script performs iterative linear regression for a specified cfs ###
### increase in the flow values. The output is a dataframe with R-squared ###
### values and the number of data points in each "chunk" analyzed ###

library(tidyverse)

# load in the csv file for a gage of interest
#### need to replace gage number in url for intended gage###
gage_usgs_events <- read_csv("https://deq1.bse.vt.edu//usgs//agws//baseflow_summary_df_01629500.csv")

# add a new column for log flow
reg_df <- gage_usgs_events %>%
  mutate(logQ = log(median_flow))

# sort flow values from smallest to largest
reg_df <- reg_df %>%
  arrange(median_flow)

# identify minimum and maximum flows based off of row order
lowest_flow <- head(reg_df$median_flow, 1)
highest_flow <- tail(reg_df$median_flow, 1)

# create a list to hold the filtered dataframes of increasing flow
reg_df_list <- list()

# define the intended range size (flow) for iterations
range_size <- 583 ### define range ####

# define the start and end of the iteration ranges for the loop
range_start <- lowest_flow
range_end <- range_start + range_size

# create a while loop that makes a new dataframe of increasing flow value
# and row length with each iteration
while(range_end <= highest_flow) { 
  # does not let the loop run if the last value in the range is greater than
  # the highest flow value in the dataframe
  subset_reg_df <- reg_df[
    reg_df$median_flow >= range_start &
      reg_df$median_flow <= range_end,
    ]
  # creates a new dataframe that has flow values between the lowest flow and
  # the lowest flow + the range_size
  reg_df_list[[paste0("range_", range_start, "_to_", range_end)]] <- subset_reg_df
  # names each subdataframe with the range of values it includes
  range_end <- range_end + range_size
  # for the next iteration, the range doubles from that of the one before
}

# get the names of the iterated data frames
names(reg_df_list)

# create linear models for each dataframe
models <- lapply(reg_df_list, function(reg_df) {
  lm(event_AGWRC ~ logQ, data = reg_df)
})

# view model summaries
lapply(models, summary)

# extract a list of the R-squared values for each dataframe
rsq <- lapply(models, function(m) summary(m)$r.squared)

# extract the number of datapoints per dataframe
n_points <- sapply(reg_df_list, nrow)

# create a dataframe that has a column for R-squared and for number of flow data
iterative_lm_output <- data.frame(
  R_squared = unlist(rsq),
  n_data_points = unlist(n_points)
)



