### This script performs iterative linear regression for every 50 cfs ###
### increase in the flow values. This allows for comparison of R-squared ###
### values as flow increases and more data is accounted for ###

library(tidyverse)

# load in the csv file for a gage of interest
#### need to replace "gage" with name of gage from workflow ###
gage_usgs_events <- read_csv("gageSummaryStats.csv")

# add a new column for log flow
reg_df <- gage_usgs_events %>%
  mutate(logQ = log(median_flow))

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
range_size <- ### define range ####

# define the start and end of the iteration ranges for the loop
range_start <- lowest_flow
range_end <- range_start + range_size

# create a while loop that makes a new dataframe of increasing flow value
# and row length with each iteration
while(range_end <= highest_flow) {
  subset_reg_df <- reg_df[
    reg_df$median_flow >= range_start &
      reg_df$median_flow <= range_end,
    ]
  reg_df_list[[paste0("range_", range_start, "_to_", range_end)]] <- subset_reg_df
  range_end <- range_end + range_size
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


