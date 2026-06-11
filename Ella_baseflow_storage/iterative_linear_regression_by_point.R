### This script performs iterative linear regression one data point ###
### at a time. The outputs will be a dataframe with the R-squared value ###
### and the number of data points considered for each iteration, as well ###
### as a dataframe with the 5 highest R-squared values and at which ###
### iteration they occurred. ###

# load in the csv file for a gage of interest
#### need to replace gage number in url for intended gage ###
gage_usgs_events <- read_csv("https://deq1.bse.vt.edu//usgs//agws//baseflow_summary_df_02055000.csv")

# add a new column for log flow
reg_df <- gage_usgs_events %>%
  mutate(logQ = log(median_flow))

# sort flow values from smallest to largest
reg_df <- reg_df %>%
  arrange(median_flow)

# run a regression at each step of the dataframe
lm_models <- lapply(3:nrow(reg_df), function(i) {
  # creates a sequence of numbers from 3 up to the number of rows in req_df
  lm(event_AGWRC ~ logQ, data = reg_df[1:i, ])
})

# create a dataframe with the sample size and the r-squared value
iterative_lm_output <- data.frame(
  n_data_points = 3:nrow(reg_df),
  r_squared = sapply(lm_models, 
                     function(m) summary(m)$r.squared)
)

# remove scientific notation
iterative_lm_output$r_squared <- format(iterative_lm_output$r_squared,
                                        scientific = FALSE)

# get the iterations that yielded the top 3 highest R-squared values
sorted_r_square <- iterative_lm_output %>%
  arrange(desc(r_squared))
print(top5 <- sorted_r_square[1:5, ])

# write csv for final dataframe
write.csv(iterative_lm_output, "iterative_lm_by_points.csv")

# write csv for top 5 R-squared values and for which iterations those values
# occurred 
write.csv(top5, "Highest_R_squared.csv")