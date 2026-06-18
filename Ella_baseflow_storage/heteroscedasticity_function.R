### This function quantifies heteroscedasticity ###
library(lmtest)
library(tidyverse)

# read in the data and use the same name as bf workflow output
event_df <- read_csv("https://deq1.bse.vt.edu//usgs//agws//baseflow_summary_df_01629500.csv")

# create a new column for the log median flow 
event_df <- event_df |>
  mutate(logQ = log(median_flow))

# create a linear model
model <- lm(event_AGWRC ~ logQ, data = event_df)

# create a function that runs the Breusch-Pagan test and the White test
# the ouput is a message that gives p-value and interpretation
# flag values that have p-value < 0.1
heteroscedasticity <- function(model){
  # run the Breusch-Pagan test 
  bp_test <- bptest(model)
  
  # write message with interpretation
  bp_msg <- paste0(
    "Breusch-Pagan test p-value = ",
    round(bp_test$p.value, 4),
    if(bp_test$p.value < 0.1){
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
    }
    else{
      ". Heteroscedasticity is likely not a concern."
    }
  )
  
  cat(bp_msg, "\n\n")
  cat(white_msg, "\n")
}
heteroscedasticity(model)
