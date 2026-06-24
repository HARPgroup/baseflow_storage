# create a function that runs the Breusch-Pagan test and the White test
# the ouput is a message that gives p-value and interpretation
# flag values that have p-value < 0.05
heteroscedasticity <- function(model) {
  # run the Breusch-Pagan test
  bp_test <- bptest(model)
  
  # write message with interpretation
  bp_msg <- paste0(
    "Breusch-Pagan test p-value = ",
    round(bp_test$p.value, 4),
    if(bp_test$p.value < 0.05) {
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
    if(white_test$p.value < 0.05){
      ". Heteroscedasticity is likely causing some uncertainty in the model."
    } else {
      ". Heteroscedasticity is likely not a concern."
    }
  )
  
  # output both messages in the console
  cat(bp_msg, "\n\n")
  cat(white_msg, "\n\n")
  
  # Create data frame
  hetero_df <- data.frame(
    white_test_p = round(white_test$p.value, 4),
    breusch_pagan_p = round(bp_test$p.value, 4)
  )
  
  # add test statistics to dataframe
  hetero_df <- hetero_df %>%
    mutate(white_test_stat = round(white_test$statistic, 4)) %>%
    mutate(breusch_pagan_stat = round(bp_test$statistic, 4))
  
  # add T/F statement for heteroscedasticity significance
  hetero_df <- hetero_df %>%
    mutate(heteroscedasticity_a_concern = (breusch_pagan_p <= 0.05) |
             (white_test_p <= 0.05)
           )
  return(hetero_df)
}
hetero <- heteroscedasticity(model)

write.csv(hetero,
          paste0("step08_", gageID, "_heteroscedasticity_results.csv"),
          row.names = FALSE
          )
