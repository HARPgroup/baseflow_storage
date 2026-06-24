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
  
  #Create data frame
  hetero_df <- date.frame(
    white_test_p = round(white_test$p.value, 4),
    breusch_pagan_p = round(bp_test$p.value, 4)
  )
  return(hetero_df)
}