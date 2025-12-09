bf_event_stats <- function(data, flow_col="Flow", date_col="Date"){

  data[[date_col]]<-as.Date(data[[date_col]])
  
  # Create lm of data
  logFlow_lm <-lm(log(data[[flow_col]]) ~ data[[date_col]])
  event_sum <- summary(logFlow_lm)
  
  # Assign AWGRC and R-squared
  AGWRC <- exp(event_sum$coefficients[[2,1]])
  R_squared <- event_sum$r.squared
  
  return(list(AGWRC = AGWRC, R_squared = R_squared))
  
}
