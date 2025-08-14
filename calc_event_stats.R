calc_event_stats <- function (data, event_num, dAGWRmax, dAGWRmin){
require(sqldf)
  
sqldf_query <- paste0(
  "select * from data 
    where GroupID = ", event_num, " and
    AGWR < 1 and
    delta_AGWR < ", dAGWRmax," and
    delta_AGWR > ", dAGWRmin,"
    ")

event_data <- sqldf(sqldf_query)

# Create lm of event selected dates
logFlow_lm <-lm(log(event_data$Flow) ~ event_data$Date)
event_sum <- summary(logFlow_lm)

# Assign AWGR and R-squared
AGWR <- exp(event_sum$coefficients[[2,1]])
R_squared <- event_sum$r.squared

new_row <- data.frame(event_num, AGWR, R_squared)

return(new_row)

}
