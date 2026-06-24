# This function runs iterative linear regression 
iterative_lm <- function(event_df) {
  
  # add a new column for log flow
  iterative_lm_df <- event_df %>%
    mutate(logQ = log(median_flow)) %>%
    # sort flow values from smallest to largest
    arrange(median_flow)
  
  # calculate the r-squared value of all flow data
  model <- lm(event_AGWRC ~ logQ, data = iterative_lm_df)
  total_r_squared = summary(model)$r.squared
  
  # run a regression at each step of the dataframe
  lm_models <- lapply(3:nrow(iterative_lm_df), function(i) {
    # creates a sequence of numbers from 3 up to the number of rows in event_df
    lm(event_AGWRC ~ logQ, data = iterative_lm_df[1:i, ])
  }
  )
  
  # create a dataframe with the sample size, the r-squared value, b, and m
  iterative_lm_df <- data.frame(
    n_data_points = 3:nrow(iterative_lm_df),
    min_flow = sapply(3:nrow(iterative_lm_df),
                      function(i) min(iterative_lm_df$median_flow[1:i], 
                                      na.rm = TRUE)
    ),
    max_flow = sapply(3:nrow(iterative_lm_df),
                      function(i) max(iterative_lm_df$median_flow[1:i], 
                                      na.rm = TRUE)
    ),
    r_squared = sapply(lm_models, 
                       function(m) summary(m)$r.squared
    ),
    m = sapply(lm_models,
               function(m) coef(m)[["logQ"]]
    ), 
    b = sapply(lm_models,
               function(m) coef(m)[["(Intercept)"]]
    )
  )
  
  # return the number of r-sqaured values that are higher than the total
  # R-squared value
  higher_r_squared_msg <- paste0("The iterations found ", 
                                 sum(iterative_lm_df$r_squared > total_r_squared), 
                                 " R_squared values that were higher than the total R_squared value of ",
                                 total_r_squared)
  
  # output R_squared comparison message in the console
  cat(higher_r_squared_msg, "\n\n")
  
  # return the r-squared values that are higher than the total R-squared value
  # higher_r_squared <- iterative_lm_df %>%
  # filter(r_squared > total_r_squared)
  # print(higher_r_squared)
  
  return(iterative_lm_df)
}

### This function returns the iterative linear regression values that have ###
### the top 5 highest R-squared value ###

top5_sorted <- function(iterative_lm_df) {
  # sort from highest to lowest R_squared value
  top5 <- iterative_lm_df %>%
    arrange(desc(r_squared))
  top5 <- top5[1:5, ]
}
