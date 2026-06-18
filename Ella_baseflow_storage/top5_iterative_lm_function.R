### This function returns the iterative linear regression values that have ###
### the top 5 highest R-squared value ###

top5_sorted <- function(iterative_lm_df) {
  # sort from highest to lowest R_squared value
  top5 <- iterative_lm_df %>%
    arrange(desc(r_squared))
  top5 <- top5[1:5, ]
}