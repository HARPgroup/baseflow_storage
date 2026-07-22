window_lm_table <- function(event, min_win = NULL, weight_power = 1) {
  
  event <- event %>%
    mutate(
      Date = as.Date(Date),
      Day = as.numeric(Date)
    ) %>%
    arrange(Date)
  
  n <- nrow(event)
  
  # Default min_win to half the event length
  if (is.null(min_win)) {
    min_win <- ceiling(n / 2)
  }
  
  results <- list()
  k <- 1
  
  for (win_len in seq(n, min_win, by = -1)) {
    
    for (i in 1:(n - win_len + 1)) {
      
      j <- i + win_len - 1
      
      window <- event[i:j, ]
      
      fit <- lm(AGWR ~ Day, data = window)
      s <- summary(fit)
      r2 <- s$r.squared
      
      #Length-weighted R2
      
      length_weight <- (win_len / n) ^ weight_power
      weighted_r2 <- r2 * length_weight
      
      results[[k]] <- data.frame(
        i             = i,
        j             = j,
        win_len       = win_len,
        date_start    = min(window$Date),
        date_end      = max(window$Date),
        slope         = coef(fit)[2],
        intercept     = coef(fit)[1],
        r2            = r2,
        adj_r2        = s$adj.r.squared,
        weighted_r2   = weighted_r2,
        rmse          = sqrt(mean(residuals(fit)^2)),
        p_value       = coef(s)[2, 4]
      )
      
      k <- k + 1
    }
  }
  
  bind_rows(results)
}
