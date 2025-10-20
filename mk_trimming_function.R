require(dplyr)
require(ggplot2)
require(Kendall)
require(gridExtra)

trim_event_mk <- function(event_df, alpha = 0.1) {
  n <- nrow(event_df)
  
  # compute MK p-value for the full event (no trimming)
  mk_full <- tryCatch(Kendall::MannKendall(event_df$AGWR), error = function(e) NULL)
  pval_orig <- if (!is.null(mk_full)) mk_full$sl else NA
  
  if (n < 5) {
    return(event_df %>%
             mutate(
               mk_pval = NA,         
               mk_pval_orig = pval_orig,  
               trim_start = 1,
               kept = TRUE
             ))
  }
  
  trim_start <- 1
  pval_final <- NA
  
  for (i in seq_len(n - 2)) {
    sub_df <- event_df[i:n, ]
    
    mk <- tryCatch(Kendall::MannKendall(sub_df$AGWR), error = function(e) NULL)
    pval <- if (!is.null(mk)) mk$sl else NA
    
    if (!is.na(pval) && pval > alpha) {
      trim_start <- i
      pval_final <- pval
      break
    }
  }
  
  
  event_df %>%
    mutate(
      mk_pval = pval_final,     
      mk_pval_orig = pval_orig,
      trim_start = trim_start,
      kept = row_number() >= trim_start
    )
}