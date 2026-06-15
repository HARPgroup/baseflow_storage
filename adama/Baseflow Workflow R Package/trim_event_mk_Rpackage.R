#'@title trim_event_mk
#'@name
#'trim_event_mk
#'@description
#'Runs Mann Kendall Test
#'@details
#'Creates df with outcome from MK test,
#'df includes best and original p-value, trim event start and end, window length,
#'indicator if flag_len is met, indicator if alpha is met, and kept column
#'
#'@param event_df df from locationstats.csv
#'@param alpha num variable for threshold of significance, default 0.20
#'@param min_len_mk num variable for minimum Mann Kendall points, default 3
#'@param flag_len num variable that flags events as short, default 14
#'@return df with num columns of best and original p-value, trim event start and end, window length, and
#'logical columns for meeting flag_len, alpha threshold and kept column
#'@author 
#'@importFrom dplyr mutate
#'@importFrom Kendall MannKendall
#'@export
trim_event_mk <- function(event_df, alpha = 0.20, 
                          min_len_mk = 3,   # absolute minimum to test MK
                          flag_len   = 14) { # flag if final window < this
  require(dplyr)
  require(Kendall)
  n <- nrow(event_df)
  
  mk_full <- tryCatch(Kendall::MannKendall(event_df$AGWR), error = function(e) NULL)
  pval_orig <- if (!is.null(mk_full)) mk_full$sl else NA_real_
  
  if (n < min_len_mk) {
    # Too short to do anything meaningful; keep all but flag as short if < flag_len
    return(event_df %>%
             mutate(
               mk_pval      = pval_orig,
               mk_pval_orig = pval_orig,
               trim_start   = 1L,
               trim_end     = n,
               win_len      = n,
               is_short     = n < flag_len,
               met_alpha    = !is.na(pval_orig) && pval_orig > alpha,
               kept         = TRUE
             ))
  }
  
  best_i <- 1L; best_j <- n
  best_len <- 0L; best_p <- NA_real_; best_met <- FALSE
  
  # Try all [i, j] with at least min_len_mk points
  for (i in 1:(n - (min_len_mk - 1))) {
    for (j in seq(from = n, to = i + (min_len_mk - 1), by = -1)) {
      sub <- event_df$AGWR[i:j]
      len <- j - i + 1L
      mk  <- tryCatch(Kendall::MannKendall(sub), error = function(e) NULL)
      p   <- if (!is.null(mk)) mk$sl else NA_real_
      if (is.na(p)) next
      
      met <- p > alpha
      if (met) {
        # Prefer longest; break ties with larger p
        if (len > best_len || (len == best_len && (is.na(best_p) || p > best_p))) {
          best_i <- i; best_j <- j; best_len <- len; best_p <- p; best_met <- TRUE
        }
      } else if (!best_met) {
        # Track best p even if it doesn't meet alpha
        if (is.na(best_p) || p > best_p || (p == best_p && len > best_len)) {
          best_i <- i; best_j <- j; best_len <- len; best_p <- p; best_met <- FALSE
        }
      }
    }
  }
  
  kept_idx <- seq_len(n) >= best_i & seq_len(n) <= best_j
  is_short <- best_len < flag_len
  
  event_df %>%
    mutate(
      mk_pval      = best_p,
      mk_pval_orig = pval_orig,
      trim_start   = best_i,
      trim_end     = best_j,
      win_len      = best_len,
      is_short     = is_short,
      met_alpha    = best_met,
      kept         = kept_idx
    )
}
