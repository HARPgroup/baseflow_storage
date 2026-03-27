###FUNCTION #1/1.5: CORE CALCULATIONS###
#AGWR = Qt / Qt-1
calc_AGWR <- function(x) {
  c(NA, x[-1] / x[-length(x)])
}
#delta_AGWR = AGWR_t / AGWR_t-1
calc_delta_AGWR <- function(x) {
  c(NA, x[-1] / x[-length(x)])
}

###FUNCTION #2: ADD SEASONAL INFO###
add_month_season <- function(df) {
  require(dplyr)
  df %>% mutate(
    Month = format(Date, "%m"),
    Season = case_when(
      Month %in% c("12", "01", "02") ~ "Winter",
      Month %in% c("03", "04", "05") ~ "Spring",
      Month %in% c("06", "07", "08") ~ "Summer",
      Month %in% c("09", "10", "11") ~ "Fall",
      TRUE ~ NA_character_
    )
  )
}

###FUNCTION #3: GAP FILLER###
gap_fill <- function(flag_vec, max_gap = 5) {
  flag_vec[is.na(flag_vec)] <- FALSE
  rle_out <- rle(flag_vec)
  lengths <- rle_out$lengths
  values <- rle_out$values
  
  for (i in seq(2, length(values) - 1)) {
    if (!values[i] && lengths[i] <= max_gap && values[i - 1] && values[i + 1]) {
      values[i] <- TRUE
    }
  }
  # for(i in seq(2, length(values) - 1)) {
  #   # Compare flow at i to overall mean flow
  #   if (flow_vec[i] >= 1.15 * mean(flow_vec, na.rm = TRUE)) {
  #     values[i] <- FALSE
  #   }
  # }
  
  inverse.rle(list(lengths = lengths, values = values))
}

###FUNCTION #4: FLAG STABLE BASEFLOW###
flag_stable_baseflow <- function(df,
                                 flow_col,
                                 AGWR_col = "AGWR",
                                 delta_col = "delta_AGWR",
                                 delta_thresh = 0.03,
                                 max_gap = 3) {
  AGWR <- df[[AGWR_col]]
  delta <- df[[delta_col]]
  
  is_stable <- abs(delta - 1.0) < delta_thresh & AGWR < 1.0
  df$RecessionDay <- gap_fill( is_stable, max_gap)
  
  for(i in 1:length(df$RecessionDay)) {
    # Compare flow at i to overall mean flow
    if (flow_col[i] >= 1.15 * mean(flow_col, na.rm = TRUE)) {
      df$RecessionDay[1] <- FALSE
    }
    return(df)
  }
}