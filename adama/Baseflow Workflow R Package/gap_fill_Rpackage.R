#'@title gap_fill
#'@name
#'gap_fill
#'@description
#'Locates periods of missed recession events
#'@details
#'Fills all false values with a consecutive length less than acceptable gap with 
#'true values if sequence of false values are between true values
#'
#'@param flag_vec A logical vector input calculated from AGWR, deltaAGWR and a deltaAGWR threshold
#'@param max_gap A numerical value for acceptable gap of falses, default is 5
#'@return A logical vector defining recession days 
#'@author 
#'@example
#'#Sample AGWR and deltaAGWR values, with threshold for change in deltaAGWR and max_gap
#'AGWR <- c(0.8430233, 0.9379310, 0.9338235, 1.0708661, 0.9338235, 0.9311024, 0.9260042, 0.9200913)
#'delta <- c(0.8871349, 1.1125803, 0.9956207, 1.1467543, 0.8720264, 0.9970860, 0.9945246, 0.9936146)
#'df <- data.frame(AGWR, delta)
#'delta_thresh = 0.03
#'max_gap = 3
#'#Creates logical vector of recession events
#'flag_vec <- abs(delta - 1.0) < delta_thresh & AGWR < 1.0
#'#Applies gap_fill function to locate missed recession events
#'df$RecessionDay <- gap_fill(flag_vec, max_gap)
#'@importFrom dplyr mutate case_when
#'@export
gap_fill <- function(
  #A logical vector input calculated from AGWR, deltaAGWR and a deltaAGWR threshold  
  flag_vec, 
  #A numerical value for acceptable gap of falses, default is 5
  max_gap = 5) {
  #NA values in flag_vec are defined as FALSE values
  flag_vec[is.na(flag_vec)] <- FALSE
  #Runs run length encoding function, creates lengths column for length of consecutive True/False values, 
  #and values column for identifying if a period is True or False
  rle_out <- rle(flag_vec)
  #Assigns rle function lengths to lengths obj
  lengths <- rle_out$lengths
  #Assigns rle function values to values obj
  values <- rle_out$values
  #Runs loop for all values except first and last position
  for (i in seq(2, length(values) - 1)) {
  #If the current index is False, its length <= max_gap, 
  #and the surrounding values are True, run next line  
    if (!values[i] && lengths[i] <= max_gap && values[i - 1] && values[i + 1]) {
  #Value of current index becomes True
      values[i] <- TRUE
    }
  }

# UNSURE 
  # for(i in seq(2, length(values) - 1)) {
  #   # Compare flow at i to overall mean flow
  #   if (flow_vec[i] >= 1.15 * mean(flow_vec, na.rm = TRUE)) {
  #     values[i] <- FALSE
  #   }
  # }
  
  #Reconstructs rle_out obj to update for new True events
  inverse.rle(list(lengths = lengths, values = values))
}


