#This function can take a completed trim stats DF from the baseflow workflow
# and use the calculated recession coefficient to estimate a storage given the flow

calc_agws <- function(df) {
  df$S <- df$Flow_in / (1 - df$AGWRC)
  return(df)
}


#Ex 

#baseflow_trimmed_stats_02024000_storage <- baseflow_trimmed_stats02024000 %>% 
  #mutate(AGWS = calc_agws(Q, AGWRC))