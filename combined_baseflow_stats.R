library(dplyr)
#reads in the two csv
baseflow_trimmed_stats <- read_csv("C:\\HARP Folder\\baseflow_storage\\baseflow_trimmed_stats_01636500.csv")
baseflow_trimmed_stats <- baseflow_trimmed_stats

baseflow_summary_stats <-read_csv ("C:\\HARP Folder\\baseflow_storage\\baseflow_summary_df_01636500.csv")
baseflow_summary_stats <- baseflow_summary_stats

# selects GroupID and median_flow columns from baseflow_summary_stats
trimmed_baseflow_summary_stats <- baseflow_summary_stats %>% 
  select(GroupID, median_flow)

# combined all columns into one object 
combined_baseflow_stats <- merge(baseflow_trimmed_stats, trimmed_baseflow_summary_stats, by = "GroupID", all.x = TRUE)

write.csv(combined_baseflow_stats, "Shenandoah_Millville_combined_baseflow_stats.csv", row.names = FALSE)