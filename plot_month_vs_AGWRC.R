#load in RoanokeSummaryStats.csv
#change csv and title of graph for different gage
usgs_low_flow <- read_csv("C:\\HARP Folder\\baseflow_storage\\baseflow_summary_df_02055000.csv")
usgs_low_flow <- usgs_low_flow %>%
  mutate(
    start_date = as.Date(start_date),
    mo = month(start_date)
  )

#Boxplot to compare median flow to AGWRC
ggplot(usgs_low_flow,
       aes(x = factor(mo,
                      levels = 1:12,
                      labels = month.abb),
           y = event_AGWRC)) +
  geom_boxplot(fill = "white") +
  labs(
    title = "Monthly Distribution of AGWRC For Roanoke (02055000)",
    x = "Month",
    y = "AGWRC"
  ) +
  theme_classic()