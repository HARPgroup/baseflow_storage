library(ggplot2)

ggplot(event_df, aes(x = median_flow, y = event_AGWRC)) +
  geom_point(size = 1.5) +
  labs(
    title = "Median Flow vs. AGWRC For Roanoke",
    x = "Q (cfs)",
    y = "AGWRC"
  ) +
  theme_classic()


#calculate quantile flows by month for gage data
usgs_gage_stats <- usgs_gage %>% 
  group_by(mo) %>% 
  summarize(
    usgs_05p = quantile(obs_flow, 0.05, na.rm = TRUE),
    usgs_10p = quantile(obs_flow, 0.10, na.rm = TRUE),
    usgs_25p = quantile(obs_flow, 0.25, na.rm = TRUE),
  )

#inner_join usgs_low_flows and usgs_gage_stats by month
low_flow_analysis <- inner_join(
  usgs_low_flow,
  usgs_gage_stats,
  by = c("mo")
) %>% 
  select(GroupID, start_date, mo, median_flow, event_AGWRC,
         usgs_05p, usgs_10p, usgs_25p)

#month to number 
low_flow_analysis$mo <- factor(
  low_flow_analysis$mo,
  levels = 1:12,
  labels = month.abb)

# box plot
ggplot(low_flow_analysis) +
  geom_boxplot(aes(x = mo, y = median_flow), size = 0.33)+
  geom_point(aes(x = mo, y = usgs_05p), color = "blue", size = 1.5)+
  geom_point(aes(x = mo, y = usgs_10p), color = "red", size = 1.5)+
  geom_point(aes(x = mo, y =  usgs_25p), color = "green", size = 1.5)+
  labs( x = "Month", y = "Flow (cfs)", title = "USGS Low Flow vs. Quantile flow")+
  theme_minimal()
