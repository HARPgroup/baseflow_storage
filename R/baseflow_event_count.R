library(tidyverse)

baseflow_events1 <- read_csv("bf_events_01634000.csv") |>
  select(Date, Flow, Month, GroupID) |>
  group_by(Month) |>
  summarize(event_cnt = n_distinct(GroupID))

plot1 <- ggplot(baseflow_events1, aes(Month, event_cnt))+
  geom_col()+
  theme_classic()+
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(y = "Number of Baseflow Events",
       title = "Total Number of Baseflow Events by Month (Strasburg)")
