library(tidyverse)

model_events <- read_csv("bf_model_events_01632000.csv")
usgs_events <- read_csv("cootes_store_usgs_flow.csv")

#join the model dataset and the usgs dataset
joined_events <- model_events |> 
  inner_join(usgs_events, by = "Date") |> 
  select(Date = Date,
         Flow_model = Flow.x,
         Flow_usgs = Flow.y) |> 
  mutate(month = month(Date))

#create a hydrograph with model and usgs data
ggplot(joined_events, aes(Date, Flow_model, col = "Model"))+
  geom_line()+
  geom_line(aes(y = Flow_usgs, col = "USGS"))+
  theme_classic()+
  labs(x = element_blank(),
       y = "Q (cfs)")

#create a new column with the 10th percentile flow
joined_events_10p <- joined_events |> 
  mutate(month = month(Date)) |> 
  group_by(month) |> 
  summarize(usgs_10p = quantile(Flow_usgs, probs = 0.1),
            model_10p = quantile(Flow_model, probs = 0.1))

joined_events <- joined_events |> 
  left_join(joined_events_10p, by = "month", multiple = "all")

#create a 