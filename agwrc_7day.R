# Scatter plots for Flow vs AGWRC

#Event based
baseflow_events <- read_csv("bf_events_01634000.csv") |>
  select(Date, Flow, Month, GroupID, AGWRC) |>
  group_by(GroupID) |>
  summarise(min_event = min(Flow),
            AGWRC = AGWRC)

ggplot(baseflow_events, aes(min_event, AGWRC))+
  geom_point()+
  theme_minimal()+
  labs(x = "Minimum Event Flow",
       title = "Strasburg")

# Daily
usgs_daily <- read_csv("strasburg_usgs_flow.csv") |>
  select(Date, Flow) |>
  mutate(xdaymin = frollmin(Flow,
                            7,
                            fill = NA,
                            na.rm = T)) |> 
  select(Date, xdaymin) |> 
  drop_na(xdaymin)

# use official function
get_AGWRC <- function(data){
  
  # Create lm of data
  logFlow_lm <-lm(log(data[, 2]) ~ data[, 1])
  event_sum <- summary(logFlow_lm)
  
  # Assign AWGRC
  AGWRC <- exp(event_sum$coefficients[[2,1]])
  return(AGWRC)
}

get_Rsq <- function(data){
  
  # Create lm of data
  logFlow_lm <-lm(log(data[, 2]) ~ data[, 1])
  event_sum <- summary(logFlow_lm)
  
  # Assign R-squared
  R_squared <- event_sum$r.squared
  return(R_squared)
}

usgs_daily <- usgs_daily |> 
  mutate(AGWRC = zoo::rollapply(data = cbind(Date, xdaymin),
                                width = 7,
                                FUN = get_AGWRC,
                                align = "right",
                                by.column = FALSE,
                                fill = NA),
         Rsq = zoo::rollapply(data = cbind(Date, xdaymin),
                         width = 7,
                         FUN = get_Rsq,
                         align = "right",
                         by.column = FALSE,
                         fill = NA))
                                
                           
ggplot(usgs_daily, aes(xdaymin, AGWRC))+
  geom_point()+
  scale_x_log10()+
  theme_minimal()

