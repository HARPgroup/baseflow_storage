library(dataRetrieval)
library(dplyr)

site_no <- "01632000"  #cootes store
param_cd <- "00060"    #discharge
delta_thresh_min <- 0.97
delta_thresh_max <- 1.03
forecast_days <- c(30, 60, 120, 240, 480)

#load data
flows <- readNWISdv(siteNumbers = site_no,
                    parameterCd = param_cd,
                    statCd = "00003",   #mean daily flow
                    startDate = "1984-01-01",
                    endDate = Sys.Date()) %>%
  renameNWISColumns() %>%
  select(Date, Flow)

#calculate AGWR and delta_AGWR (like before)
calc_AGWR <- function(x) c(NA, x[-1] / x[-length(x)])
calc_delta_AGWR <- function(x) c(NA, x[-1] / x[-length(x)])

flows <- flows %>%
  mutate(
    AGWR = calc_AGWR(Flow),
    delta_AGWR = calc_delta_AGWR(AGWR)
  )

#flag valid AGWR days (stable recession)
flows <- flows %>%
  mutate(
    AGWR_valid = !is.na(AGWR) & !is.na(delta_AGWR) &
      AGWR < 1 &                            # <-- NEW
      delta_AGWR >= 0.97 &
      delta_AGWR <= 1.03
  )

#project flow for valid AGWR days
flows <- flows %>%
  rowwise() %>%
  mutate(
    Flow_30d  = ifelse(AGWR_valid, Flow * AGWR^30, NA_real_),
    Flow_60d  = ifelse(AGWR_valid, Flow * AGWR^60, NA_real_),
    Flow_120d = ifelse(AGWR_valid, Flow * AGWR^120, NA_real_),
    Flow_240d = ifelse(AGWR_valid, Flow * AGWR^240, NA_real_),
    Flow_480d = ifelse(AGWR_valid, Flow * AGWR^480, NA_real_)
  ) %>%
  ungroup()


# #try to plot
# library(ggplot2)
# library(tidyr)
# #pivot to long format
# flows_long <- flows %>%
#   select(Date, Flow, AGWR_valid, starts_with("Flow_")) %>%
#   pivot_longer(
#     cols = starts_with("Flow_"),
#     names_to = "Horizon",
#     values_to = "Forecast"
#   ) %>%
#   mutate(Horizon = factor(Horizon, levels = c("Flow_30d", "Flow_60d", "Flow_120d", "Flow_240d", "Flow_480d")))
# 
# #plot
# ggplot() +
#   geom_line(data = flows, aes(x = Date, y = Flow), color = "black", linewidth = 0.5) +
#   geom_point(data = flows_long %>% filter(AGWR_valid),
#              aes(x = Date, y = Forecast, color = Horizon),
#              size = 1.5, alpha = 0.7) +
#   scale_color_brewer(palette = "Set1", name = "Projected Flow") +
#   labs(
#     title = "Observed Flow with No-Rain Forecasts",
#     subtitle = "Forecasted flow under stable AGWR conditions only",
#     x = "Date", y = "Flow (cfs)"
#   ) +
#   coord_cartesian(ylim = c(0, 800)) +  #visual cap so we can actually see points
#   theme_minimal() +
#   theme(legend.position = "right")


#plot for just one day
target_date <- as.Date("2020-04-28")
flow_day <- flows %>% filter(Date == target_date)

# ilona_target_date <- as.Date("1987-03-29")
# ilona_flow_day <- flows %>% filter(Date == ilona_target_date)

future_flows <- tibble(
  Day = c(0, 30, 60, 120, 240, 480),
  Flow = c(flow_day$Flow,
           flow_day$Flow_30d,
           flow_day$Flow_60d,
           flow_day$Flow_120d,
           flow_day$Flow_240d,
           flow_day$Flow_480d)
) %>%
  mutate(Label = Flow)

ggplot(future_flows, aes(x = Day, y = Flow)) +
  geom_line(color = "blue", linewidth = 0.8) +
  geom_point(size = 3, color = "darkblue") +
  geom_text(aes(label = Label), vjust = -0.7, size = 3.5) +
  labs(
    title = paste("Forecasted Flow from", target_date),
    subtitle = "Assuming constant AGWR and no rainfall",
    x = "Days into Future",
    y = "Flow (cfs)"
  ) +
  theme_minimal()


##TESTING A MORE MATHEMATICAL APPROACH##
#compute decay constant k from AGWR (k = -ln(AGWR))
#apply exponential decay: Q(t) = Q0 * e^(-k * t)
forecast_days_decay <- c(30, 60, 120, 240, 480)

flows <- flows %>%
  rowwise() %>%
  mutate(
    decay_k = ifelse(AGWR_valid, -log(AGWR), NA_real_),
    Flow_30d  = ifelse(AGWR_valid, Flow * exp(-decay_k * 30), NA_real_),
    Flow_60d  = ifelse(AGWR_valid, Flow * exp(-decay_k * 60), NA_real_),
    Flow_120d = ifelse(AGWR_valid, Flow * exp(-decay_k * 120), NA_real_),
    Flow_240d = ifelse(AGWR_valid, Flow * exp(-decay_k * 240), NA_real_),
    Flow_480d = ifelse(AGWR_valid, Flow * exp(-decay_k * 480), NA_real_)
  ) %>%
  ungroup()