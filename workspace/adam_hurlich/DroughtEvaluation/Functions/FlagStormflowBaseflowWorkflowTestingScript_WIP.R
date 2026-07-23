library(dplyr)
library(agws)
library(plotly)

step1 <- read.csv("C:/HARP/HARP - GitHub/baseflow_storage/adama/flag_stormflow/02016000/BaseflowWorkflow_02016000.csv")

step1$Date <- as.Date(step1$Date)

step2 <- flag_stormflow(df = step1)

step2 <- baseflow_groupID(df = step2, value = step2$Flow, time = step2$Date, min_len = 14)

#####-STEP3-flag_stormflow_AGWR#####

step3 <- flag_stormflow_AGWR(df = step2)

step3 <- baseflow_groupID(df = step3, value = step3$Flow, time = step3$Date)

#####-STEP4-residual_flow-#####

step4 <- residual_flow(df = step3)

step4 <- baseflow_groupID(df = step4, value = step4$Flow, time = step4$Date)

#####-STEP5-Quantile-#####

step5 <- step4 |>
  filter(Flow < quantile(Flow, 0.90))

step5 <- baseflow_groupID(df = step5, value = step5$Flow, time = step5$Date)

#####-STEP6-regression-#####

step6 <- calc_AGWRC(df = step5, value = "Flow", time = "Date")

step6_model <- fit_agwrc_regression(step6)

#####-STEP7-model-summary-#####

model_summary <- summary(step6_model)

m = coef(model_summary)[2]
b = coef(model_summary)[1]

df <- data.frame(
  x = step6$median_flow,
  y = step6$event_AGWRC,
  label = paste0("GroupID: ", step6$GroupID)
)

# Smooth curve
curve_df <- data.frame(
  x = seq(min(df$x), max(df$x), length.out = 500)
)

curve_df$y <- m * log(curve_df$x) + b


# Plotly AGWRC vs. Median Flow
plot_ly() %>%
  add_markers(
    data = df,
    x = ~x,
    y = ~y,
    name = "Observed",
    marker = list(size = 8),
    mode = "markers+text",
    text = ~label,
    textposition = "top center"
  ) %>%
  add_lines(
    data = curve_df,
    x = ~x,
    y = ~y,
    name = "y = m log(x) + b",
    line = list(width = 3)
  ) %>%
  layout(
    xaxis = list(title = "Characteristic Event Flow (median, cfs)"),
    yaxis = list(title = "Event AGWRC"),
    title = "AGWRC vs. Flow (Event-Level) - USGS-02016000"
  )
