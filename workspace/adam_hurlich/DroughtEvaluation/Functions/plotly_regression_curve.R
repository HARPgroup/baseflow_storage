library(plotly)

step6a <- read.csv("C:/HARP/HARP - GitHub/baseflow_storage/adama/flag_stormflow/03478400/step06a_02016000.csv")
step7a <- read.csv("C:/HARP/HARP - GitHub/baseflow_storage/adama/flag_stormflow/03478400/step07a_02016000.csv")

m = -0.013562580154482
b = 1.02202214121864

m <- step7a$m
b <- step7a$b

# Your data
df <- data.frame(
  x = step6a$median_flow,
  y = step6a$event_AGWRC
)


# Smooth curve
curve_df <- data.frame(
  x = seq(min(df$x), max(df$x), length.out = 500)
)

curve_df$y <- m * log(curve_df$x) + b

# Plot
plot_ly() %>%
  add_markers(
    data = df,
    x = ~x,
    y = ~y,
    name = "Observed",
    marker = list(size = 8)
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
    title = "AGWRC vs. Flow (Event-Level) - USGS-02016000 - +/- 11 Day Window"
  )

Q = 490
y <- (log(Q) * m) + b
