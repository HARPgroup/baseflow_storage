### This script quantifies heteroscedasticity 

library(tidyverse)

# load in the csv file for a gage of interest
# in this case, gage 01629500
Luray_usgs_events <- read_csv("LuraySummaryStats.csv")

# add a new column for log flow
reg_df <- Luray_usgs_events %>%
  mutate(logQ = log(median_flow))

# run a linear model where event_AGWRC is the dependent variable
# and logQ is the independent variable
model <- lm(event_AGWRC ~ logQ, data = reg_df)
model_summary <- summary(model)

# Plot AGWRC vs logQ with regression line
ggplot(reg_df, aes(x = logQ, y = event_AGWRC)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "red",
              se = FALSE) + 
  labs(x = "Log Median Flow (cfs)",
       y = "AGWRC",
       title = "AGWRC vs Log Median Flow for South Fork Shenandoah River near Luray, VA")

# create a dataframe with fitted values and residuals for visual inspection
heteroscedasticity_test <- data.frame(
  Fitted = fitted(model),
  Residuals = resid(model)
)

# plot fitted values vs residuals 
ggplot(heteroscedasticity_test, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  theme_classic() +
  labs(
    title = "Fitted Values vs Residuals for Luray, VA",
    x = "Fitted Values",
    y = "Residuals"
  )

# Breusch–Pagan test for quantitative approach
# this test does assume normal residuals
library(lmtest)
bp_test <- bptest(model)
print(bp_test)

# White test for quantitative approach
# this test is a more general approach that does not assume a linear form
# for heteroscedasticity ---> more flexible test
white_test <- bptest(model, ~fitted(model) + I(fitted(model)^2))
print(white_test)

#### Interpretation ####
# Ho: homoscedasticity (residual variance is constant)
# Ha: heteroscedasticity (residual variance changes with predictors)
# if p-value < 0.05 --> reject Ho (sufficient evidence for heteroscedasticity)
# if p-value > 0.05 --> fail to reject Ho (no sufficient evidence for heteroscedasticity)





