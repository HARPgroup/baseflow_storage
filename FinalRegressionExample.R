# FINAL REGRESSION EXAMPLE SCRIPT
# Runs regression for selected gages and creates final comparison table

#
# Source final regression functions
#

source("FinalRegression.R")

#
# Choose regression flow metric
#

# Use "Flow" for cfs
# Use "flow_in_day" for watershed inches/day
regression_flow_col <- "flow_in_day"

#
# Site lookup table
#

site_lookup <- data.frame(
  site_no = c("01632000", "01633000", "01634000"),
  site = c("Cootes Store", "Mount Jackson", "Strasburg"),
  Landseg = c("N51165", "N51171", "N51187")
)

#
# Run all regressions
#

regression_results <- dplyr::bind_rows(
  run_one_site_regression("01632000", regression_flow_col = regression_flow_col),
  run_one_site_regression("01633000", regression_flow_col = regression_flow_col),
  run_one_site_regression("01634000", regression_flow_col = regression_flow_col)
)

#
# Final comparison table
#

final_regression_table <- regression_results %>%
  left_join(site_lookup, by = "site_no") %>%
  select(site, flow_metric, m, b, Landseg)

print(final_regression_table)

#
# Optional export
#

output_file <- paste0(
  "agwrc_regression_coefficients_",
  regression_flow_col,
  ".csv"
)

utils::write.csv(
  final_regression_table,
  output_file,
  row.names = FALSE
)