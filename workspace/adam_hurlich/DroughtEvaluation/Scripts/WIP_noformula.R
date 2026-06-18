regression_csv <- read_csv("https://deq1.bse.vt.edu/usgs/agws/baseflow_regression_df_02056000.csv")

Q = 263
AGWRC <- (regression_csv$m * log(Q)) + regression_csv$b
