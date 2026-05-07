# Verification Example
CS_data <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/bf_events_01632000.csv")
MJ_data <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/bf_events_01633000.csv")
SB_data <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ben_bf_events/bf_events_01634000.csv")

check <- run_one_site_regression("01634000")
da_sqmi <- get_drainage_area_sqmi("01634000")

quantile(event_data$Flow) # Choose 106 cfs and 270 cfs

# Verification calcs
Q_cfs <- 189.5
C_cfs <- check$m[1] * log(Q_cfs) + check$b[1]

Q_in <- convert.flow(Q_cfs, da_sqmi)
C_in <-	-0.0175015 * log(Q_in) + 0.8911815

S_in <- Q_in / (1 - C_in)

C_lkp <- solve_agwrc_lookup(S_in, Svar)

C_lkp

# Making new site coef tab;le
CS_cfs <- run_one_site_regression("01632000")
CS_in <- run_one_site_regression("01632000", regression_flow_col = "flow_in_day", add_inches_day = TRUE)
CS_cfs$landseg <- "N51165"
CS_in$landseg <- "N51165"
MJ_cfs <- run_one_site_regression("01633000")
MJ_in <- run_one_site_regression("01633000", regression_flow_col = "flow_in_day", add_inches_day = TRUE)
MJ_cfs$landseg <- "N51171"
MJ_in$landseg <- "N51171"
SB_cfs <- run_one_site_regression("01634000")
SB_in <- run_one_site_regression("01634000", regression_flow_col = "flow_in_day", add_inches_day = TRUE)
SB_cfs$landseg <- "N51187"
SB_in$landseg <- "N51187"

coefs <- rbind(CS_cfs,CS_in, MJ_cfs, MJ_in, SB_cfs, SB_in)

