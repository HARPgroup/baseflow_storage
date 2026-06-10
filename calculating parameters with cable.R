library(dataRetrieval)
library(kableExtra)

# CFS coefficients
coeff_CS_cfs <- run_one_site_regression("01632000", regression_flow_col = "Flow")
coeff_MJ_cfs <- run_one_site_regression("01633000", regression_flow_col = "Flow")
coeff_S_cfs  <- run_one_site_regression("01634000", regression_flow_col = "Flow")

# Inches coefficients
coeff_CS_in <- run_one_site_regression("01632000", regression_flow_col = "flow_in_day", add_inches_day = TRUE)
coeff_MJ_in <- run_one_site_regression("01633000", regression_flow_col = "flow_in_day", add_inches_day = TRUE)
coeff_S_in  <- run_one_site_regression("01634000", regression_flow_col = "flow_in_day", add_inches_day = TRUE)

# Get drainage areas
area_CS <- get_drainage_area_sqmi("01632000")
area_MJ <- get_drainage_area_sqmi("01633000")
area_S  <- get_drainage_area_sqmi("01634000")

# m and b per gage
m_CS <- -0.0003047; b_CS <- 0.9418478
m_MJ <- -0.0088198; b_MJ <- 0.9202850
m_S  <- -0.0175015; b_S  <- 0.8911815

# Build data
gages <- c("01632000 (CS)", "01632000 (CS)",
           "01633000 (MJ)", "01633000 (MJ)",
           "01634000 (S)",  "01634000 (S)")
q_cfs <- c(25, 81, 106, 270, 200.25, 444)
areas <- c(area_CS, area_CS, area_MJ, area_MJ, area_S, area_S)
ms    <- c(m_CS, m_CS, m_MJ, m_MJ, m_S, m_S)
bs    <- c(b_CS, b_CS, b_MJ, b_MJ, b_S, b_S)

# Calculate columns
q_in  <- mapply(convert.flow, q_cfs, areas)
c_cfs <- ms * log(q_cfs) + bs
c_in  <- ms * log(q_in)  + bs
s_in  <- q_in / (1 - c_in)

# Build table
df <- data.frame(
  Gage  = gages,
  Q_cfs = q_cfs,
  Q_in  = round(q_in,  4),
  C_cfs = round(c_cfs, 4),
  C_in  = round(c_in,  4),
  S_in  = round(s_in,  4),
  C_lkp = NA
)

kable(df, format = "latex")