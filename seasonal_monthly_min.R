# Load required libraries
library(zoo)
library(hydrotools)
library(dplyr)
library(tidyr)
library(ggplot2)

#set up command args
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3) {
  stop("Usage: Rscript monthly_min_boxplot.R input.csv 'Plot Title' output.png [timezone]")
}

input_csv  <- args[1]
plot_title <- args[2]
output_file <- args[3]

# Read CSV
csv1 <- input_csv
csv1 <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ben_bf_csvs/strasburg_usgs_flow.csv")

# Ensure Flow is numeric
csv1$Flow <- as.numeric(csv1$Flow)

# Convert to zoo time series
fz <- csv1
fz$timestamp <- as.POSIXct(fz$Date, tz = "EST")
fz <- zoo::zoo(as.numeric(fz$Flow), order.by = fz$timestamp)

# Apply group1 (Monthly Min by Year)
csv1_group_1 <- hydrotools::group1(fz, yearType = "calendar", FUN = min)

# Convert to dataframe
csv1_group_1 <- as.data.frame(csv1_group_1)

# Move rownames (years) into a column
csv1_group_1$Year <- as.numeric(rownames(csv1_group_1))
rownames(csv1_group_1) <- NULL

#  Pivot to long format
csv1_group_1 <- csv1_group_1 %>%
  pivot_longer(
    cols = -Year,
    names_to = "Month",
    values_to = "MinFlow"
  )

# Make Month an ordered factor
csv1_group_1$Month <- factor(
  csv1_group_1$Month,
  levels = c("January","February","March","April","May","June",
             "July","August","September","October","November","December"),
  ordered = TRUE
)

# Create Boxplot
p <- ggplot(csv1_group_1, aes(x = Month, y = MinFlow)) +
  geom_boxplot() +
  labs(
    title = "plot_title",
    x = "Month",
    y = "Monthly Minimum Flow (CFS)"
  ) +
  theme_minimal()

p

#output plot
ggsave(output_file, p)