# Load required libraries
library(zoo)
library(hydrotools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

#set up command args
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 4) {
  stop("Usage: Rscript monthly_min_boxplot.R input.csv 'Plot Title' output.png [timezone]")
}

input_observed_csv  <- args[1]
inputer_model_csv <- args[2]
plot_title <- args[3]
output_file <- args[4]

process_monthly_min <- function(input_csv, source_name, tz = "EST") {
  
  # Read CSV
  df <- read.csv(input_csv)
  
  # Ensure numeric
  df$Flow <- as.numeric(df$Flow)
  
  # Convert to zoo
  df$timestamp <- as.POSIXct(df$Date, tz = tz)
  fz <- zoo::zoo(df$Flow, order.by = df$timestamp)
  
  # Monthly minimum by year
  g1 <- hydrotools::group1(fz, yearType = "calendar", FUN = min)
  
  # Convert to dataframe
  g1 <- as.data.frame(g1)
  g1$Year <- as.numeric(rownames(g1))
  rownames(g1) <- NULL
  
  # Pivot longer
  g1 <- g1 %>%
    pivot_longer(
      cols = -Year,
      names_to = "Month",
      values_to = "MinFlow"
    )
  
  # Ordered month factor
  g1$Month <- factor(
    g1$Month,
    levels = c("January","February","March","April","May","June",
               "July","August","September","October","November","December"),
    ordered = TRUE
  )
  
  # Add source label
  g1$Source <- source_name
  
  return(g1)
}

input_observed_csv <- "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ben_bf_csvs/strasburg_usgs_flow.csv"
input_model_csv <- "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ben_bf_csvs/Strasburg_model_flow_daily.csv"
plot_title <- "Strasburg Minimum Monthly Flows"

observed_data <- process_monthly_min(input_observed_csv, "Observed")
modeled_data  <- process_monthly_min(input_model_csv, "Modeled")


monthly_data <- inner_join(observed_data, modeled_data,
                           by = c("Year", "Month")) %>%
  filter(!is.na(MinFlow.y))

monthly_data <- monthly_data %>%
  pivot_longer(
    cols = c(MinFlow.x, MinFlow.y),
    names_to = "Source",
    values_to = "MinFlow"
  ) %>%
  mutate(Source = recode(Source,
                         "MinFlow.x" = "Observed",
                         "MinFlow.y" = "Modeled"))

mins <- monthly_data %>%
  group_by(Month, Source) %>%
  summarise(monthly_min = min(MinFlow, na.rm = TRUE), .groups = "drop")


p <- ggplot(monthly_data, aes(x = Month, y = MinFlow, fill = Source)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.65) +
  
  # Minimum point
  geom_point(data = mins,
             aes(x = Month, y = monthly_min),
             position = position_dodge(width = 0.8),
             size = 3,
             shape = 21,
             color = "black") +
  
  # label 
  geom_text(data = mins,
            aes(x = Month,
                y = monthly_min,
                label = round(monthly_min, 2)),
            position = position_dodge(width = 0.8),
            vjust = -0.8,
            size = 3) +
  
  theme_minimal() +
  labs(
    y = "Monthly Minimum Flow (cfs)",
    x = "Month",
    title = plot_title,
    fill = "Data Source"
  )

p

p2 <- ggplot(mins, aes(x = Month, y = monthly_min, fill = Source))+
  geom_col( position = "dodge") + 
  theme_minimal()+
  labs(
   y= "Minimum Flow CFS",
   x= "Month",
    title = plot_title
  )

p2

# save plot
ggsave(filename = output_file,
       plot = p,
       dpi = 300)
