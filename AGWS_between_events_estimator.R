library(dplyr)
library(plotly)

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/convert.flow.R")


argst <- commandArgs(trailingOnly = T)
if (length(argst) < 4) {
  message("This script will take a series of trimmed baseflow events and a full time series flow data and estimate storage values calculated between baseflow events.")
  message("Use: model_outflow_calculator.R original_model_data_time_series_daily site_name site_no output_file ")
  q()
}


#example
csv1_path <- "https://deq1.bse.vt.edu:444/usgs/agws/baseflow_trimmed_stats_01632000.csv"
csv2_path <- "https://deq1.bse.vt.edu:444/usgs/agws/01632000-flow.csv"
m <- -0.0003047
b <- 0.9418478


# csv1_path <- "https://deq1.bse.vt.edu:444/usgs/agws/baseflow_trimmed_stats_01634000.csv"
# csv2_path <- "https://deq1.bse.vt.edu:444/usgs/agws/01634000-flow.csv"
# m <- -0.0145270564970664
# b <- 1.04829247996453

csv1_path <- argst[1]
csv2_path <- argst [2]
m <- argst[3]
b <- argst[4]
output_file <- argst[5]

#load in baseflow events
csv1 <- read.csv(csv1_path)


#load in full time series
csv2 <- read.csv(csv2_path) %>%
  rename(Date = obs_date, Flow = obs_flow) |> 
  filter(!is.na(Flow))

#make sure dates line up

csv1$Date <- as.Date(csv1$Date)

csv2$Date <- as.Date(csv2$Date)

#merge on date
df_full <- csv2 %>%
  left_join(csv1, by = "Date")

#clean up df

df_clean <- df_full %>%
  mutate(
    Flow = Flow.x,
    ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

#convert flows
df_clean <- df_clean %>% 
  mutate(
    flow_in = convert.flow(Flow, dra)
  )

#add m and b

m = m
b = b

df_clean <- df_clean %>%
  rename(da_sqmi = dra)

da_sqmi <- df_clean$da_sqmi[1]



#rebuilding lookup table:
Qcfs <- seq(min(df_clean$Flow, na.rm=TRUE),
            max(df_clean$Flow, na.rm=TRUE),
            by = 0.1)


site_factors <- function(da_sqmi, flow_vec = Qcfs, vec_for_reg = NULL, m, b) {
    
    Qin <- convert.flow(flow_vec, da_sqmi)
    
    if (is.null(vec_for_reg)) {
      vec_for_reg <- Qin
    }
    
    C <- b + (m * log(vec_for_reg))
    C <- pmin(pmax(C, 0.001), 0.999)
    
    return(data.frame(flow_vec, Qin, C))
  }


lookupdata <- site_factors(da_sqmi, flow_vec = Qcfs, vec_for_reg = Qcfs, m = m, b = b)

lookupdata$S <- lookupdata$Qin / (1 - lookupdata$C)


lookupdata$dS <- c(lookupdata$S[-1] / lookupdata$S[-length(lookupdata$S)], NA)



Svar <- lookupdata[lookupdata$dS > 1 & lookupdata$S > 0,]

#Estimating Storage
df_clean$AGWS_est <- approx(
  Svar$Qin,
  Svar$S,
  xout = df_clean$flow_in,
  rule = 2
)$y
            

df_clean <- df_clean %>%
  mutate(
    Flow_in = flow_in,
    AGWS = if_else(!is.na(AGWRC), Flow_in / (1 - AGWRC), NA_real_)
  )

df_clean$AGWS_final <- ifelse(
  !is.na(df_clean$AGWS),
  df_clean$AGWS,          
  df_clean$AGWS_est       
)


#Save as .csv files
write.csv(df_clean, file = output_file,
          row.names = FALSE
)

#checks and example plots
sum(is.na(df_clean$AGWS_final)) # should be 0


plot_ly(df_clean, x = ~Date) %>%
  add_lines(y = ~AGWS_final, name = "AGWS Best Estimate", line = list(color = "blue")) %>%
  add_markers(y = ~AGWS, name = "Observed Baseflow Storage", marker = list(color = "red")) %>%
  plotly::layout(
    xaxis = list(title = "Date"),
    yaxis = list(title = "Storage (AGWS)"),
    legend = list(x = 1, xanchor = "right", y = 1)
  )


