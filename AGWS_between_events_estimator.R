library(dplyr)

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/convert.flow.R")


argst <- commandArgs(trailingOnly = T)
if (length(argst) < 4) {
  message("This script will take a series of trimmed baseflow events and a full time series flow data and estimate storage values calculated between baseflow events.")
  message("Use: model_outflow_calculator.R original_model_data_time_series_daily site_name site_no output_file ")
  q()
}
csv1_path <- argst[1]
csv2_path <- argst [2]
m <- argst[3]
b <- argst[4]
output_file <- argst[5]

#load in baseflow events
csv1 <- read_csv(csv1_path)


#load in full time series
csv2 <- read_csv(csv2_path) %>%
  rename(Date = obs_date, Flow = obs_flow)

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

# m = -0.0157647428890143	
# b = 1.05289351038922

df_clean <- df_clean %>%
  rename(da_sqmi = dra)

da_sqmi <- df_clean$da_sqmi[1]



#rebuilding lookup table:


Qts <- seq(min(df_clean$flow_in, na.rm=TRUE),
           max(df_clean$flow_in, na.rm=TRUE),
           by = 0.001)


site_factors <- function(da_sqmi, flow_vec = Qts, vec_for_reg = NULL, m, b){
  Qin <- convert.flow(flow_vec, da_sqmi)
  
  if (is.null(vec_for_reg)) {
    vec_for_reg <- Qin
  }
  
  C <- b + (m * log(vec_for_reg))
  
    C <- pmin(pmax(C, 0.001), 0.999)
  
  assign("Qin", Qin, envir = .GlobalEnv)
  assign("C",   C,   envir = .GlobalEnv)
}


site_factors(da_sqmi, flow_vec = Qts, m = m, b = b)

S <- Qin / (1 - C)

Svar <- data.frame(
  Qts = Qts,
  C = C,
  Qin = Qin,
  S = S
)


Svar$dS <- c(Svar$S[-1] / Svar$S[-length(Svar$S)], NA)

Svar <- subset(Svar, dS > 1 & S > 0) 
            
#Estimating Storage

df_clean$AGWS_est <- approx(
  Svar$Qts,
  Svar$S,
  xout = df_clean$flow_in,
  rule = 2
)$y

#Fill missing Storage

df_clean$AGWS_final <- ifelse(
  is.na(df_clean$AGWS),
  df_clean$AGWS_est,
  df_clean$AGWS
)

#Save as .csv files
write.csv(df_clean, file = output_file,
          row.names = FALSE
)

#checks and example plots
# sum(is.na(df_clean$AGWS_final))
# 
# 
# plot(df_clean$Date, df_clean$AGWS_final,
#      type = "l", col = "blue",
#      xlab = "Date", ylab = "Storage (AGWS)")
# 
# points(df_clean$Date, df_clean$AGWS, col = "red")
# 
# legend("topright",
#        legend = c("AGWS Best Estimate", "Observed Baseflow Storage"),
#        col = c("blue", "red"),
#        lty = c(1, NA),
#        pch = c(NA, 1))



