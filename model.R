# Load in data and functions
event_data <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/bf_events_01633000.csv")

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/add_model_data.R")

# get regression function loaded
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/will_baseflow/RegressionFunctionTesting.R")


# Add storage data from model and convert flow to inches
event_df <- add_model_data(event_data,land_type_code = "forN51171", "AGWS", scenario = "subsheds2",site = "http://deq1.bse.vt.edu:81")
da_sqmi <- get_drainage_area_sqmi("01633000")
event_df$Flow_in <- convert.flow(event_df$Flow, da_sqmi)

# optional For loop to do every event ----
group_ids <- unique(event_df$GroupID)
all_events <- list()

for (gid in unique(event_df$GroupID)) {
  
  ex <- sqldf(sprintf("select * from event_df where GroupID = %d", gid))
  ex$Date <- as.Date(ex$Date)
  
  
  # Get initial storage value from selected event
  S0 <- ex$AGWS[1]
  
  # Use AGWRC found through trimming for unchanged AGWRC calculations
  U_AGWRC <- ex$AGWRC[1]
  
  # Calc projection with unchanged AGWRC
  for(i in 1:length(ex$Date)) {
    
    if(i==1){
      ex$U_storage[i] <- S0
    }else{
      stor <- as.numeric(ex$U_storage[i-1])
      nxds <- U_AGWRC*stor
      
      ex$U_storage[i] <- nxds
    }
  }
  
  
  # Using regression and uniroot ----
  
  # get m and b for MJ
  reg_df <- make_event_regression_df(event_df, da_sqmi, flow_col = "Flow")
  reg_out <- fit_agwrc_regression_in_day(reg_df)
  
  # Set m and b from regression
  b <- reg_out$coefficients[1]
  m <- reg_out$coefficients[2]
  
  # solve function
  solve_agwrc_log <- function(S, m, b) {
    
    g <- function(C) {
      C - (m * log(S * (1 - C)) + b)
    }
    
    res <- tryCatch(
      uniroot(g, lower = 0.001, upper = 0.999)$root,
      error = function(e) NA_real_
    )
    
    return(res)
  }
  
  ex$V_storage <- NA
  ex$V_AGWRC <- NA
  ex$V_flow <- NA
  
  # set up repeat regression
  for (i in 1:length(ex$Date)) {
    # Set n-1 storage
    if(i==1){
      ex$V_storage <- S0
      ex$V_flow <- ex$Flow_in[i]
      
    }else{
      Stg <- ex$V_storage[i-1]
      Flw <- ex$Flow_in[i-1]
      
      # Run uniroot
      res <- solve_agwrc_log(S=Stg, m, b)
      
      # use calcd agwrc to get storage
      ex$V_storage[i] <- res * Stg
      ex$V_flow[i] <- res * Flw
      ex$V_AGWRC[i] <- res
      
    }
  }
  
  all_events[[gid]] <- ex
  
}

all_df <- do.call(rbind, all_events)
all_df$Date <- as.Date(all_df$Date)
# Clip to model data available
all_df <- subset(all_df, Date >= as.Date("1983-12-31"))

# Using Sample Event----
ex <- sqldf("select * from event_df where GroupID=96")
ex$Date <- as.Date(ex$Date)


# Get initial storage value from selected event
S0 <- ex$AGWS[1]

# Use AGWRC found through trimming for unchanged AGWRC calculations
U_AGWRC <- ex$AGWRC[1]

# Calc projection with unchanged AGWRC
for(i in 1:length(ex$Date)) {
  
  if(i==1){
    ex$U_storage[i] <- S0
  }else{
    stor <- as.numeric(ex$U_storage[i-1])
    nxds <- U_AGWRC*stor
  
    ex$U_storage[i] <- nxds
    }
}

# get m and b for MJ

reg_df <- make_event_regression_df(event_df, da_sqmi, flow_col = "Flow")
reg_out <- fit_agwrc_regression_in_day(reg_df)

# Set m and b from regression
b <- reg_out$coefficients[1]
m <- reg_out$coefficients[2]

# solve function
solve_agwrc_log <- function(S, m, b) {
  
  g <- function(C) {
    C - (m * log(S * (1 - C)) + b)
  }
  
  res <- tryCatch(
    uniroot(g, lower = 0.001, upper = 0.999)$root,
    error = function(e) NA_real_
  )
  
  return(res)
}

ex$V_storage <- NA
ex$V_AGWRC <- NA
ex$V_flow <- NA

# set up repeat regression
for (i in 1:length(ex$Date)) {
  # Set n-1 storage
  if(i==1){
    ex$V_storage <- S0
    ex$V_flow <- ex$Flow_in[i]
    
  }else{
    Stg <- ex$V_storage[i-1]
    Flw <- ex$Flow_in[i-1]
    
    # Run uniroot
    res <- solve_agwrc_log(S=Stg, m, b)
    
    # use calcd agwrc to get storage
    ex$V_storage[i] <- res * Stg
    ex$V_flow[i] <- res * Flw
    ex$V_AGWRC[i] <- res
    
  }
}


# Plugging in arbitrary S values----
solve_agwrc_log <- function(S, m, b) {
  
  g <- function(C) {
    C - (m * log(S * (1 - C)) + b)
  }
  
  res <- tryCatch(
    uniroot(g, lower = 0.001, upper = 0.999)$root,
    error = function(e) NA_real_
  )
  
  return(res)
}

test_df <- data.frame(Storage = seq(0, 10, by = 0.01), AGWRC = NA)


# set up repeat regression
for (i in 1:length(test_df$Storage)) {
    # Run uniroot
    res <- solve_agwrc_log(S=test_df$Storage[i], m, b)
    
    test_df$AGWRC[i] <- res
    
}
