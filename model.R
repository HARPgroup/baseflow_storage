# data needed:
#   - input agws with timestep info
#     - one agws input?
#   - AGWO data to match agws
#     - if only using one agws input, do you only need one agwo?
#   - Regression coefficients to describe rel between agwrc and q
#
# Variables:
#   - AGWS = input, f(agwo, agwrc)
#   - AGWO = from model? input data? f(q)?
#     - AGWO = (1-agwrc)*agws
#   - AGWRC = f(agws)
#   - Q = f(agws,agwrc)
#
#


event_data <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/bf_events_01633000.csv")

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/add_model_data.R")

# get regression function loaded
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/will_baseflow/RegressionFunctionTesting.R")


# Add storage data from model and convert flow to inches
event_df <- add_model_data(event_data,land_type_code = "forN51171", "AGWS", scenario = "subsheds2",site = "https://deq1.bse.vt.edu:443")
da_sqmi <- get_drainage_area_sqmi("01633000")
event_df$Flow_in <- convert.flow(event_df$Flow, da_sqmi)

# Pick example event
ex <- sqldf("select * from event_df where GroupID=55")
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

reg_df <- make_event_regression_df(event_df, da_sqmi, flow_col = "Flow_in")
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

# Cgecking that res is changing
r_check <- data.frame(
  i = numeric(length = length(ex$Date)),
  r = character(length = length(ex$Date))
)

# set up repeat regression
for (i in 1:length(ex$Date)) {
  # Set n-1 storage
  if(i==1){
    ex$V_storage <- S0
    
    r_check$i[i] <- i
    r_check$r[i] <- NA
    
  }else{
    Stg <- ex$V_storage[i-1]
    
    # Run uniroot
    res <- solve_agwrc_log(S=Stg, m, b)
    
    r_check$i[i] <- i
    r_check$r[i] <- res
    
    # use calcd agwrc to get storage
    ex$V_storage[i] <- res * Stg
  }
}


ggplot(ex, mapping = aes(Date))+
  geom_line(mapping = aes(y=AGWS, color="Model Data"))+
  geom_line(mapping = aes(y=U_storage, color="Constant AGWRC"))+
  geom_line(mapping = aes(y=V_storage, color="Variable AGWRC"))+
  scale_color_manual(
    name = "Legend", 
    values = c("Model Data" = "grey25", 
               "Constant AGWRC" = "dodgerblue2", 
               "Variable AGWRC" = "firebrick2"))+
  coord_cartesian(ylim = c(0,S0+0.01))+
  theme_bw()+
  ggtitle(paste0("Storage Calculated using different AGWRCs, MJ Event ", ex$GroupID[1]))+
  theme(plot.title = element_text(hjust = 0.5))

