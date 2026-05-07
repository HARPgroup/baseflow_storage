# Making ctable
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/convert.flow.R")


#Site <- "Mount Jackson"

# Make lookup table
# Arbitrary flow in cfs for lookup
Qts <- 1:600
Qin <- vector(mode="numeric", length = 600)

# Set site specific data and factors
site_factors <- function(da_sqmi, flow_vec = Qts, vec_for_reg = NULL, m, b){
  Qin <- convert.flow(flow_vec, da_sqmi)
  
  if (is.null(vec_for_reg)) {
    vec_for_reg <- Qin
  }
  
  C <- b + (m * log(vec_for_reg))
  
  assign("Qin", Qin, envir = .GlobalEnv)
  assign("C",   C,   envir = .GlobalEnv)
}

site_factors(get_drainage_area_sqmi("01634000"),Qts, Qin,m=	-0.0175015, b=0.8911815)

# Finish calcs for lookup table
S <- Qin / (1.0 - C)
Ccon = 0.96
Qconin <- S * (1.0 - Ccon)
Qcon <- S * (1.0 - Ccon) * revconvert 
Scon <- S * Ccon

# Create lookup table
Svar <- data.frame(
  Qts = Qts,
  C = C,
  Qin = Qin,
  S = S)

# Remove values where s is decreasing or negative
for(i in 1:length(Svar$S)) {
  Svar$dS[i] <- Svar$S[i+1]/Svar$S[i]
}

Svar <- sqldf::sqldf(
  " Select * from Svar where dS > 1 and S > 0"
)


# solve_agwrc_lookup
# Main loop
solve_agwrc_lookup <- function(S, Ctable){
  return(Ctable[Ctable$S >= S,][1,]$C)
}

solve_storage_lookup <- function(Qts, Ctable){
  return(Ctable[Ctable$Qts >= Qts,][1,]$S)
}


# Examples
solve_agwrc_lookup(0.3, Svar)
solve_storage_lookup(235, Svar)

Svar[Svar$Qts >= 331,][1,]$S

# Test with real MJ data
for(i in 1:nrow(all_df)){
  all_df$L_storage[i] <- solve_storage_lookup(all_df$Flow[i], Svar)
}

for(i in 1:nrow(all_df)){
  all_df$L_AGWRC[i] <- solve_agwrc_lookup(all_df$L_storage[i], Svar)
}

