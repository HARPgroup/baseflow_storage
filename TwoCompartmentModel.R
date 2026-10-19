library(R6)
library(dplyr)
library(tidyr)
library(ggplot2)


##########################
##2. simulation settings##
##########################

numts <- 150

#compartment properties
agwsmax1 <- 5.0
agwsmax2 <- 1.0

c1 <- 0.95
c2 <- 0.98

tmethod <- "tmax"


#########################
##3. initial conditions##
#########################

#start the entire groundwater system empty
S1_init <- 0.0
S2_init <- 0.0


#################################
##4. warmup / recharge settings##
#################################

warmup_days <- 30
warmup_input <- 0.1   # inches/day

# External input to the upper compartment:
#
# AGWIN1(t) = 0.1 in/day for days 1-30
#             0.0 in/day after day 30

agwin1_series <- ifelse(
  1:numts <= warmup_days,
  warmup_input,
  0.0
)

#check total external water added during warmup
sum(agwin1_series)

###################################
##5. groundwater outflow function##
###################################

# calculate groundwater outflow from a compartment using
# the HSPF recession relationship.
#
# S    = compartment storage (inches)
# C    = recession coefficient
# dthr = timestep length (hours)

agwo_hspf <- function(S, C, dthr = 24) {
  
  kgwV <- 1.0 - C^(dthr / 24.0)
  
  agwo <- kgwV * S
  
  return(agwo)
}

#########################################
##6. two-compartment groundwater solver##
#########################################

solve_double_C <- function(
    agws1,
    agws2,
    c1,
    c2,
    agwsmax1,
    agwsmax2,
    agwin1 = 0.0,
    tmethod = "tmax",
    dthr = 24
) {
  
  ##############################################
  ##1. calculate outflow from starting storage##
  ##############################################
  
  agwo1 <- agwo_hspf(agws1, c1, dthr)
  agwo2 <- agwo_hspf(agws2, c2, dthr)
  
  
  #########################################################
  ##2. calculate transfer from upper to lower compartment##
  #########################################################
  
  #available space in lower compartment
  free2 <- agwsmax2 - agws2
  
  if (tmethod == "all") {
    
    #lower compartment can accept all available free space
    tmax2 <- free2
    
  } else {
    
    #limit transfer based on lower-compartment recession rate
    tmax2 <- agwsmax2 * (1.0 - c2)
  }
  
  #transfer cannot exceed available lower-compartment space
  if (tmax2 > free2) {
    tmax2 <- free2
  }
  
  #transfer also cannot exceed water leaving upper compartment
  agwin2 <- min(agwo1, tmax2)
  
  
  ##############################################
  ##3. determine groundwater outflow to stream##
  ##############################################
  
  #water transferred to compartment 2 is not streamflow
  agwo1 <- agwo1 - agwin2
  
  #total groundwater outflow to stream
  agwo <- agwo1 + agwo2
  
  
  ######################################
  ##4. effective recession coefficient##
  ######################################
  
  # Calculate Ce using the starting storage associated
  # with this timestep's outflow
  
  starting_storage <- agws1 + agws2
  
  # Ce is undefined when the system begins completely empty
  if (starting_storage > 0) {
    
    ce <- 1.0 - (agwo / starting_storage)
    
  } else {
    
    ce <- NA_real_
  }
  
  
  #####################
  ##5. update storage##
  #####################
  
  #upper compartment:
  #   starting storage
  #   - water leaving upper compartment
  #   + external input at END of timestep
  
  agws1 <- agws1 - (agwo1 + agwin2) + agwin1
  
  #lower compartment:
  #   starting storage
  #   + transfer from upper compartment
  #   - lower-compartment outflow
  
  agws2 <- agws2 + agwin2 - agwo2
  
  
  ####################
  ##6. total storage##
  ####################
  
  agws <- agws1 + agws2
  
  ##############################
  ##7. return timestep results##
  ##############################
  
  return(
    list(
      agwo = agwo,
      agws = agws,
      agws1 = agws1,
      agws2 = agws2,
      agwo1 = agwo1,
      agwo2 = agwo2,
      agwin1 = agwin1,
      agwin2 = agwin2,
      ce = ce
    )
  )
  
}
  
  #################################
  ##8. run groundwater simulation##
  #################################
  
  run_agw_model <- function(
    numts,
    S1_init,
    S2_init,
    c1,
    c2,
    agwsmax1,
    agwsmax2,
    agwin1_series,
    tmethod = "tmax",
    dthr = 24
  ) {
    
    #set initial storage
    agws1 <- S1_init
    agws2 <- S2_init
    
    #create empty dataframe for model results
    model_out <- data.frame()
    
    #run model one timestep at a time
    for (ts in 1:numts) {
      
      #save storage at beginning of timestep
      agws1_start <- agws1
      agws2_start <- agws2
      
      #run one timestep
      out <- solve_double_C(
        agws1 = agws1,
        agws2 = agws2,
        c1 = c1,
        c2 = c2,
        agwsmax1 = agwsmax1,
        agwsmax2 = agwsmax2,
        agwin1 = agwin1_series[ts],
        tmethod = tmethod,
        dthr = dthr
      )
      
      #save ending storage for use in next timestep
      agws1 <- out$agws1
      agws2 <- out$agws2
      
      #save timestep results
      model_out <- rbind(
        model_out,
        data.frame(
          timestep = ts,
          agws1_start = agws1_start,
          agws2_start = agws2_start,
          agwin1 = out$agwin1,
          agwin2 = out$agwin2,
          agwo1 = out$agwo1,
          agwo2 = out$agwo2,
          agwo = out$agwo,
          agws1 = out$agws1,
          agws2 = out$agws2,
          agws = out$agws,
          ce = out$ce
        )
      )
    }
    
    return(model_out)
  }
  
  ###########################
  ##9. run baseline warmup##
  ###########################
  
  baseline_run <- run_agw_model(
    numts = numts,
    S1_init = S1_init,
    S2_init = S2_init,
    c1 = c1,
    c2 = c2,
    agwsmax1 = agwsmax1,
    agwsmax2 = agwsmax2,
    agwin1_series = agwin1_series,
    tmethod = tmethod
  )
  
  ########################
  ##10. water balance check##
  ########################
  
  initial_storage <- S1_init + S2_init
  
  total_input <- sum(baseline_run$agwin1)
  
  total_outflow <- sum(baseline_run$agwo)
  
  final_storage <- tail(baseline_run$agws, 1)
  
  water_balance_error <-
    initial_storage +
    total_input -
    total_outflow -
    final_storage
  
  water_balance_error
  
  ################################
  ##11. plot storage through time##
  ################################
  
  storage_plot <- baseline_run %>%
    select(
      timestep,
      agws1,
      agws2,
      agws
    ) %>%
    pivot_longer(
      cols = c(agws1, agws2, agws),
      names_to = "storage_type",
      values_to = "storage"
    ) %>%
    mutate(
      storage_type = recode(
        storage_type,
        agws1 = "Upper compartment",
        agws2 = "Lower compartment",
        agws = "Total storage"
      )
    )
  
  ggplot(
    storage_plot,
    aes(
      x = timestep,
      y = storage,
      color = storage_type
    )
  ) +
    geom_line(linewidth = 1) +
    geom_vline(
      xintercept = warmup_days,
      linetype = "dashed"
    ) +
    labs(
      title = "Groundwater Storage During Model Warmup",
      subtitle = "0.1 in/day added to upper compartment for first 30 days",
      x = "Timestep (day)",
      y = "Storage (in)",
      color = "Storage"
    ) +
    theme_minimal()
  
  ###############################
  ##12. plot fluxes through time##
  ###############################
  
  flux_plot <- baseline_run %>%
    select(
      timestep,
      agwin1,
      agwin2,
      agwo
    ) %>%
    pivot_longer(
      cols = c(agwin1, agwin2, agwo),
      names_to = "flux_type",
      values_to = "flux"
    ) %>%
    mutate(
      flux_type = recode(
        flux_type,
        agwin1 = "External input to upper",
        agwin2 = "Upper-to-lower transfer",
        agwo = "Groundwater outflow"
      )
    )
  
  ggplot(
    flux_plot,
    aes(
      x = timestep,
      y = flux,
      color = flux_type
    )
  ) +
    geom_line(linewidth = 1) +
    geom_vline(
      xintercept = warmup_days,
      linetype = "dashed"
    ) +
    labs(
      title = "Water Fluxes During Model Warmup",
      subtitle = "External input stops after day 30",
      x = "Timestep (day)",
      y = "Water flux (in/day)",
      color = "Flux"
    ) +
    theme_minimal()
  
  ########################################
  ##13. plot groundwater outflow sources##
  ########################################
  
  outflow_plot <- baseline_run %>%
    select(
      timestep,
      agwo1,
      agwo2
    ) %>%
    pivot_longer(
      cols = c(agwo1, agwo2),
      names_to = "compartment",
      values_to = "outflow"
    ) %>%
    mutate(
      compartment = recode(
        compartment,
        agwo1 = "Upper compartment",
        agwo2 = "Lower compartment"
      )
    )
  
  ggplot(
    outflow_plot,
    aes(
      x = timestep,
      y = outflow,
      color = compartment
    )
  ) +
    geom_line(linewidth = 1) +
    geom_vline(
      xintercept = warmup_days,
      linetype = "dashed"
    ) +
    labs(
      title = "Groundwater Outflow by Compartment",
      subtitle = "Comparison of upper- and lower-compartment contributions",
      x = "Timestep (day)",
      y = "Groundwater outflow (in/day)",
      color = "Compartment"
    ) +
    theme_minimal()