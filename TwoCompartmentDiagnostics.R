##double c model##

library("agws")

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/R/AGWdouble.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/R/step_scq.R")
### Run the New Model
agwhilo = AGWdouble$new(
  agws1=5, agwsmax1 = 5,
  agws2=2.0, agwsmax2 = 1.0,
  c1=0.95, c2=0.98, tmethod = "tmax"
)
# note in2cfs is only used when looking at Qcfs
# in2cfs = 1.0 / convert.flow(1, 508) # 508 = area sqmi
in2cfs = 1.0
testhilo = step_scq(numts=150, Sinit=0.5, method=agwhilo, in2cfs=in2cfs)
agwhilo$plot("cfq", warmup=5) # C = f(Q)
agwhilo$plot("cfs", warmup=5) # C = f(S)
agwhilo$plot("storage", warmup=5) # S = f(t)
agwhilo$plot("cft", warmup=5) # effective C = f(t)
quantile(agwhilo$log$agwo, na.rm=TRUE)
quantile(agwhilo$log$agwin2, na.rm=TRUE)
quantile(agwhilo$log$ce, na.rm=TRUE)
agwhilo$tmethod = "all" # uses 1.0 - c2 as max inflow to bottom layer

#### Note: this varying method of percolation into agws2 does not appear to function
# show difference between all and tmax methods of layer 2 inflow
# this equations DO show a diff when run manually but the logs and plots do not
# when run from the object
agwhilo$solve_double_C(2.5, 0.5, 0.95, 0.99, 5, 2.5, agwin1=0.0, tmethod="tmax", dthr=24)$agwin2
agwhilo$solve_double_C(2.5, 0.5, 0.95, 0.99, 5, 2.5, agwin1=0.0, tmethod="all", dthr=24)$agwin2


agw2$eval()
agw2$show_state()





##DIAGNOSTIC TESTING##
agwhilo = AGWdouble$new(
  agws1=5,
  agwsmax1=5,
  agws2=0.5,
  agwsmax2=1.0,
  c1=0.95,
  c2=0.98,
  tmethod="tmax"
)

testhilo = step_scq(
  numts=10,
  Sinit=0.5,
  method=agwhilo,
  in2cfs=1.0
)

agwhilo$log[, c(
  "timestamp",
  "agws1", "agws2", "agws",
  "agwin2",
  "agwo1", "agwo2", "agwo",
  "ce"
)]
#check water balance
agwhilo$log$agws -
  (agwhilo$log$agws1 + agwhilo$log$agws2)


##COMPARE "all" TO "tmax"##
# tmax
agw_tmax = AGWdouble$new(
  agws1=5,
  agwsmax1=5,
  agws2=0.5,
  agwsmax2=1.0,
  c1=0.95,
  c2=0.98,
  tmethod="tmax"
)

out_tmax = step_scq(
  numts=150,
  Sinit=0.5,
  method=agw_tmax,
  in2cfs=1.0
)

# all
agw_all = AGWdouble$new(
  agws1=5,
  agwsmax1=5,
  agws2=0.5,
  agwsmax2=1.0,
  c1=0.95,
  c2=0.98,
  tmethod="all"
)

out_all = step_scq(
  numts=150,
  Sinit=0.5,
  method=agw_all,
  in2cfs=1.0
)
#plots
plot(
  out_tmax$S,
  type="l",
  col="blue",
  xlab="Timestep",
  ylab="Total storage (in)"
)

lines(
  out_all$S,
  col="red"
)

legend(
  "topright",
  legend=c("tmax", "all"),
  col=c("blue", "red"),
  lty=1
)

plot(
  out_tmax$C,
  type="l",
  col="blue",
  xlab="Timestep",
  ylab="Effective C"
)

lines(
  out_all$C,
  col="red"
)



##TESTING DIFFERENT INTIAL STORAGES##
# ============================================================
# Test 2: Sensitivity to Initial Storage Distribution
library(ggplot2)
library(dplyr)
library(tidyr)

#model settings
# ------------------------------------------------------------
numts <- 150

#total initial storage remains constant for every scenario
total_initial_storage <- 5.5

#storage capacities
agwsmax1 <- 5.0
agwsmax2 <- 1.0

#recession coefficients
c1 <- 0.95
c2 <- 0.98

#transfer method
tmethod <- "tmax"

#conversion factor
in2cfs <- 1

#define initial storage scenarios
# ------------------------------------------------------------
initial_storage_scenarios <- list(
  "All storage in upper bin" = c(
    agws1 = 5.0,
    agws2 = 0.5
  ),
  "Mostly upper storage" = c(
    agws1 = 4.0,
    agws2 = 1.5
  ),
  "Evenly distributed storage" = c(
    agws1 = 2.75,
    agws2 = 2.75
  ),
  "Mostly lower storage" = c(
    agws1 = 1.5,
    agws2 = 4.0
  ),
  "All storage in lower bin" = c(
    agws1 = 0.5,
    agws2 = 5.0
  )
)

#run each scenario
# ------------------------------------------------------------
scenario_results_list <- lapply(
  names(initial_storage_scenarios),
  function(scenario_name) {
    
    initial_storage <- initial_storage_scenarios[[scenario_name]]
    
    #check that total initial storage is constant
    stopifnot(
      sum(initial_storage) == total_initial_storage
    )
    
    #create a new model for this scenario
    model <- AGWdouble$new(
      agws1 = initial_storage["agws1"],
      agws2 = initial_storage["agws2"],
      agwsmax1 = agwsmax1,
      agwsmax2 = agwsmax2,
      c1 = c1,
      c2 = c2,
      tmethod = tmethod
    )
    
    #run the model
    step_scq(
      numts = numts,
      Sinit = total_initial_storage,
      method = model,
      in2cfs = in2cfs
    )
    
    #extract the detailed two-bin model log
    results <- as.data.frame(model$log)
    
    #add scenario information
    results <- results %>%
      mutate(
        scenario = scenario_name,
        initial_agws1 = as.numeric(initial_storage["agws1"]),
        initial_agws2 = as.numeric(initial_storage["agws2"]),
        initial_agws = total_initial_storage
      )
    
    return(results)
  }
)

#combine the five data frames into one data frame
scenario_results <- bind_rows(scenario_results_list)

#check that all scenarios have the same initial total storage
# ------------------------------------------------------------
scenario_results %>%
  distinct(scenario, initial_agws1, initial_agws2, initial_agws)

#reshape storage results for plotting
# ------------------------------------------------------------
storage_results <- scenario_results %>%
  select(timestamp, scenario, agws1, agws2, agws) %>%
  pivot_longer(
    cols = c(agws1, agws2, agws),
    names_to = "storage_type",
    values_to = "storage"
  )

#PLOT 1: total storage through time
# ------------------------------------------------------------
ggplot(
  scenario_results,
  aes(
    x = timestamp,
    y = agws,
    color = scenario
  )
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Effect of Initial Storage Distribution on Total Storage",
    x = "Timestep",
    y = "Total groundwater storage",
    color = "Initial storage distribution"
  ) +
  theme_minimal()


#PLOT 2: upper and lower storage through time
# ------------------------------------------------------------
ggplot(
  storage_results %>%
    filter(storage_type %in% c("agws1", "agws2")),
  aes(
    x = timestamp,
    y = storage,
    color = scenario
  )
) +
  geom_line(linewidth = 1) +
  facet_wrap(
    ~ storage_type,
    scales = "free_y",
    labeller = as_labeller(c(
      agws1 = "Upper groundwater storage",
      agws2 = "Lower groundwater storage"
    ))
  ) +
  labs(
    title = "Effect of Initial Storage Distribution on Individual Bins",
    x = "Timestep",
    y = "Storage",
    color = "Initial storage distribution"
  ) +
  theme_minimal()


# # ------------------------------------------------------------
# # Plot 3: Outflow through time
# # ------------------------------------------------------------
# 
# ggplot(
#   scenario_results,
#   aes(
#     x = timestamp,
#     y = agwo,
#     color = scenario
#   )
# ) +
#   geom_line(linewidth = 1) +
#   labs(
#     title = "Effect of Initial Storage Distribution on Groundwater Outflow",
#     x = "Timestep",
#     y = "Groundwater outflow",
#     color = "Initial storage distribution"
#   ) +
#   theme_minimal()
# 
# 
# # ------------------------------------------------------------
# # Plot 4: Effective recession coefficient through time
# # ------------------------------------------------------------
# 
# ggplot(
#   scenario_results,
#   aes(
#     x = timestamp,
#     y = ce,
#     color = scenario
#   )
# ) +
#   geom_line(linewidth = 1) +
#   labs(
#     title = "Effect of Initial Storage Distribution on Effective C",
#     x = "Timestep",
#     y = "Effective recession coefficient",
#     color = "Initial storage distribution"
#   ) +
#   theme_minimal()

# ------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------

scenario_summary <- scenario_results %>%
  group_by(scenario) %>%
  summarise(
    initial_agws1 = first(initial_agws1),
    initial_agws2 = first(initial_agws2),
    initial_agws = first(initial_agws),
    initial_agwo = first(agwo),
    final_agws1 = last(agws1),
    final_agws2 = last(agws2),
    final_agws = last(agws),
    final_agwo = last(agwo),
    mean_agwo = mean(agwo),
    minimum_agwo = min(agwo),
    maximum_agwo = max(agwo),
    mean_ce = mean(ce),
    final_ce = last(ce),
    .groups = "drop"
  )

scenario_summary
