library("agws")

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/R/AGWdouble.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/R/step_scq.R")
### Run the New Model
agwhilo = AGWdouble$new(
  agws1=5, agwsmax1 = 5, 
  agws2=1.0, agwsmax2 = 1.0,
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
