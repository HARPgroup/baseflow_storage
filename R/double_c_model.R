# set up the lookup table using values from
# the test_Svar.R example, grabbing select rows
# like so: Svar[c(1,25,50,100,150,200,250),]
Sprop = RomProperty$new(ds)
Sprop$set_matrix(
  data.frame(
    c("Qts", "C", "Qin", "S"),
    c(4,0.9920419, 0.0002928353, 0.03679706),
    c(28, 0.9748790, 0.0020498471, 0.08159880),
    c(53, 0.9692510, 0.0038800677, 0.12618527),
    c(103, 0.9633907, 0.0075405089, 0.20597244),
    c(153, 0.9599005, 0.0112009501, 0.27932918),
    c(203, 0.9574065, 0.0148613913, 0.34891238),
    c(253, 0.9554645, 0.0185218325, 0.41588922)
  )
)
Svar = Sprop$data_matrix

AGWdouble <- R6::R6Class(
  public = list(
    tmethod="all",
    agws1 = 0.0,
    agws2 = 0.0,
    agwo1 = 0.0,
    agwo2 = 0.0,
    agwin1 = 0.0,
    agwin2 = 0.0,
    agwo = 0.0,
    agws = 0.0,
    c1 = 0.99,
    c2 = 0.99,
    ce = 0.99,
    agwsmax1 = 1.0,
    agwsmax2 = 1.0,
    log = NA,
    dthr = 24.0,
    initialize = function(
        agws1=0.0, agws2=0.0, c1=0.99, c2=0.99, 
        agwsmax1=1.0, agwsmax2=1.0, tmethod="all"
      ) {
      self$agws1 = agws1
      self$agws2=agws2
      self$c1=c1
      self$c2=c2
      self$agwsmax1=agwsmax1
      self$agwsmax2=agwsmax2
      self$agws = self$agws1 + self$agws2
      log = data.frame(
        timestamp = integer(),
        agws1 = numeric(),
        agws2 = numeric(),
        agws = numeric(),
        agwin1 = numeric(),
        agwin2 = numeric(),
        agwo1 = numeric(),
        agwo2 = numeric(),
        agwo = numeric(),
        c1 = numeric(),
        c2 = numeric(),
        ce = numeric()
      )
    },
    solve_double_C = function(agws1, agws2, c1, c2, agwsmax1, agwsmax2, agwin1=0.0, tmethod="all", dthr=24) {
      # calculate agwo1 = flow out of agws1 (top layer of GW)
      # calculate agwin2 = amount of agwo1 that goes into agws2 from agws1
      # - this cannot exceed storage available in agws2
      # - should not exceed transmissivity rate of agws2
      # - *but* we will assume that the agws1 will take all it can from agwo1
      # calculate agwo2 = amount of flow out of agwo2
      # we will ignore agwsmax1 for now since HSPF does so
      agwo1 = agwo_hspf(agws1, c1, dthr)
      # then calculate outflow from bottom layer
      agwo2 = agwo_hspf(agws2, c2, dthr)
      # now computer transfers from top layer to bottom
      agwin2 = 0.0
      # how much free space in agws2?
      free2 = (agwsmax2 - agws2) 
      # this method assumes lower layer can take all that upper gives
      if (tmethod == "all") {
        tmax2 = free2
      } else {
        # compute on head/transmissivity?
        tmax2 = agwsmax2 * (1.0 - c2)
      }
      # cannot allow more in than available storage
      if (tmax2 > free2) {
        tmax2 = free2
      }
      if (agwo1 > tmax2) {
        agwin2 = tmax2
      } else {
        agwin2 = agwo1
      }
      # since we consider agwo as that which goes from GW to the stream
      # we subtract the transfer to the bottom GW layer
      agwo1 = agwo1 - agwin2 
      # record total outflow from GW to stream
      agwo = agwo1 + agwo2
      # calculate an effective C to compare to other methods
      # TODO: need to accomodate varying timesteps a la 1.0 - C^(dthr/24.0)
      ce = 1.0 - (agwo / (agws1 + agws2))
      # finish by updating storage at end of timestep
      agws1 = agws1 - (agwo1 + agwin2) # remove total from agws1
      agws2 = agws2 + agwin2 - agwo2
      return(
        list(
          agwo=agwo, agws1=agws1, agws2=agws2, agwo2=agwo2, 
          agwo1=agwo1, agwin1=agwin1, agwin2=agwin2, ce=ce
        )
      )
    },
    eval = function() {
      outlist = self$solve_double_C(
        agws1=self$agws1, agws2=self$agws2, c1=self$c1, c2=self$c2, agwsmax1=self$agwsmax1, 
        agwsmax2=self$agwsmax2, agwin1=self$agwin1, dthr=self$dthr, tmethod=self$tmethod
      )
      self$agws1 = outlist$agws1
      self$agws2 = outlist$agws2
      self$agws = self$agws1 + self$agws2
      self$agwo1 = outlist$agwo1
      self$agwo2 = outlist$agwo2
      self$agwo = outlist$agwo
      self$agwin1 = outlist$agwin1
      self$agwin2 = outlist$agwin2
      self$ce = outlist$ce
    },
    show_state = function(format="markdown") {
      output = as.data.frame(
        list(
          AGWS1 = self$agws1,
          AGWS2 = self$agws2,
          AGWIN1 = self$agwin1,
          AGWO1 = self$agwo1,
          AGWIN2 = self$agwin2,
          AGWO2 = self$agwo2,
          AGWO = self$agwo
        )
      )
      if (format != "dataframe") {
        output = kableExtra::kable(
          output, 
          format=format
        )
      }
      return(output)
    },
    log_state = function(ts) {
      self$log = rbind(
        self$log,
        data.frame(
          timestamp = ts,
          agws1 = self$agws1,
          agws2 = self$agws2,
          agws = self$agws,
          agwin1 = self$agwin1,
          agwin2 = self$agwin2,
          agwo1 = self$agwo1,
          agwo2 = self$agwo2,
          agwo = self$agwo,
          c1 = self$c1,
          c2 = self$c2,
          ce = self$ce
        )
      )
    },
    plot = function(type="cfq", warmup=0, ylim=TRUE) {
      wts = self$log[(warmup + 1):nrow(self$log),]
      if (type == "cfq") {
        if (is.logical(ylim)) {
          ylim=c(min(0.9, min(wts$ce)), 1.0)
        }
        plot(
          (wts$agwo1 + wts$agwo2),
          wts$ce,
          xlab='Total Outflow (in/day)',
          ylab='Effective Recession Coefficient(AGWRC)',
          ylim=ylim
        )
      }
      if (type == "cfs") {
        if (is.logical(ylim)) {
          ylim=c(min(0.9, min(wts$ce)), 1.0)
        }
        plot(
          (wts$agws1 + wts$agws2),
          wts$ce,
          xlab='Total Storage (in)',
          ylab='Effective Recession Coefficient(AGWRC)',
          ylim=ylim
        )
      }
      if (type == "qfs") {
        if (is.logical(ylim)) {
          ylim=c(0, max(wts$agwo))
        }
        plot(
          wts$agws,
          wts$agwo,
          xlab='Total Storage (in)',
          ylab='Outflow (in/day)',
          ylim=ylim
        )
      }
      if (type == "storage") {
        plot(
          (wts$agws),
          ylab='Total Storage (in)',
          xlab='Timestep',col="black",
          ylim=c(0,max((wts$agws1 + wts$agws2), na.rm = TRUE))
        )
        points(
          (wts$agws1),col="blue"
        )
        points(
          (wts$agws2),col="green"
        )
        points(
          (wts$agwo),col="orange"
        )
        legend(
          x="right",y="top",
          fill=c("black", "blue", "green","orange"),
          legend=c("Total (in)", "Upper Layer", "Lower Layer", "Flow")
        )
      }
    }
  )
)


step_scq <- function (numts, Sinit, method, in2cfs, b=0.0, m=1.0, C=0.99) {
  # note if method = "lookup", C should be a dataframe with columns Q and C
  S = Sinit
  model_out <- data.frame(
    S = numeric(),
    Q = numeric(),
    Qinch = numeric(),
    C = numeric()
  )
  for (ts in 1:numts) {
    if (typeof(method) == "environment") {
      method$eval()
      method$log_state(ts)
      Cs = method$ce
      Qinch = method$agwo
      Q = in2cfs * Qinch
      S = method$agws1 + method$agws2
    } else {
      if (method == 'solver') {
        # use solver
        Cs = solve_agwrc_log(S=S, m, b)
      } else if (method == 'lookup') {
        Cs = solve_agwrc_lookup(S, C)
      } else {
        Cs = C
      }
      Qinch = S * (1.0 - Cs)
      Q = in2cfs * Qinch
      S = S - Qinch
    }
    model_out <- rbind(model_out, data.frame(S=S, Q=Q, Qinch=Qinch, C=Cs))
  }
  return(model_out)
}
# simple identical containers
agw2 = AGWdouble$new(
  agws1=1.0,agws2=1.0, 
  agwsmax1 = 1.5, agwsmax2 = 1.5,
  c1=0.99, c2=0.99
)
agwz = AGWdouble$new(
  agws1=2.0, agwsmax1 = 2.5, 
  agws2=2.5, agwsmax2 = 2.5,
  c1=0.98, c2=0.99
)


test1 = step_scq(numts=100, Sinit=0.5, method="lookup", in2cfs=revconvert, C=Svar)
test2 = step_scq(numts=3, Sinit=0.5, method=agw2, in2cfs=revconvert)
testz = step_scq(numts=100, Sinit=0.5, method=agwz, in2cfs=revconvert)


agwhilo = AGWdouble$new(
  agws1=2.5, agwsmax1 = 2.5, 
  agws2=1, agwsmax2 = 1,
  c1=0.99, c2=0.95
)
testhilo = step_scq(numts=100, Sinit=0.5, method=agwhilo, in2cfs=revconvert)
agwhilo$plot("cfq", warmup=5)
agwhilo$plot("storage", warmup=5)

agwhilo = AGWdouble$new(
  agws1=2.5, agwsmax1 = 2.5, 
  agws2=1, agwsmax2 = 1,
  c1=0.95, c2=0.99
)
testhilo = step_scq(numts=300, Sinit=0.5, method=agwhilo, in2cfs=revconvert)
agwhilo$plot("cfq", warmup=5)
agwhilo$plot("storage", warmup=5)


agwhilo = AGWdouble$new(
  agws1=5, agwsmax1 = 5, 
  agws2=5, agwsmax2 = 5,
  c1=0.99, c2=0.95
)
agwhilo$tmethod = "transmissivity"
testhilo = step_scq(numts=300, Sinit=0.5, method=agwhilo, in2cfs=revconvert)
agwhilo$plot("cfq", warmup=5)
agwhilo$plot("storage", warmup=5)

agwhilo = AGWdouble$new(
  agws1=5, agwsmax1 = 5, 
  agws2=5, agwsmax2 = 5,
  c1=0.95, c2=0.99
)
agwhilo$tmethod = "transmissivity"
testhilo = step_scq(numts=300, Sinit=0.5, method=agwhilo, in2cfs=revconvert)
agwhilo$plot("cfq", warmup=5)
agwhilo$plot("storage", warmup=5)

agwhilo = AGWdouble$new(
  agws1=5, agwsmax1 = 5, 
  agws2=1.0, agwsmax2 = 1.0,
  c1=0.95, c2=0.99
)
agwhilo$tmethod = "all" # uses 1.0 - c2 as max inflow to bottom layer
testhilo = step_scq(numts=300, Sinit=0.5, method=agwhilo, in2cfs=revconvert)
agwhilo$plot("cfq", warmup=5)
agwhilo$plot("storage", warmup=5)
quantile(agwhilo$log$agwo, na.rm=TRUE)
quantile(agwhilo$log$agwin2, na.rm=TRUE)
quantile(agwhilo$log$ce, na.rm=TRUE)
# show difference between all and tmax methods of layer 2 inflow
agwhilo$solve_double_C(2.5, 0.5, 0.95, 0.99, 5, 2.5, agwin1=0.0, tmethod="tmax", dthr=24)$agwin2
agwhilo$solve_double_C(2.5, 0.5, 0.95, 0.99, 5, 2.5, agwin1=0.0, tmethod="all", dthr=24)$agwin2


agw2$eval()
agw2$show_state()
