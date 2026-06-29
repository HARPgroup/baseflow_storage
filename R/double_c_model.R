Qts <- 0:600
C <- 1.004269 + -0.008820 * log(Qts)
Qin <- convert.flow(Qts, 508)
S <- Qin / (1.0 - C);#A S(t-1) = S(t) + Q(t)
revconvert = 1.0 / convert.flow(1, 508) # bring from in/day back to cfs
agwrc_loglin <- function(x, m, b) { y = m * log(x) + b; return(y) }
agwo_hspf <- function(S, C, dthr) {
  kgwV = 1.0 - C^(dthr/24.0)
  agwo = kgwV * S
  return(agwo)
}

# Assemble a dataframe of calculated flows. and back calculate S
Svar <- data.frame(
  Qts = Qts,
  C = C,
  Qin = Qin,
  S = S
)
dS <- Svar[2:nrow(Svar),]$S/Svar[1:(nrow(Svar)-1),]$S
Svar <- Svar[dS > 1,]
Svar <- Svar[Svar$S > 0,]


# Main loop
solve_agwrc_lookup <- function(S, Ctable){
  return(Ctable[Ctable$S >= S,][1,]$C)
}

solve_agwrc_log <- function(S, m, b) {

  g <- function(C) {
    message(paste(m, C, b, S))
    C - (m * log(S * (1 - C)) + b)
  }

  res <- tryCatch(
    stats::uniroot(g, lower = 0.001, upper = 0.999)$root,
    error = function(e) NA_real_
  )

  return(res)
}


AGWdouble <- R6::R6Class(
  public = list(
    agws1 = 0.0,
    agws2 = 0.0,
    agwo1 = 0.0,
    agwo2 = 0.0,
    agwin1 = 0.0,
    agwin2 = 0.0,
    agwo = 0.0,
    c1 = 0.99,
    c2 = 0.99,
    ce = 0.99,
    agwsmax1 = 1.0,
    agwsmax2 = 1.0,
    dthr = 24.0,
    initialize = function(agws1=0.0, agws2=0.0, c1=0.99, c2=0.99, agwsmax1=1.0, agwsmax2=1.0) {
      self$agws1 = agws1
      self$agws2=agws2
      self$c1=c1
      self$c2=c2
      self$agwsmax1=agwsmax1
      self$agwsmax2=agwsmax2
    },
    solve_double_C = function(agws1, agws2, c1, c2, agwsmax1, agwsmax2, agwin1=0.0, dthr=24) {
      # calculate agwo1 = flow out of agws1 (top layer of GW)
      # calculate agwin2 = amount of agwo1 that goes into agws2 from agws1
      # - this cannot exceed storage available in agws2
      # - should not exceed transmissivity rate of agws2
      # - *but* we will assume that the agws1 will take all it can from agwo1
      # calculate agwo2 = amount of flow out of agwo2
      # we will ignore agwsmax1 for now since HSPF does so
      agwo1 = agwo_hspf(agws1, c1, dthr)
      agwin2 = 0.0
      if (agwo1 > (agwsmax2 - agws2)) {
        agwin2 = (agwsmax2 - agws2)
      } else {
        agwin2 = agwo1
      }
      agwo1 = agwo1 - agwin2 # remainder goes out of top GW layer
      agws2 = agws2 + agwin2
      agwo2 = agwo_hspf(agws2, c2, dthr)
      agwo = agwo1 + agwo2
      # finish by updating storage at end of timestep
      agws2 = agws2 - agwo2
      agws1 = agws1 - agwo1 - agwin2 # remove total from agws1
      # calculate an effective C to compare to other methods
      # TODO: need to accomodate varying timesteps a la 1.0 - C^(dthr/24.0)
      ce = 1.0 - (agwo / (agws1 + agws2))
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
        agwsmax2=self$agwsmax2, agwin1=self$agwin1, dthr=self$dthr
      )
      self$agwo = outlist$agwo
      self$agws1 = outlist$agws1
      self$agws2 = outlist$agws2
      self$agwo2 = outlist$agwo2
      self$agwo1 = outlist$agwo1
      self$agwin1 = outlist$agwin1
      self$agwin2 = outlist$agwin2
      self$ce = outlist$ce
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
      Cs = method$ce
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

step_scq(numts=100, Sinit=0.5, method="lookup", in2cfs=revconvert, C=Svar)
step_scq(numts=10, Sinit=0.5, method="constant", in2cfs=revconvert, C=0.960)
step_scq(numts=10, Sinit=0.5, method="solver",b=1.004269, m=-0.008820, in2cfs=revconvert, C=0.960)

