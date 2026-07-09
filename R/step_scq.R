#'@title solve_agwrc_lookup
#'@name
#'solve_agwrc_lookup
#'@description
#'Returns a C value from a table with S as lookup, stair-step
#'@details
#'For use to looup a C value based on S. Works best with a very fine grained table.
#'
#'@param S Input Storage
#'@param Ctable table of Storage and C for lookup
#'@return C from table with stair-step
#'@export
solve_agwrc_lookup <- function(S, Ctable){
  return(Ctable[Ctable$S >= S,][1,]$C)
}

#'@title step_scq
#'@name
#'step_scq
#'@description
#'Rund a model with multiple AGWS calcuation options.
#'@details
#'User can choose to run a lookup version, regression version or
#'two compartment with varying C and max S per compartment.
#'
#'@param numts number of timesteps to iterate
#'@param Sinit initial storage
#'@param method how to simulate, can be 'static', 'lookup', ''solver' or a 2 compartment object
#'@param in2cfs conversion factor inches to cfs
#'@param b intercept for C = mS + b equation
#'@param m intercept for C = mS + b equation
#'@param C static C value
#'@return dataframe with simulation values for each timestep from 1 to numts
#'@export
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
