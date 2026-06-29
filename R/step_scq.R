

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
