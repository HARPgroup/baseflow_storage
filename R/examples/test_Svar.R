Qts <- 0:600
b = 1.004269
m = -0.00820
C <- b + m * log(Qts)
Qin <- convert.flow(Qts, 508)
dQ <- (b + m*log(Qts) - m) / (b + m*log(Qts))^2
S <- Qin / (1.0 - C)
revconvert = 1.0 / convert.flow(1, 508) # bring from in/day back to cfs 
Ccon = 0.96
Qconin <- S * (1.0 - Ccon)
Qcon <- S * (1.0 - Ccon) * revconvert 
Scon <- S * Ccon

# Assemble a dataframe of calculated flows. and back calculate S
Svar <- data.frame(
  Qts = Qts,
  C = C,
  Qin = Qin,
  S = S,
  Qconin = Qconin,
  Qcon = Qcon,
  Scon = Scon
)

dS <- Svar[2:nrow(Svar),]$S/Svar[1:(nrow(Svar)-1),]$S
Svar <- Svar[dS > 1,]

Svar$Qs <- 0.0 # initialize
Svar$Qindex <- 0 # initialize
Svar$Smodel <- 0 # initialize
Svar$Qmodelin <- 0 # initialize
Svar$Cmodel <- 0 # initialize
Svar$Ccon <- Ccon # constant

steps = 0
Qmodel = 600
Qindex = 600
Smodel = max(Svar$S)
while (Smodel > 0.01) {
  steps = steps + 1
  Cmodel <- C[as.integer(Qindex)]
  Qmodelin <- Smodel * (1.0 - Cmodel)
  Smodel <- Smodel - Qmodelin
  Qmodel <- Qmodelin * revconvert
  Qcon <- Scon
  Qindex = as.integer(Qmodel)
  Svar$Qs[Qindex] <- Qmodelin * revconvert
  Svar$Smodel[Qindex] <- Smodel
  Svar$Qindex[Qindex] <- Qindex
  Svar$Qmodelin[Qindex] <- Qmodelin
  Svar$Cmodel[Qindex] <- Cmodel
}

Svalid <- Svar[Svar$Qindex > 0,]


plot(Svalid$Cmodel ~ Svalid$Smodel, main="Mount Jackson, C = f(S)", col="blue", pch=4, ylim=c(0.8,1.0))
points(Svalid$Ccon ~ Svalid$Scon, col="green", pch=4)
legend("bottomright", legend = c("C = f(S)", "Constant C"), col = c( "blue", "green"), pch = c(4,4))


plot(Svalid$Qs ~ Svalid$Smodel, main="Mount Jackson, Q = f(S, C), C = f(S)", col="blue", pch=4)
points(Svalid$Qcon ~ Svalid$Scon, col="green", pch=4)
legend("bottomright", legend = c("Q @ C =f(S)", "Q constant C"), col = c("blue", "green"), pch = c(4,4))

SQlow <- Svalid[Svalid$Qts <= 200.0,]
plot(SQlow$Qs ~ SQlow$Smodel, pch=4, col="blue", main="Mount Jackson, Q = f(S, C), C = f(S), For Q <= 200")
points(SQlow$Qcon ~ SQlow$Scon, col="green", pch=4)
legend("bottomright", legend = c("Q @ C =f(S)", "Q constant C"), col = c("blue", "green"), pch = c(4,4))

plot(SQlow$Smodel ~ SQlow$Scon, pch=4, col="black", main="Mount Jackson, Scon vs. Svar", xlim=c(0,0.5), ylim=c(0,0.5))


# Main loop
solve_agwrc_lookup <- function(S, Ctable){
  return(Ctable[Ctable$S >= S,][1,]$C)
}

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
    model_out <- rbind(model_out, data.frame(S=S, Q=Q, Qinch=Qinch, C=Cs))
  }
  return(model_out)
}

test = step_scq(numts=100, Sinit=0.5, method="lookup", in2cfs=revconvert, C=Svar)
# check mass balance (omit day 1 out since it is lagged)
(max(test$S) - min(test$S) + test$Qinch[1]); sum(test$Qinch)

step_scq(numts=10, Sinit=0.5, method="constant", in2cfs=revconvert, C=0.960)
step_scq(numts=10, Sinit=0.5, method="solver",b=1.004269, m=-0.008820, in2cfs=revconvert, C=0.960)

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
    res <- solve_agwrc_log(S=Stg, m, b)
    
    r_check$i[i] <- i
    r_check$r[i] <- res
    
    # use calcd agwrc to get storage
    ex$V_storage[i] <- res * Stg
  }
}