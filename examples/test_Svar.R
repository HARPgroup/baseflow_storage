
Qts <- 0:600
b = 1.004269
m = -0.00820
C <- b + m * log(Qts)
source("C:\\Users\\gcw73279.COV\\Desktop\\gitBackups\\OWS\\baseflow_storage\\convert.flow.R")
source("C:\\Users\\gcw73279.COV\\Desktop\\gitBackups\\OWS\\baseflow_storage\\workspace\\adam_hurlich\\DroughtEvaluation\\Functions\\forwardForecastv2.R")
revconvert = 1.0 / convert.flow(1, 508) # bring from in/day back to cfs 

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

test <- step_scq(numts=10, Sinit=0.5, method="lookup", in2cfs=revconvert, C=Svar)

forwardForecast(test$Q[1],1:10,AGWRC = test$C)
test$Q

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