# Calculates AGWRC based off Regression Equation
RegressionAGWRC <- function(Flow, m, b) {
  AGWRC <- m * log(Flow) + b
}