#Root Finder for C

#Command line args
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript agwrc.R <input.csv> <output.csv>")
}

input_file  <- args[1]
output_file <- args[2]
AGWRC_check <- function(C, S, m, b) {
  C - (m * log(S * (1 - C)) + b)
}

#Make tolerance smaller for a more accurate but longer result
find_AGWRC <- function(S, m, b, c_low = 0.7, c_high = 1.0, tol = 0.0001, max_iter = 500) {
  if (sign(AGWRC_check(c_low, S, m, b)) == sign(AGWRC_check(c_high, S, m, b))) {
    warning(paste("No sign change detected for S =", S, "— skipping."))
    return(NA)
  }
  
  for (i in 1:max_iter) {
    c_guess <- (c_high + c_low) / 2
    QC <- AGWRC_check(c_guess, S, m, b)
    
    if (abs(QC) <= tol) {
      return(c_guess)
    }
    
    if (QC < 0) {
      c_low <- c_guess
    } else {
      c_high <- c_guess
    }
  }
  
  warning(paste("Did not converge for S =", S))
  return(NA)
}


df <- read.csv(input_file)
df$c_calculated <- mapply(find_AGWRC, df$S, df$m, df$b)

write.csv(df, output_file, row.names = FALSE)
message(paste("Done. Results written to", output_file))

# --- Example usage ---
# Rscript agwrc.R my_inputs.csv my_outputs.csv
#
# Where my_inputs.csv contains:
#   S,m,b
#   1.0,-0.0003047,0.9418478
#   0.8,-0.0088198,0.920285
#
# Output my_outputs.csv will look like:
#   S,m,b,c_estimate
#   1.0,-0.0003047,0.9418478,0.944...
#   0.8,-0.0088198,0.920285,0.932...