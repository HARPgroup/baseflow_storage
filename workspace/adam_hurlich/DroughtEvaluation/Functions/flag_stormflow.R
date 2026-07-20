library(dplyr)

# input df --> step2 from baseflow workflow
step2 <- read.csv("C:/HARP/HARP - GitHub/baseflow_storage/adama/DroughtPredictionEvaluation/CowpastureOutlierData/step2_02016000.csv")

flag_stormflow <- function(df, flow_threshold = 1.10, min_days = 11){

  # df length of input df
  flag_runoff <- rep(F, nrow(df))

  # initial flagging condition
  condition_start <- step2$AGWR > 1 & (step2$delta_AGWR < 0.97 | step2$delta_AGWR > 1.03)

  # continuing flagging
  condition_continue <- step2$AGWR > 1 | step2$delta_AGWR < 0.97 | step2$delta_AGWR > 1.03

  n <- nrow(step2)
  i <- 1

  # run length df
  while (i <= n) {

    # look for initial flagging
    if (!condition_start[i]) {
      i <- i + 1
      next
    }
    j <- i

    # start defining flagged period
    while (j <= n && condition_continue[j]) {
      j <- j + 1
    }

    # minimum 3 days flagged to continue
    if ((j - i) < 3) {
      i <- j
      next
    }

    # find flow0 of flagged period
    Flow0 <- step2$Flow[i-1]
    flag_runoff[i:(j - 1)] <- TRUE

    k <- j

    # flag_runoff df set to T for condition_continue T
    while (k <= n) {

      if (condition_continue[k]) {
        flag_runoff[k] <- TRUE
        k <- k + 1
        next
      }
      gap_day <- k

      # Logical vector, Allowance of 1 day gap or flow[k] > 1.10 * Flow0
      if (gap_day == n) break
      resumes <- condition_continue[gap_day + 1] || step2$Flow[gap_day + 1] >= flow_threshold * Flow0

      # If condition of resumes met, continue condition_continue methodology
      if (resumes) {

        flag_runoff[gap_day] <- TRUE

        k <- gap_day + 1
        next

      } else {

        # 110% of initial flow continues flagging
        if (step2$Flow[gap_day] >= flow_threshold * Flow0) {
          flag_runoff[gap_day] <- TRUE
          k <- gap_day + 1

          # final check to avoid residuals flow, 110% is tentative
          while (k <= n && step2$Flow[k] >= flow_threshold * Flow0) {
            flag_runoff[k] <- TRUE
            k <- k + 1
          }
        }

        break
      }
    }

    i <- k
  }

  # rle to check for minimum flag length of 3 days
  flag_runoff <- rle(flag_runoff)
  flag_runoff$values <- flag_runoff$values & (flag_runoff$lengths >= 3)
  flag_runoff <- inverse.rle(flag_runoff)
  step2$flag_runoff <- flag_runoff

  # filtering for day length before and after flagged period
  step2filt <- step2 |>
    dplyr::group_by(GroupID) |>
    dplyr::group_modify(~ {

      data <- .x

      remove <- rep(F, nrow(data))

      runs <- rle(data$flag_runoff)

      ends <- cumsum(runs$lengths)

      starts <- ends - runs$lengths + 1

      for (i in seq_along(runs$values)) {

        # ignore F values
        if (!runs$values[i]) next

        # remove all T values
        remove[starts[i]:ends[i]] <- T

        # remove previous F run if length < min_days (11 default)
        if (i > 1 && !runs$values[i - 1] && runs$lengths[i - 1] < min_days) {

          remove[starts[i - 1]:ends[i - 1]] <- T
        }

        # remove next F run if length < min_days (11 default)
        if (i < length(runs$values) && !runs$values[i + 1] && runs$lengths[i + 1] < min_days) {

          remove[starts[i + 1]:ends[i + 1]] <- T
        }
      }

      data$remove <- remove
      return(data)
    }) %>%
    dplyr::ungroup()

  step2a <- step2filt |>
    dplyr::filter(!remove) |>
    dplyr::select(-remove)

  step2a$flag_runoff <- NULL

return(step2a)

}



result <- flag_stormflow(df = step2)

write.csv(step2a, file = "step2a_02016000_11day.csv", row.names = F)

filtered_out_ids <- setdiff(stepog$GroupID, step2a$GroupID)

library(plotly)

step6a <- read.csv("C:/HARP/HARP - GitHub/baseflow_storage/adama/flag_stormflow/02016000/flag_stormflow_11day_v2/step06a_02016000.csv")
step7a <- read.csv("C:/HARP/HARP - GitHub/baseflow_storage/adama/flag_stormflow/02016000/flag_stormflow_11day_v2/step07a_02016000.csv")

# Your data
df <- data.frame(
  x = step6a$median_flow,
  y = step6a$event_AGWRC
)

# Your regression coefficients
m <- step7a$m   # should be negative for downward curve
b <- step7a$b

# Smooth curve
curve_df <- data.frame(
  x = seq(min(df$x), max(df$x), length.out = 500)
)

curve_df$y <- m * log(curve_df$x) + b

# Plot
plot_ly() %>%
  add_markers(
    data = df,
    x = ~x,
    y = ~y,
    name = "Observed",
    marker = list(size = 8)
  ) %>%
  add_lines(
    data = curve_df,
    x = ~x,
    y = ~y,
    name = "y = m log(x) + b",
    line = list(width = 3)
  ) %>%
  layout(
    xaxis = list(title = "Characteristic Event Flow (median, cfs)"),
    yaxis = list(title = "Event AGWRC"),
    title = "AGWRC vs. Flow (Event-Level) - USGS-02016000 - +/- 11 Day Window"
  )

Q = 106
y <- (log(Q) * m) + b
