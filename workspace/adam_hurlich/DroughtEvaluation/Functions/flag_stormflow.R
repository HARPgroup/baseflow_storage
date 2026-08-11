library(dplyr)

## input df --> step2 from baseflow workflow
#step2 <- read.csv("C:/HARP/HARP - GitHub/baseflow_storage/adama/DroughtPredictionEvaluation/CowpastureOutlierData/step2_02016000.csv")

#'@title flag_stormflow
#'@name
#'flag_stormflow
#'@description
#'Additional baseflow workflow filtering
#'@details
#'From step 2 in baseflow workflow, applies additional filtering conditions to reject events
#'with substantial flow runoff. Events are filtered, and all days meeting runoff conditions are removed.
#'Event length is then checked by days prior and after filtered period. Event fragments must meet min_days to exist
#'@param df data.frame with GroupID, Flow, AGWR, and delta_AGWR
#'@param flow_threshold numeric, default of 1.10 set for runoff tolerance. Flow0 * flow_threshold =< Flow[i]
#'@param min_days numeric, default of 11 set for minimum length of days prior and after runoff filtering
#'@return df data.frame with GroupID, Flow, AGWR, and delta_AGWR, all filtered events are removed
#'@importFrom dplyr group_by group_modify ungroup filter select
#'@importFrom rlang .data
#'@export
flag_stormflow <- function(df, flow_threshold = 1.10, min_days = 11){

  # df length of input df
  flag_runoff <- rep(F, nrow(df))

  # initial flagging condition
  condition_start <- df$AGWR > 1 & (df$delta_AGWR < 0.97 | df$delta_AGWR > 1.03)

  # continuing flagging condition
  condition_continue <- df$AGWR > 1 | df$delta_AGWR < 0.97 | df$delta_AGWR > 1.03

  n <- nrow(df)
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
    Flow0 <- df$Flow[i - 1]
    flag_runoff[i:(j - 1)] <- T

    k <- j

    # flag_runoff df set to T for condition_continue T past 3 days
    while (k <= n) {

      if (condition_continue[k]) {
        flag_runoff[k] <- T
        k <- k + 1
        next
      }
      gap_day <- k

      # Logical vector, Allowance of 1 day gap or flow[k] > 1.10 * Flow0
      if (gap_day == n) break
      resumes <- condition_continue[gap_day + 1] || df$Flow[gap_day + 1] >= flow_threshold * Flow0

      # If condition of resumes met, continue condition_continue methodology
      if (resumes) {

        flag_runoff[gap_day] <- T

        k <- gap_day + 1
        next

      } else {

        # 110% of initial flow continues flagging
        if (df$Flow[gap_day] >= flow_threshold * Flow0) {
          flag_runoff[gap_day] <- T
          k <- gap_day + 1

          # final check to avoid residuals flow, 110% is tentative
          while (k <= n && df$Flow[k] >= flow_threshold * Flow0) {
            flag_runoff[k] <- T
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
  df$flag_runoff <- flag_runoff

  # filtering for day length before and after flagged period
  df_filt <- df |>
    dplyr::group_by(GroupID) |>
    dplyr::group_modify(~ {
      data <- .x
      remove <- rep(F, nrow(data))

      # end and start indices from rle
      runs <- rle(data$flag_runoff)
      ends <- cumsum(runs$lengths)
      starts <- ends - runs$lengths + 1

      # check i for 1:length of rle values column
      for (i in seq_along(runs$values)) {

        # ignore F values
        if (!runs$values[i]) next

        # remove all initial T values
        remove[starts[i]:ends[i]] <- T

        # remove previous F run if length < min_days (11 default)
        if (i > 1 && !runs$values[i - 1] && runs$lengths[i - 1] < min_days) {

          remove[starts[i - 1]:ends[i - 1]] <- T
        }

        # remove following F run if length < min_days (11 default)
        if (i < length(runs$values) && !runs$values[i + 1] && runs$lengths[i + 1] < min_days) {

          remove[starts[i + 1]:ends[i + 1]] <- T
        }
      }

      data$remove <- remove
      return(data)
    }) %>%
    dplyr::ungroup()

  # remove indices with T
  df_a <- df_filt |>
    dplyr::filter(!remove) |>
    dplyr::select(-remove)

  df_a$flag_runoff <- NULL

  # list of filtered out GroupIDs
  cat("Filtered out GroupIDs:\n")
  cat(setdiff(df$GroupID, df_a$GroupID))

return(df_a)

}

## Local Testing
#step2a <- flag_stormflow(df = step2)
