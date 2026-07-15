step2 <- read.csv("C:/HARP/HARP - GitHub/baseflow_storage/adama/DroughtPredictionEvaluation/CowpastureOutlierData/step2_02016000.csv")

flag_storm <- rep(F, nrow(step2))

condition_start <- step2$AGWR > 1 & (step2$delta_AGWR < 0.97 | step2$delta_AGWR > 1.03)

condition_continue <- step2$AGWR > 1 | step2$delta_AGWR < 0.97 | step2$delta_AGWR > 1.03

i <- 1

while (i <= nrow(step2) - 2) {

  initial_flag <- condition_start[i]

  if (initial_flag) {

    j <- i + 1

    while (j <= nrow(step2) && condition_continue[j]) {

      j <- j + 1
    }

    event_length <- j - 1

    if (event_length >= 3) {
      flag_storm[i:(j - 1)] <- T
    }

    i <- j

  } else {

    i <- i + 1

  }
}

flag_storm <- rle(flag_storm)
flag_storm$values <- flag_storm$values & (flag_storm$lengths >= 3)
flag_storm <- inverse.rle(flag_storm)
step2$flag_storm <- flag_storm


library(dplyr)

step2filt <- step2 |>
  group_by(GroupID) |>
  group_modify(~ {

    data <- .x

    remove <- rep(F, nrow(data))

    runs <- rle(data$flag_storm)

    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1

    for (i in seq_along(runs$values)) {

      # Skip FALSE runs
      if (!runs$values[i]) next

      # Always remove the TRUE run
      remove[starts[i]:ends[i]] <- T

      # Remove previous FALSE run if < 7
      if (i > 1 &&
          !runs$values[i - 1] &&
          runs$lengths[i - 1] < 11) {

        remove[starts[i - 1]:ends[i - 1]] <- T
      }

      # Remove next FALSE run if < 7
      if (i < length(runs$values) &&
          !runs$values[i + 1] &&
          runs$lengths[i + 1] < 11) {

        remove[starts[i + 1]:ends[i + 1]] <- T
      }
    }

    data$remove <- remove
    return(data)
  }) %>%
  ungroup()

step2a <- step2filt |>
  dplyr::filter(!remove) |>
  dplyr::select(-remove)

step2a$flag_storm <- NULL

"step2a_02016000.csv" <- step2a
gageID <- '02016000'

filtered_out_ids <- setdiff(step2$GroupID, step2a$GroupID)
