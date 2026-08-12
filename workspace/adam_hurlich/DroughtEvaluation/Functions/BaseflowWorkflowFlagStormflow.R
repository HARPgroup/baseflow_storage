#####-LIBRARY-#####

library(hydrotools)
library(agws)
library(dplyr)
library(plotly)

#####-DATA-#####

gage_obj <- WaterGageDaily$new(gage_id = "01628500")

step1 <- gage_obj$gage_data

step1$AGWR <- calc_AGWR(step1$value)

step1$delta_AGWR <- calc_delta_AGWR(step1$AGWR)

step1 <- step1[!is.na(step1$delta_AGWR), ]

#####-STEP2-flag_stormflow-#####

step2 <- flag_stormflow(df = step1)

step2 <- baseflow_groupID(df = step2, min_len = 14)

#####-STEP3-flag_stormflow_AGWR#####

step3 <- flag_stormflow_AGWR(df = step2)

step3 <- baseflow_groupID(df = step3, min_len = 14)

#####-STEP4-residual_flow-#####

step4 <- residual_flow(df = step3)

step4 <- baseflow_groupID(df = step4, min_len = 14)

#####-STEP5-#####

step5 <- step4 |>
  filter(value < quantile(value, 0.90))

step5 <- baseflow_groupID(df = step5, min_len = 14)

#####-STEP6-#####

step5 <- step5 |>
  filter(value != 0)

step6 <- calc_AGWRC(df = step5, value = "value", time = "time")

step6 <- step6 |>
  filter(event_AGWRC < 1)

step6_model <- fit_agwrc_regression(step6)

#####-FUNCTIONS-#####

baseflow_groupID <- function(df, value, time, min_len = 7) {

  # find isolated runs of minimum length 7, assign groupID, look for next event
  df_a <- df |>
    dplyr::mutate(
      new_run = c(T, diff(time) != 1),
      run = cumsum(new_run)
    ) |>
    dplyr::group_by(run) |>
    dplyr::filter(dplyr::n() >= min_len) |>
    dplyr::ungroup() |>
    dplyr::mutate(GroupID = match(run, unique(run))) |>
    dplyr::select(-new_run, -run)

  return(df_a)
}

flag_stormflow <- function(df, dAGWRrange = 0.03) {

  # logical vector of F for length df
  flag_storm <- rep(F, nrow(df))

  # logical vectors for flag_stormflow conditions
  condition_start <- df$AGWR > 1 & (df$delta_AGWR < (1 - dAGWRrange) | df$delta_AGWR > (1 + dAGWRrange))

  condition_continue <- df$AGWR > 1 | df$delta_AGWR < (1 - dAGWRrange) | df$delta_AGWR > (1 + dAGWRrange)

  i <- 1

  # index through length of df - 2, event must be 3 days long
  while (i <= nrow(df) - 2) {

    # condition_start[i] <- T?
    if (condition_start[i]) {

      # j represents potential event
      j <- i + 1

      # j less than length of df and condition_continue[j] <- T?
      while (j <= nrow(df) && condition_continue[j]) {

        # j increases = potential event length increases
        j <- j + 1
      }

      event_length <- j - 1

      # event_length must be 3 days minimum
      if (event_length >= 3) {

        # flag_storm logical vector indexs <- T
        flag_storm[i:(j - 1)] <- T

      }

      # start next sequence
      i <- j

    } else {

      # continue searching for flag_stormflow
      i <- i + 1

    }
  }

  # rle for values and length of flag_storm logical vector, min length 3 consecutive days
  flag_storm <- rle(flag_storm)
  flag_storm$values <- flag_storm$values & (flag_storm$lengths >= 3)
  flag_storm <- inverse.rle(flag_storm)
  df_a <- df[!flag_storm, ]

  return(df_a)

}

flag_stormflow_AGWR <- function(df, min_len = 7) {

  # AGWR flag condition
  flag_AGWR <- df$AGWR >= 1

  # rle for values and length of flag_AGWR logical vector, min length 3 consecutive days
  r <- rle(flag_AGWR)
  r$values <- r$values & (r$lengths >= 3)
  df$flag_AGWR <- inverse.rle(r)

  # group_modify by GroupID
  df_a <- df |>
    dplyr::group_by(GroupID) |>
    dplyr::group_modify(~ {
      data <- .x
      remove <- rep(F, nrow(data))

      # finding start and ending of each GroupID through rle of flag_AGWR
      runs <- rle(data$flag_AGWR)
      ends <- cumsum(runs$lengths)
      starts <- ends - runs$lengths + 1

      # check i for 1:length of rle values column
      for (i in seq_along(runs$values)) {

        # ignore F values
        if (!runs$values[i]) next

        # remove all initial T values
        remove[starts[i]:ends[i]] <- T

        # remove previous F run if length < min_len (7 default)
        if (i > 1 && !runs$values[i - 1] && runs$lengths[i - 1] < min_len) {

          remove[starts[i - 1]:ends[i - 1]] <- T
        }

        # remove following F run if length < min_len (7 default)
        if (i < length(runs$values) && !runs$values[i + 1] && runs$lengths[i + 1] < min_len) {

          remove[starts[i + 1]:ends[i + 1]] <- T
        }
      }

      data$remove <- remove
      return(data)
    }) %>%
    dplyr::ungroup()

  # remove indices with T, clean df
  df_a <- df_a |>
    dplyr::filter(!remove) |>
    dplyr::select(-remove)

  return(df_a)

}

residual_flow <- function(df, delta_thresh = 0.995) {

  # logical vector length of df
  residual_flow <- rep(F, nrow(df))

  # logical column in df for delta_AGWR >= 0.995
  df$condition_delta_AGWR <- df$delta_AGWR >= delta_thresh

  # all unique GroupID
  for (id in unique(df$GroupID)) {

    # indices of GroupID matching id
    idx <- which(df$GroupID == id)

    # smaller df of just GroupID == id days
    data <- df[idx, ]

    i <- 1
    start <- F
    gap <- F

    # run length of data
    while (i <= nrow(data)) {

      # if F, run this portion
      if (!start) {

        # data[i] <- T? Event starts and i = 1:2 removed
        if (data$condition_delta_AGWR[i]) {

          start <- T

          residual_flow[idx[1:2]] <- T

        } else {

          # i = 1 <- F, maximum gap of 1 day used
          if (i == 1) {
            gap <- T
          }

          # i = 2 <- F and i = 1 <- F, end id
          if (i == 2 && !data$condition_delta_AGWR[1]) {

            break
          }
        }

      } else {

        # data[i] <- F? Check gap
        if (!data$condition_delta_AGWR[i]) {

          if (!gap) {

            # Allow one failed day
            gap <- T

          } else {

            # remove 3:i-1
            residual_flow[idx[3:i-1]] <- T
            break
          }
        }
      }

      i <- i + 1
    }
  }

  df_a <- df[!residual_flow, ]

  df_a$condition_delta_AGWR <- NULL

  return(df_a)
}

calc_AGWRC <- function(df, value, time) {

    ids <- unique(df$GroupID)

    result <- lapply(ids, function(id) {

      dat <- df[df$GroupID == id, ]

      Flow <- dat[[value]]
      Date <- dat[[time]]

      log_lm <- stats::lm(log(Flow) ~ Date)
      event_sum <- summary(log_lm)

      data.frame(
        GroupID = id,
        median_flow = median(Flow, na.rm = TRUE),
        event_AGWRC = exp(coef(log_lm)[2]),
        r2 = event_sum$r.squared
      )
    })

    do.call(rbind, result)
}

#####-PLOT-#####

model_summary <- summary(step6_model)

m = coef(model_summary)[2]
b = coef(model_summary)[1]

df <- data.frame(
  x = step6$median_flow,
  y = step6$event_AGWRC,
  label = paste0("GroupID: ", step6$GroupID)
)

# Smooth curve
curve_df <- data.frame(
  x = seq(min(df$x), max(df$x), length.out = 500)
)

curve_df$y <- m * log(curve_df$x) + b


# Plotly AGWRC vs. Median Flow
plot_ly() %>%
  add_markers(
    data = df,
    x = ~x,
    y = ~y,
    name = "Observed",
    marker = list(size = 8),
    mode = "markers+text",
    text = ~label,
    textposition = "top center"
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
    title = "AGWRC vs. Flow (Event-Level) - USGS-01628500"
)
