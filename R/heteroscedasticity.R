#'@title Heteroscedasticity
#'@name Heteroscedasticity
#' @details Determines if heteroscedasticity is present at a given gage by
#' running the Breusch-Pagan test based on the model summary from the baseflow
#' workflow e.g. bptest(model)
#' Runs the White Test, a more general test for heteroscedasticity that does
#' not assume a specific functional form, based on the model summary from the
#' baseflow workflow e.g. bptest(model, ~fitted(model) + I(fitted(model)^2))
#' @param model an object of class "lm" representing a linear regression, often
#' from a series of summarized baseflow events as may be generated from
#' \code{fit_agwrc_regression()} in the agws worflow
#' @return A dataframe with five fields: the White test p-value, the
#' Breusch-Pagan test p-value, the White test statistic, the Breusch-Pagan test
#' statistic, and a T/F interpretation of the p-value. If either of the two
#' tests has a p-value < 0.05, heteroscedasticity is flagged True as a concern.
#' If both p-values are > 0.05, heteroscedasticity is flagged False as a concern.
#'@importFrom rlang .data
#'@export
heteroscedasticity <- function(model) {
  # run the Breusch-Pagan test
  bp_test <- lmtest::bptest(model)

  # write message with interpretation
  bp_msg <- paste0(
    "Breusch-Pagan test p-value = ",
    round(bp_test$p.value, 4),
    if(bp_test$p.value < 0.05) {
      ". Heteroscedasticity is likely causing some uncertainty in the model."
    }
    else{
      ". Heteroscedasticity is likely not a concern."
    }
  )

  # run the White test
  white_test <- lmtest::bptest(model, ~fitted(model) + I(fitted(model)^2))

  # write message with interpretation
  white_msg <- paste0(
    "White test p-value = ",
    round(white_test$p.value, 4),
    if(white_test$p.value < 0.05){
      ". Heteroscedasticity is likely causing some uncertainty in the model."
    } else {
      ". Heteroscedasticity is likely not a concern."
    }
  )

  # output both messages in the console
  message(bp_msg)
  message(white_msg)

  # Create data frame
  hetero_df <- data.frame(
    white_test_p = round(white_test$p.value, 4),
    breusch_pagan_p = round(bp_test$p.value, 4)
  )

  # add test statistics to dataframe
  hetero_df <- hetero_df |>
    dplyr::mutate(white_test_stat = round(white_test$statistic, 4)) |>
    dplyr::mutate(breusch_pagan_stat = round(bp_test$statistic, 4))

  # add T/F statement for heteroscedasticity significance
  hetero_df <- hetero_df |>
    dplyr::mutate(heteroscedasticity_a_concern = (.data$breusch_pagan_p <= 0.05) |
                    (.data$white_test_p <= 0.05)
    )
  return(hetero_df)
}
