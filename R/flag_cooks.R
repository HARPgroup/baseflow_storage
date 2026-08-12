#' @title flag_cooks
#' @name flag_cooks
#' @description Calculates cooks distance from a regression R object and
#'   determines potential outliers
#' @param model An R object, typically returned by an lm object. This is often
#'   derived in step 06 of the AGWS workflow
#' @returns A list with two vectors. cooks_d, representing the cooks distance
#'   calcualte metric for each observation in model and cooks_flagged, a logical
#'   vector that is TRUE when cooks values are beyond the recommended threshold
#' @export flag_cooks
flag_cooks <- function(model) {
  # define the variables
  cooks_d <- stats::cooks.distance(model)
  n <- length(cooks_d)
  threshold <- 4 / n
  # determine flagged values
  flagged_cooks <- which(cooks_d > threshold)

  # create a new vector with T/F for flagged points
  cooks_flagged <- cooks_d > threshold

  return(list(cooks_d = cooks_d, cooks_flagged = cooks_flagged))
}
