#' @title flag_outliers_IQR
#' @name flag_outliers_IQR
#' @details
#' Calculates the Inner Quartile Range (IQR; 25-75%) of an input vector and
#' returns a logical vector that is TRUE when values are out of the 1.5 * IQR
#' @param values numeric. A vector of numeric values from which the IQR will be
#'   calculated.
#' @returns logical. A vector of logical values that are TRUE if the
#'   corresponding elment in values is out of the 1.5 * IQR
#' @export flag_outliers_IQR
flag_outliers_IQR <- function(values) {
  # calculate Q1, Q3, and IQR
  Q1 <- stats::quantile(values, 0.25, na.rm = TRUE)
  Q3 <- stats::quantile(values, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR

  # create a vector of logical IQR outlier flags
  flagged_outliers <- (values < lower | values > upper)

  return(flagged_outliers)
}
