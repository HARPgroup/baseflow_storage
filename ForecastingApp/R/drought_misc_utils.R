
# Small helper (used in server code sometimes)
`%||%` <- function(x, y) if (!is.null(x)) x else y

#Get summary stats for a numeric vector
population_stats <- function(vals){
  out <- tibble::tibble(
    n_events = length(vals),
    min      = min(vals, na.rm = TRUE),
    p05      = unname(stats::quantile(vals, 0.05, na.rm = TRUE)),
    p10      = unname(stats::quantile(vals, 0.10, na.rm = TRUE)),
    p25      = unname(stats::quantile(vals, 0.25, na.rm = TRUE)),
    median   = stats::median(vals, na.rm = TRUE),
    mean     = mean(vals, na.rm = TRUE),
    p75      = unname(stats::quantile(vals, 0.75, na.rm = TRUE)),
    p90      = unname(stats::quantile(vals, 0.90, na.rm = TRUE)),
    p95      = unname(stats::quantile(vals, 0.95, na.rm = TRUE)),
    max      = max(vals, na.rm = TRUE)
  )
  return(out)
}