bfd_agwrc_project <- function (Q0, C, n) {
  # Q(t) = Q0 * e^(-k * t)
  # where t = number of days into the future
  # k = -ln(C); where C = AGWRC
  k = -log(C)
  Qt = Q0 * exp(-k * n)
  return(Qt)
}
