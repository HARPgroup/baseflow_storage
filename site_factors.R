source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/convert.flow.R")

site_factors <- function(da_sqmi, flow_vec = Qcfs, vec_for_reg = NULL, m, b){
  #convert flow fron cfs to watershed inches per day
  Qin <- convert.flow(flow_vec, da_sqmi)
  
  if (is.null(vec_for_reg)) {
    vec_for_reg <- Qin
  }
  
  #calculate AGWRC with recession
  C <- b + (m * log(vec_for_reg))
  
  #bound C by 0 and 1
  C <- pmin(pmax(C, 0.001), 0.999)
  
  return(data.frame(flow_vec, Qin, C))
}