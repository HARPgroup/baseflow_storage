# Download coefficient data
coef <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ih_data/data/QC_coefficients.csv")

# Download event data
event_data <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/bf_events_01634000.csv")



# Add column for calculated flow 
event_data$calc_flow_model <- 10^((event_data$AGWRC - coef$intercept[5])/coef$slope[2])
event_data$calc_flow_gage <- 10^((event_data$AGWRC - coef$intercept[5])/coef$slope[1])

# example
(coef$slope[5]*log(70.9))+coef$intercept[5]

myCsv <- getURL("https://deq1.bse.vt.edu/p6/out/land/subsheds/eos/N51165_0111-0211-0411.csv", ssl.verifypeer = FALSE)
d <- read.csv(textConnection(myCsv))


ex <- read.csv("https://deq1.bse.vt.edu/p6/vadeq/input/param/for/P620171001WQf/PWATER.csv", header = TRUE)  
colnames(ex) <- ex[1,]
ex <- ex[-1,]
colnames(ex)[1] <- "LANDSEG"


ex <- ex[,-c(43:66)]

write.csv(ex, "C:/Users/ilona/OneDrive - Virginia Tech/HARP/Github/baseflow_storage/data/P620171001WQf_model_params.csv")

ex <- ex[,c("LANDSEG", "AGWR")]            
