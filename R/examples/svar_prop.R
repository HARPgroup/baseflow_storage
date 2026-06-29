basepath="/var/www/R"
source("/var/www/R/config.R")
### INSTRUCTIONS ###
# To run this example, all code must be run to set up
# then

#### Set Up Lookup for Soil #### 
# set up the lookup table using values from
# the test_Svar.R example, grabbing select rows
# like so: Svar[c(1,25,50,100,150,200,250),]
Sprop = RomProperty$new(ds)
Sprop$set_matrix(
  data.frame(
    c("Qts", "C", "Qin", "S"),
    c(4,0.9920419, 0.0002928353, 0.03679706),
    c(28, 0.9748790, 0.0020498471, 0.08159880),
    c(53, 0.9692510, 0.0038800677, 0.12618527),
    c(103, 0.9633907, 0.0075405089, 0.20597244),
    c(153, 0.9599005, 0.0112009501, 0.27932918),
    c(203, 0.9574065, 0.0148613913, 0.34891238),
    c(253, 0.9554645, 0.0185218325, 0.41588922)
  )
)
Svar = Sprop$data_matrix