# baseflow_storage

DEQ Users
This package is available for internal distribution on the DEQ Posit Package manager. Additional information on this may be found in the DEQ Methods Encyclopedia. Users will need to add the UAT package manager to their list of secondary repositories: https://positpackagemanager-uat.deq.virginia.gov/DEQmethods/latest. A config file may be provided by the package managers for database integration.
``` R
install.packages("agws")
```

All Other Users
A config file may be provided by the package managers for database integration.
```R
install.packages("devtools")
library("devtools")
#Make sure it hasn't been called, but if it has we can unload it
unloadNamespace('agws')
#Get the master branch deployment of the package
devtools::install_github("HARPgroup/agws")
# alternate, install a development branch for testing:
devtools::install_github("HARPgroup/agws", ref = "packageinitialize", force=TRUE)

#EXAMPLE FUNCTION DOCUMENTATION
??convert.flow
```
<!-- badges: start -->
[![R-CMD-check](https://github.com/HARPgroup/baseflow_storage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HARPgroup/baseflow_storage/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
