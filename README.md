# baseflow_storage
## Description
A collection of functions used to parse a flow time series to identify potential
periods in which a river system is at baseflow. From there, additional functions
can analyze these baseflow events to find baseflow recession rates and run
analyses to look for any correlation between baseflow recession rate and flow.
Additional functions in this package can estimate active groundwater storage
based on the recession coefficient and flow using the HSPF assumptions. Finally,
forecasting functions are available to project flow based on a recession decay
coefficient. This work is wrapped in a linux workflow within
https://github.com/HARPgroup/meta_model/tree/main/models/drought/agws


## Installation
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
devtools::install_github("HARPgroup/baseflow_storage")
# alternate, install a development branch for testing:
devtools::install_github("HARPgroup/baseflow_storage", ref = "packageinitialize", force=TRUE)

#EXAMPLE FUNCTION DOCUMENTATION
??convert.flow
```
<!-- badges: start -->
[![R-CMD-check](https://github.com/HARPgroup/baseflow_storage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HARPgroup/baseflow_storage/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

### Patch Notes:
V 0.0.4 July 22, 2026
1. Added StreamStats API function to get watershed and point geometries for a
given lat/long often from a USGS gage

V 0.0.3 July 09, 2026
1. Added slope and heteroscedasticity functions.

V 0.0.2 July 09, 2026
1. Updated `forwadForecast()` with `regressionLimitAGWRC()` to prevent variable
forecasts from dropping below minimum values to prevent extrapolation. Added a
method to ensure Q0 is in range as well on lm_constant.

V 0.0.1 June 29, 2026
1. First package build.
