
#cran rjags count

remotes::install_github("r-hub/cranlogs")

cran_downloads(package = "plyr", from = "2014-01-01", to = "2014-02-01")

library(cranlogs)

x = cran_downloads(package = "rjags", from = "1900-01-01", to = "2014-02-01")
sum(x[,2])
