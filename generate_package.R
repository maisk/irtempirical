##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check irtempirical
# R CMD Rd2pdf irtempirical
# R CMD build irtempirical --resave-data
.rs.restartR()
library(devtools)
library(roxygen2)
setwd("/mnt/WDRED_REMOTE/repositories/irtempirical/")
# usethis::create_package("irtempirical")
document()
install()

