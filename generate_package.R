##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check irtempirical
# R CMD Rd2pdf irtempirical
# R CMD build irtempirical --resave-data
library(devtools)
library(roxygen2)
setwd("/mnt/WDRED_REMOTE/repositories/irtempirical/")
setwd("/mnt/WD500/public_rstatistics/")
usethis::create_package("irtempirical")
document()
install()

