##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check irtempirical
# R CMD Rd2pdf irtempirical
# R CMD build irtempirical --resave-data
.rs.restartR()
library(devtools)
library(roxygen2)
directory <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/")
setwd(directory)
# usethis::create_package("irtempirical")
document()
install()

library(irtempirical)
help(package = "irtempirical")

pkgdown::build_site(paste0(getwd()))
system("R CMD Rd2pdf irtempirical --force")
system("R CMD build irtempirical --resave-data")
