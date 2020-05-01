

# see tab 13.2

options(scipen =  999)

library(masteringmetrics)
library(devtools)

##################################################
#                   SAY NO TO ALL UUPDATES      #                       
##################################################
install.packages("Rcpp")

if (!require('devtools')) install.packages('devtools')

devtools::install_github( "bquast/rddtools" )

library(rddtools)
