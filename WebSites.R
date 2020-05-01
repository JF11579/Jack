
#https://api.rpubs.com/robincrlee/broom-coef

#https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html

#############################################################
# https://donskerclass.github.io/EconometricsII/RegressionDiscontinuity.html#
######################################################3
library(rddtools)
data(house)

head(house,5)

# Specify x, y, and cutpoint = 0 Dem-Rep vote share
house_rdd<-rdd_data(y=house$y,x=house$x,cutpoint=0)
head(house_rdd,3)
dim(house)
head(house_rdd)
dim(house_rdd)
str(house_rdd)

# Run OLS RD estimate, linear 
# with different slope on each side
reg_para <- rdd_reg_lm(rdd_object=house_rdd)
head(reg_para,3)

plot(reg_para,xlab="Dem-Rep Vote Share, t", 
     ylab="Dem-Rep Vote Share, t+1", 
     main="Parametric RD")

'''
different slope on each side: 11.8% incumbency advantage
'''
#Estimate MSE- optimal bandwidth on Lee (#MSE= Mean Sq Error)
# data by Imbens-Kalyanaraman procedure
LEEbw<-rdd_bw_ik(house_rdd)  # what functionis RDD_BW_IK ?
#https://www.rdocumentation.org/packages/rddtools/versions/0.4.0/topics/rdd_bw_ik
'''
Find optimal bandwith ?\
 ex: 
      data(house)
      rd<- rdd_data(x=house$x, y=house$y, cutpoint=0)
      rdd_bw_ik(rd)
'''
head(LEEbw)

# Run local linear RD estimate, 
# linear with different slope on each side
LEEnp<-rdd_reg_np(rdd_object=house_rdd,bw=LEEbw)

plot(LEEnp,xlab="Dem-Rep Vote Share, t", 
     ylab="Dem-Rep Vote Share, t+1", 
     main="Nonparametric RD")

########################################################################
#    Now try it on Jack's data                                         #
########################################################################

#OPTIONS
options(scipen =  999)

#PACKAGES

library(tidyverse)
library(broom)
library(masteringmetrics)
library(devtools)
library(rddtools)
'''
install.packages("Rcpp")

if (!require('devtools')) install.packages('devtools')

devtools::install_github( "bquast/rddtools" )

library(rddtools)
'''

Jack_data<- read_csv("Nat_Inc_2.csv")
is.data.frame(Jack_data)
str(Jack_data)
head(Jack_data,3)
# Now subset and rename cols
Jack_data<- select(Jack_data,number_year , value)
Jack_data<-rename(Jack_data , x = number_year )
Jack_data<-rename(Jack_data , y = value )
head(Jack_data , 3)
str(Jack_data)

# Specify x, y, and cutpoint = 0 Dem-Rep vote share
#house_rdd<-rdd_data(y=house$y,x=house$x,cutpoint=0)
Jack_rdd<-rdd_data(y=Jack_data$y,x=Jack_data$x,cutpoint= 1993)
str(Jack_rdd)

#reg_para <- rdd_reg_lm(rdd_object=house_rdd)
reg_para_mine <- rdd_reg_lm(rdd_object =Jack_rdd )

plot(reg_para_mine,xlab="Year, t", 
     ylab="National Income, t+1", 
     main="Jack Parametric RD")

View(Jack_rdd)

########################################################################
#   Twds Data Science                                                  #
#https://towardsdatascience.com/the-crown-jewel-of-causal-inference-regression-discontinuity-design-rdd-bad37a68e786
########################################################################

#cutoff point = 3.5
GPA <- runif(1000, 0, 4)
future_success <- 10 + 2 * GPA + 10 * (GPA>=3.5) + rnorm(1000)

#install and load the package ‘rddtools’
#install.packages(“rddtools”)
#library(rddtools)
data <- rdd_data(future_success, GPA, cutpoint = 3.5)















#############################################################
#############################################################
#Visualize model coefficients with broom

library(broom)
library(ggplot2)

a <- 5
b <- 3
x1 <- rnorm(1000, 40, 25)
x2 <- rep(seq(1:100), 10)
n_over <- sum()
# x2[x2>70] <- rnorm()
e <- rnorm(1000, 5, 10)

y <-a*x1 - b*x2 + e
data <- data.frame(y,x1,x2)
data
summary(data)

Linear_model<-ml<- lm(y ~ . , data)
summary(Linear_model)
tidy(Linear_model)
Linear_model$lm$coef

summary(ml)
summary(ml)$coefficients

coef<- tidy(ml, conf.int =  TRUE)
coef

#############  GRAPH OUTPUT  ###################
ggplot(coef, aes(term, estimate))+
  geom_point()+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  labs(title = "Coefficients of a linear regression model")
#########################################

coef_my <-tidy()
######################
#https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html

