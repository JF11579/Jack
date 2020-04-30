
#https://bookdown.org/carillitony/bailey/chp11.html

#OPTIONS
options(scipen =  999)

#PACKAGES

library(tidyverse)
library(broom)
library(masteringmetrics)
library(devtools)

##################################################
#                   SAY NO TO ALL UUPDATES      #                       
##################################################
install.packages("Rcpp")

if (!require('devtools')) install.packages('devtools')

devtools::install_github( "bquast/rddtools" )

library(rddtools)

mlda %>% 
  mutate(D = ifelse(agecell >= 21, 1, 0)) %$% 
  lm(mva ~ D * I(agecell - 21)) %>% 
  summary()

mlda_subset<- select(mlda, agecell, mva)
head(mlda_subset)
str(mlda_subset)

mlda_subset %>% 
  mutate(D = ifelse(agecell >= 21, 1, 0)) %$% 
  lm(mva ~ D * I(agecell - 21)) %>% 
  summary()

###############

##################



value<- select(data_subset,value)
str(value)
observations<- select(data_subset, observation)

X<- data.frame(value, observations)
head(X)
str(X)


data_subset<-select(data,value, observation)
head(data_subset)
str(data_subset)

a<- c(10,20,30, 40, 50,60,70,80,90,100)
b<- c(5,10,15,20,25,30,35,40,45,50)
df<- data.frame(a,b)
df
str(df)

df %>%
  mutate(D = ifelse(a >= 20,1, 0))  %>%
  lm(FORMULA = b ~ D*I(a - 30)) %>%
  summary()

data<- read_csv("Nat_Inc_2.csv")
is.data.frame(data)
str(data)

data %>%
    mutate(D = ifelse(observation >= 9,1, 0))  %>%
    lm(formula = value ~ D*I(observation - 9)) %>%
    summary()


###### Plot  13.4.2

#library(rddtools)
rdd_data(mlda$mva, mlda$agecell, cutpoint = 21) %>% 
  rdd_reg_lm(slope = "same") %>% 
  summary()

rdd_data(mlda$mva, mlda$agecell, cutpoint = 21) %>% 
  rdd_reg_lm(slope = "same") %>% 
  plot()

rdd_data(mlda$mva, mlda$agecell, cutpoint = 21) %>% 
  rdd_reg_lm(slope = "separate") %>% 
  plot()




