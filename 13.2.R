
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
data<- read_csv("Nat_Inc_2.csv")
is.data.frame(data)
str(data)
##################



'''
data %>%
    mutate(D = ifelse(scale_yr >=0,1,0)) %>%
    lm(formula = value ~ D*I(scale_yr - 0)) %>%
    summary()
'''
#################
##################
data %>%
  mutate(D = ifelse(number_year >= 1978 , 1, 0)) %>%
  lm(formula = value ~ D*I(number_year - 0)) %>%
  summary()
#################
###################
'''
mlda %>% 
  select(agecell, mva) %>% 
  mutate(D = as.factor(ifelse(agecell >= 21, 1, 0))) %>% 
  ggplot(aes(x = agecell, y = mva)) +
  geom_point(aes(color = D)) + 
  geom_smooth(method = "lm")
'''
mlda %>% 
  select(agecell, mva) %>% 
  mutate(D = as.factor(ifelse(agecell >= 21, 1, 0))) %>% 
  ggplot(aes(x = agecell, y = mva)) +
  geom_point(aes(color = D)) + 
  geom_smooth(method = "lm")

'''
data %>%
  mutate(D = ifelse(number_year >= 1978 , 1, 0)) %>%
  lm(formula = value ~ D*I(number_year - 0)) %>%
  summary()
'''
write_csv(data, "data.csv")
# simple regression
data %>%
  mutate(D = ifelse(number_year >= 1978 , 1, 0)) %>%
  lm(formula = value ~ D*I(number_year - 0)) %>%
  summary()

'''
data %>%
    select(number_year, value) %>%
  mutate(D = as.factor(ifelse(number_year >= 1978, 1 , 0)) %>% 
  ggplot(aes(x = number_year, y = value)) +
  geom_point(aes(color = D)) + 
  geom_smooth(method = "lm")
'''



#############  GRAPH OUTPUT  ###################
#https://stackoverflow.com/questions/28029922/linear-regression-and-storing-results-in-data-frame
#Stack how to plot regression output





########


  data %>%
    select(number_year, value) %>%
    mutate(D = as.factor(ifelse(number_year >= 1978, 1 , 0)) %>%
             ggplot(aes(x = number_year, y = value)) +
             geom_line(aes(color = D)) + 
             geom_smooth(method = "lm")













#data %>%
  mutate(D = ifelse(observation >= 9,1, 0))  %>%
  lm(formula = value ~ D*I(observation - 9)) %>%
  summary()

#data %>%
  mutate(D = ifelse(observation >= 9,1, 0))  %>%
  lm(formula = value ~ D*I(observation - 9)) %>%
  summary()

'''
mlda %>% 
  mutate(D = ifelse(agecell >= 21, 1, 0)) %$% 
  lm(mva ~ D * I(agecell - 21)) %>% 
  summary()
'''
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




