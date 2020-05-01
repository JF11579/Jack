#
#https://bookdown.org/carillitony/bailey/chp11.html


getwd()

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


#Data
#Adjusted net national income (current US$)
#Subset skinny data to get year and income
#skinny_data<- read_csv("skinny_data.csv")
data<- read_csv("Nat_Inc_2.csv")

str(data)

'''
mlda %>% 
  ggplot(aes(x = agecell, y = mva)) + 
  geom_point() +
  geom_vline(xintercept = 21) + 
  labs(y = "Deaths in Moving Vehicle Accidents", x = "Age")
'''

mlda %>% 
  ggplot(aes(x = agecell, y = mva)) + 
  geom_point() +
  geom_vline(xintercept = 21) + 
  labs(y = "Deaths in Moving Vehicle Accidents", x = "Age")


data%>%
  ggplot(aes(x= observation , y = value)) +
  geom_point()+
  geom_vline(xintercept = 9)+
  labs(y= "Year" , x = "Gross Nat Income")

'''
13.1 Same slope
To estimate an RD model where the slope is the same 
before and after the cutoff value make use of 
the ifelse call in R. ifelse returns one value if the test
condition holds and another when it doesn’t. For example 
suppose the we create a variable, T that takes on the value 1 
when another variable say X is greater than 10. Create T 
with the call T -> ifelse(X > 10, 1, 0)
'''

#glimpse(data)
str(data)
is.data.frame(data)
data <-date.frame(data)

        data%>%
        mutate(D = ifelse(observation >= 9, 1, 0)) %>%
        lm( value ~ D + I(observation - 9)) %>%
        summary()

        data%>%
          mutate(D = ifelse(observation >= 9, 1, 0))
is.data.frame(data)

A<-data %>%
  lm(observation ~ D +I(observation -9))
  



################## Junk Below  #################3
dat <- data.frame(x = rnorm(10000, 4, 3),
                  y = rnorm(10000, 2, 2))

str(dat)

dat %>%
  lm(y ~ x,.) %>%
  summary()

