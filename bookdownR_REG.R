
#https://bookdown.org/carillitony/bailey/chp11.html


#PACKAGES

library(tidyverse)
library(broom)
library(masteringmetrics)
library(devtools)

install.packages("Rcpp")
if (!require('devtools')) install.packages('devtools')
devtools::install_github( "bquast/rddtools" )

library(rddtools)

#DATA

data("mlda", package = "masteringmetrics")
head(mlda)
#load("Data/mlda.rda")

mlda %>% 
  ggplot(aes(x = agecell, y = mva)) + 
  geom_point() +
  geom_vline(xintercept = 21) + 
  labs(y = "Deaths in Moving Vehicle Accidents", x = "Age")

mlda %>% 
  mutate(D = ifelse(agecell >= 21, 1, 0)) %$% 
  lm(mva ~ D + I(agecell - 21)) %>% 
  summary()

mlda %>% 
  select(agecell, mva) %>% 
  mutate(D = as.factor(ifelse(agecell >= 21, 1, 0))) %>% 
  ggplot(aes(x = agecell, y = mva)) +
  geom_point(aes(color = D)) + 
  geom_smooth(method = "lm")

mlda %>% 
  select(agecell, mva) %>% 
  mutate(D = as.factor(ifelse(agecell >= 21, 1, 0))) %>% 
  ggplot(aes(x = agecell, y = mva, color = D)) +
  geom_point() + 
  geom_smooth(method = "lm")

rdd_data(mlda$mva, mlda$agecell, cutpoint = 21) %>% 
  rdd_reg_lm(slope = "same") %>% 
  summary()

rdd_data(mlda$mva, mlda$agecell, cutpoint = 21) %>% 
  rdd_reg_lm(slope = "separate") %>% 
  summary()

rdd_data(mlda$mva, mlda$agecell, cutpoint = 21) %>% 
  rdd_reg_lm(slope = "same") %>% 
  plot()

rdd_data(mlda$mva, mlda$agecell, cutpoint = 21) %>% 
  rdd_reg_lm(slope = "separate") %>% 
  plot()



