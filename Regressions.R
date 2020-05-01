

# see tab 13.2

options(scipen =  999)

library(masteringmetrics)
library(devtools)

##################################################
#                   SAY NO TO ALL UPDATES      #                       
##################################################
install.packages("Rcpp")

if (!require('devtools')) install.packages('devtools')

devtools::install_github( "bquast/rddtools" )

library(rddtools)

###################################################
##################################################
# TEST
mlda<- read_csv("mlda.csv")
mlda %>% 
  mutate(D = ifelse(agecell >= 21, 1, 0)) %$% 
  lm(mva ~ D * I(agecell - 21)) %>% 
  summary()

#data %>%
  mutate(D = ifelse(number_year >= 1978 , 1, 0)) %>%
  lm(formula = value ~ D*I(number_year - 0)) %>%
  summary()

##########################################################
#  DISCONTINUOUS REGRESSION WITH AN INTERACTIVE TERM     #
#    1979                                                #
#     NATIONAL INCOME GROWTH                         #
##########################################################

head(national_income, 3)
  
national_income   %>% 
    mutate(D = ifelse(year_number >= 1979, 1, 0)) %$% 
    lm(value ~ D * I(year_number - 1979)) %>% 
    summary()  



##########################################################
#  DISCONTINUOUS REGRESSION WITH AN INTERACTIVE TERM     #
#                  1988                                  #
#     NATIONAL INCOME GROWTH                         #
##########################################################

head(national_income, 3)

national_income   %>% 
  mutate(D = ifelse(year_number >= 1988, 1, 0)) %$% 
  lm(value ~ D * I(year_number - 1988)) %>% 
  summary()  



##########################################################
#  DISCONTINUOUS REGRESSION WITH AN INTERACTIVE TERM     #
#                  1979                                  #
#     NATIONAL INCOME GROWTH RATE Pct                    #
##########################################################

head(national_income, 3)

national_income   %>% 
  mutate(D = ifelse(year_number >= 1979, 1, 0)) %$% 
  lm(national_income$Growth_rate_pct ~ D * I(year_number - 1979)) %>% 
  summary()


##########################################################
#  DISCONTINUOUS REGRESSION WITH AN INTERACTIVE TERM     #
#                  1988                                  #
#     NATIONAL INCOME GROWTH RATE Pct                    #
##########################################################

head(national_income, 3)

national_income   %>% 
  mutate(D = ifelse(year_number >= 1988, 1, 0)) %$% 
  lm(national_income$Growth_rate_pct ~ D * I(year_number - 1988)) %>% 
  summary()

###############################################################################
#                 POPULTION                                                   #
###############################################################################
head(total_pop)


##########################################################
#  DISCONTINUOUS REGRESSION WITH AN INTERACTIVE TERM     #
#                  1979                                  #
#     Population GROWTH RATE Pct                         #
##########################################################

total_pop   %>% 
  mutate(D = ifelse(year_number >= 1979, 1, 0)) %$% 
  lm(total_pop$pop_growth_rate_pct ~ D * I(total_pop$year_number - 1979)) %>% 
  summary()


##########################################################
#  DISCONTINUOUS REGRESSION WITH AN INTERACTIVE TERM     #
#                  1988                                  #
#     Population GROWTH RATE Pct                         #
##########################################################

total_pop   %>% 
  mutate(D = ifelse(year_number >= 1988, 1, 0)) %$% 
  lm(total_pop$pop_growth_rate_pct ~ D * I(total_pop$year_number - 1988)) %>% 
  summary()




##########################################################
#  Regression of Population and Income Growth            #
#                  1979                                  #
#                                                        #
##########################################################


