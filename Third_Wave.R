#Third Wave

# see tab 13.2

options(scipen =  999)

library(masteringmetrics)
library(devtools)
library(ggplot2)
library(tidyverse)

##################################################
#                   SAY NO TO ALL UPDATES      #                       
##################################################
install.packages("Rcpp")

if (!require('devtools')) install.packages('devtools')

devtools::install_github( "bquast/rddtools" )

library(rddtools)

# DATA
National_Income<-read_csv("National_Income.csv")
Total_population<- read_csv("total_population.csv")

######################################################
#                     DATA PREP                      #
######################################################
dim(National_Income) #58  6
dim(Total_population) #59  6

#Data frames need to have teh same # of rows
#So we will drop the last row from Tot_Pop
#slice(test_df, 1:(n()-5))
Total_population<-slice(Total_population, 1:(n()-1))
dim(Total_population) #59  6

pop_inc<- inner_join(National_Income , Total_population , by = "year_number")
View(pop_inc)  


# We only have income data from 1970 on so lets filter out any rows with prioer dates.
pop_inc_2<- filter(pop_inc, year_number > 1969)
View(pop_inc_2)


########################################################
#                   Graphs                             #
########################################################
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

# Initiate a ggplot
b <- ggplot(pop_inc_2, aes(x = pop_inc_2$pop_growth_rate_pct, y = pop_inc_2$Growth_rate_pct))

# Basic scatter plot
b + geom_point()

# Change color, shape and size
b + geom_point(color = "red", size = 2, shape = 21, fill = "red") +
  ggtitle("Pop Growth Rate Pct Over Income Pct Growth Rate") +
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method='lm')

b + geom_point(color = "red", size = 2, shape = 21, fill = "red") +
  ggtitle("Pop Growth Rate Pct Over Income Pct Growth Rate") +
  geom_smooth(method='lm')

########################################################
#                     Regressions                      #
########################################################






