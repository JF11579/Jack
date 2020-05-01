
'''
Select net national income
       net nat income per capita
       pop total
       popannual growth

'''

#OPTIONS
options(scipen =  999)

#PACKAGES
library(tidyverse)
library(ggplot2)
require(gridExtra)

data<- read_csv("skinny_data.csv")
head(data,3)

tot_pop<- skinny_data %>%
  filter(variable == 'Population, total')
View(tot_pop)

str(tot_pop)
tot_pop$year  <- as.Date(as.character(tot_pop$year) , "%Y")

#easier to add teh growth rate in a spreadsheet
write_csv(tot_pop, "total_population.csv")
#####
total_pop<- read_csv("total_population.csv")
head(total_pop)
str(total_pop)
###############################################################################
################################## GRAPHING ###################################
###############################################################################
theme_set(
  theme_classic() +
    theme(legend.position = "top")
)

Growth_Rate_1<- ggplot(total_pop , aes(x= year_number, y = total_pop$pop_growth_rate))+
            geom_col()+
  geom_vline(xintercept = 1978, linetype="dotted", 
             color = "red", size=1.5)+
          ggtitle("China's Change In Pop Growth Rate Year Over Year")
 
Growth_Rate_1
#
Growth_Rate_2 <-ggplot(total_pop , aes(x= year_number, y = total_pop$pop_growth_rate))+
  geom_col()+
  ggtitle("China's Change In  Pop Growth Rate Year Over Year")+
  geom_vline(xintercept = 1978, linetype="dotted", 
             color = "red", size=1.5)
Growth_Rate_2

####

Population_1<- ggplot(total_pop , aes(x= year, y = total_pop$))+
  geom_col()+
  ggtitle("China's Population")
Population_1
#
Population_3<- ggplot(total_pop , aes(x= year, y = total_pop$pop_growth_rate_pct))+
  geom_col()+
  ggtitle("China's Population Growth Rate As Pct")+
  geom_vline(xintercept = 1978, linetype="dotted", 
             color = "red", size=1.5)
Population_3


###
Population_2<- ggplot(total_pop , aes(x= year, y = total_pop$value))+
  geom_col()+
  ggtitle("China's Population")+
  geom_vline(xintercept = 1978, linetype="dotted", 
             color = "red", size=1.5)
Population_2


grid.arrange( Growth_Rate_1 ,Population_1 , ncol=2)
grid.arrange( Growth_Rate_2 ,Population_2 , ncol=2)

##########################################
#           Net National Income          #
##########################################
nat_income<- skinny_data %>%
  filter(variable == 'Adjusted net national income (current US$)')
View(nat_income)

#easier to add growth rate on a spreadsheet
write_csv( nat_income,"National_Income.csv")

national_income<-read_csv("National_Income.csv")
head(national_income)

Inc_Growth_1<- ggplot(national_income , aes(x= year_number, y = growth_rate))+
  geom_col()+
  ggtitle("China's National Income Growth Rate")+
  geom_vline(xintercept = 1978, linetype="dotted", 
             color = "red", size=1.5)
Inc_Growth_1
#
Inc_Growth_2<- ggplot(national_income , aes(x= year_number, y = value))+
  geom_col()+
  ggtitle("China's National Income")+
  geom_vline(xintercept = 1978, linetype="dotted", 
             color = "red", size=1.5)
Inc_Growth_2

Inc_Growth_3<- ggplot(national_income , aes(x= year_number, y = national_income$Growth_rate_pct))+
  geom_col()+
  ggtitle("China's National Income Growth Rate Pct")+
  geom_vline(xintercept = 1978, linetype="dotted", 
             color = "red", size=1.5)
Inc_Growth_3



grid.arrange( Growth_Rate_2 ,Population_2 , ncol=2)

grid.arrange(Growth_Rate_1 , Population_2 ,Inc_Growth_1 , Inc_Growth_3, ncol=2)

###############################################################################
###########              Linear Regressions                    ################
###############################################################################

