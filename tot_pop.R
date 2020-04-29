

#
options(scipen = 999)

#PACKAGES
library(tidyverse)
library(ggplot2)
library(lubridate)


data<- read_csv("skinny_data.csv")
head(data,3)

tot_pop<- skinny_data %>%
  filter(variable == 'Population, total')

head(tot_pop)

# Wrangle Data
#data$year <- as.Date(as.character(data$year) , format = "%Y")



tot_pop$year  <- as.Date(as.character(tot_pop$year) , "%Y")

data$value <- as.numeric(data$value)
tot_pop$value <- as.numeric(tot_pop$value)
str(tot_pop)

specific_years<- tot_pop %>%
        filter(year >= as.Date("1978-04-28") & year<= as.Date("2017-04-28"))
View(specific_years)

#easier to add teh growth rate in a spreadsheet
write_csv(specific_years, "specific_years.csv")

# re-import the spreadheet now that growth rate is added
specific_years<- read_csv("specific_years.csv")
str(specific_years)  

######### FERTILITY RATES  ################
#Fertility rate, total (births per woman)
#tot_pop<- skinny_data %>%
#filter(variable == 'Population, total')

Fertility_rate<- skinny_data %>%
        filter(variable == 'Fertility rate, total (births per woman)')
str(Fertility_rate)

#tot_pop$year  <- as.Date(as.character(tot_pop$year) , "%Y")
Fertility_rate$year <- as.Date(as.character(Fertility_rate$year) , '%Y')
str(Fertility_rate)

Fertility_rate$value <- as.numeric(Fertility_rate$value)
str(Fertility_rate)

Fertility_rate <- Fertility_rate %>%
    filter(year >= as.Date("1978-04-28") & year<= as.Date("2017-04-28"))

############ TOTAL GDP  #########################
#Adjusted net national income (current US$)

#First filter out the income rows
Nat_Inc<- skinny_data %>%
        filter(variable == 'Adjusted net national income (current US$)')

Nat_Inc$year  <- as.Date(as.character(Nat_Inc$year) , "%Y")

Nat_Inc$value <- as.numeric(Nat_Inc$value)

Nat_Inc<- Nat_Inc %>%
      filter(year >= as.Date("1970-04-28") & year<= as.Date("2017-04-28"))

head(Nat_Inc,20)
str(Nat_Inc)

##################################################
#              GRAPHING                          #
##################################################

L1<- ggplot(data= tot_pop , aes(x= year, y = value))+
  geom_line()+
  ggtitle("China's Population Growth 1960 -2020")
L1

L2<- ggplot(data= specific_years , aes(x= year, y = value))+
  geom_line()+
  ggtitle("China's Population Growth 1978 -2017")
L2


L3<- ggplot(data= Fertility_rate , aes(x= year, y = value))+
  geom_line()+
  ggtitle("China's Fertility Rate 1978 -2017")
L3

L4<- ggplot(data= Nat_Inc, aes(x=year, y= value))  +
  geom_line()+
  ggtitle("China's Adjusted National Income 1970 - 2017")
L4

