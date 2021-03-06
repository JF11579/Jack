

options(scipen = 999)

#PACKAGES
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)

'''
simple version discontinuou regression.
devide the data at before 1978 and 1978 and later.
get slope of each growth rate

'''
data<- read_csv("skinny_data.csv")

income_per_capita<- data%>%
        filter(variable == 'Adjusted net national income per capita (current US$)')

income_per_capita$year <- as.Date(as.character(income_per_capita$year) , "%Y")

income_per_capita$value <- as.numeric(income_per_capita$value)

str(income_per_capita)

Before_1978<- income_per_capita %>%
            filter(year <  as.Date("1978-04-28"))

After_1978<- income_per_capita %>%
            filter(year >= as.Date("1978-04-28"))
After_1978


###################  Graphing ##########################
Before<- ggplot(data = Before_1978, aes(x=year, y= value)) +
  geom_line()+
  ylim(0 , 7000)+
  ggtitle("Pre 1978 Per Capita Income")
Before

Before_2<- ggplot(data = Before_1978, aes(x=year, y= value)) +
  geom_line()+
  ylim(0 , 7000)+
  stat_regline_equation(label.x = 3, label.y = 1000)+
  ggtitle("Pre 1978 Per Capita Income")
Before_2


After<- ggplot(data = After_1978, aes(x=year, y= value)) +
  geom_line()+
  ylim(0 , 7000)+
  ggtitle("Post 1978 Per Capita Income")
After

After_2<- ggplot(data = After_1978, aes(x=year, y= value)) +
  geom_line()+
  ylim(0 , 7000)+
  stat_regline_equation(label.x = 5000, label.y = 1000)+
  ggtitle("Post 1978 Per Capita Income")
After_2

ggarrange(Before , After  , 
        ncol = 2)

ggarrange(Before_2 , After_2  , 
          ncol = 2)


