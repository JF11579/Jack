

#DATA: https://docs.google.com/spreadsheets/d/12Acq8OckmWl0vkzUaOBuekAylBpcicVPtFhYTk8irOs/edit#gid=123817820

getwd()


options(scipen = 999)

#PACKAGES
library(tidyverse)
library(ggplot2)
library(lubridate)


data<- read_csv("skinny_data.csv")
head(data,3)

# Wrangle Data
data$year <- as.Date(as.character(data$year) , format = "%Y")
str(data)

data$value <- as.numeric(data$value)
str(data)










