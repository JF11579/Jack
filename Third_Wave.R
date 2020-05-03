#Third Wave

# see tab 13.2

options(scipen =  999)

library(masteringmetrics)
library(devtools)
library(ggplot2)
library(tidyverse)
library(ggpmisc)
library(lubridate)

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
#
pop_inc_2<- read_csv("pop_inc_2.csv")


######################################################
#                     DATA PREP                      #
######################################################
dim(National_Income) #58  6
dim(Total_population) #59  6

#Data frames need to have teh same # of rows
#So we will drop the last row from Tot_Pop
#slice(test_df, 1:(n()-5))
#Total_population<-slice(Total_population, 1:(n()-1))
dim(Total_population) #59  6

pop_inc<- inner_join(National_Income , Total_population , by = "year_number")
View(pop_inc)  


# We only have income data from 1970 on so lets filter out any rows with prioer dates.
pop_inc_2<- filter(pop_inc, year_number > 1969)
View(pop_inc_2)

write_csv(pop_inc_2, "pop_inc_2.csv")


########################################################
#                   Graphs % Models                   #
########################################################
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

########### Pop Growth Rate Pct over Incom Growth Rate Pct
b<-ggplot(pop_inc_2, aes(x = pop_inc_2$pop_growth_rate_pct, y = pop_inc_2$Income_growth_rate_pct))

LM_Pop_Inc <-
  b + geom_point(color = "red", size = 2, shape = 21, fill = "red") +
  ggtitle("Line Type: GEOM_SMOOTH: \nPop Growth Rate Pct Over Income Pct Growth Rate") +
  geom_smooth(method='lm') #, se=FALSE) # or to get 95% conf level drop se
LM_Pop_Inc

my.formula<- lm(pop_inc_2$Income_growth_rate_pct) ~ (pop_inc_2$pop_growth_rate_pct) , data = pop_inc_2)
# my.formula <- y ~ x
C<-ggplot(pop_inc_2, aes(x = pop_inc_2$pop_growth_rate_pct, y = pop_inc_2$Income_growth_rate_pct)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_point()+
ggtitle("IncomeGrowth Over Pop Growth Rate")  
C

# or
Stat_smooth<-
  ggplot(pop_inc_2, aes(x = pop_inc_2$pop_growth_rate_pct, y = pop_inc_2$Income_growth_rate_pct))+
  geom_point(color = "red", size = 2, shape = 21, fill = "red") +
  ggtitle("line type: STAT_SMOOTH \nPop Growth Rate Pct Over Income Pct Growth Rate") +
  stat_smooth()
Stat_smooth 
  
Growth_Income_Rates<-
  ggplot(pop_inc_2, aes(x = pop_inc_2$pop_growth_rate_pct, y = pop_inc_2$Income_growth_rate_pct))+
  geom_point(color = "red", size = 2, shape = 21, fill = "red") +
  ggtitle("line type: STAT_SMOOTH \nPop Growth Rate Pct Over Income Pct Growth Rate") 
Growth_Income_Rates

grid.arrange(Stat_smooth , Growth_Income_Rates )
######

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
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method='lm')

LM_Pop_Inc <-b + geom_point(color = "red", size = 2, shape = 21, fill = "red") +
  ggtitle("Pop Growth Rate Pct Over Income Pct Growth Rate") +
  geom_smooth(method='lm') #, se=FALSE) # or to get 95% conf level drop se
LM_Pop_Inc

#What is the correlation coefficient btwn pop growth and inc growth ?  -0.2849001   
cor( pop_inc_2$pop_growth_rate_pct , pop_inc_2$Growth_rate_pct)

lmodel<- lm(pop_inc_2$pop_growth_rate_pct ~ pop_inc_2$Income_growth_rate_pct)

#The Intercept  and the slope
lmodel$coefficients
#Intercept             Slope
# 1.3700905            -0.0194357 

summary(lmodel)
# And the reverse
cor()
                
summary(lmodel)

plot(lmodel)  # hit RETURN TO STEP THROGH 4 GRAPHS.
#################################################################
#      LM Income Pop Pct on Pop GratePct                        #
#################################################################
#What is the correlation coefficient btwn pop growth and inc growth ?  -0.2849001   
cor(  pop_inc_2$Income_growth_rate_pct , pop_inc_2$pop_growth_rate_pct )


lmodel<- lm( pop_inc_2$Income_growth_rate_pct  ~ pop_inc_2$pop_growth_rate_pct   )

#The Intercept  and the slope
lmodel$coefficients
#Intercept             Slope
# 15.444791            -4.176237 

cor(pop_inc_2$Income_growth_rate_pct ,  pop_inc_2$pop_growth_rate_pct )  # -0.2849001

summary(lmodel)




#################################################################
#        Double Y-Axis Pop&Income Growth                        #
#################################################################

#https://www.zstat.pl/2018/07/19/ggplot2-with-2-y-axes/

library(ggplot2)
theme_set(
  theme_classic() +
    theme(legend.position = "top")
)

P<-ggplot(pop_inc_2, aes(x = pop_inc_2$year_number, y = pop_inc_2$population)) +
  geom_line()+
  ggtitle("Population")  
P

I<- ggplot(pop_inc_2, aes(x = pop_inc_2$year_number, y = pop_inc_2$national_income)) +
  geom_line()+
  ggtitle("Income") 
I

grid.arrange(P , I, ncol=2)

##
calc_fudge_axis = function(y1, y2) {
  
  ylim1 <- range(y1, na.rm = TRUE)
  ylim2 <- range(y2, na.rm = TRUE)
  
  mult <- (ylim1[2] - ylim1[1]) / (ylim2[2] - ylim2[1])
  miny1 <- ylim1[1]
  miny2 <- ylim2[1]
  
  cast_to_y1 = function(x) {
    (mult * (x - miny2)) + miny1
  }
  
  yf <- cast_to_y1(y2)
  
  labelsyf <- pretty(y2)
  return(
    list(
      yf = yf,
      labels = labelsyf,
      breaks = cast_to_y1(labelsyf),
      zero = cast_to_y1(0)
    ))
}


#rescaledVals <- calc_fudge_axis(salesData$sales, y2 = salesData$roc)

##
# skipped code
#

pFinal <- ggplot() + 
  geom_line(aes(x = date, y = sales)) + 
  geom_line(aes(x = date, y = rocScaled), color = "red") + 
  scale_y_continuous(
    sec.axis = dup_axis(breaks = rescaledVals$breaks, labels = paste0(rescaledVals$labels * 100, "%"), name = "ROC")
  )


########################################################################
#              Population and  Income Actual Numbers                   #
########################################################################

'''
b<-ggplot(pop_inc_2, aes(x = pop_inc_2$pop_growth_rate_pct, y = pop_inc_2$Income_growth_rate_pct))

LM_Pop_Inc <-
  b + geom_point(color = "red", size = 2, shape = 21, fill = "red") +
  ggtitle("Line Type: GEOM_SMOOTH: \nPop Growth Rate Pct Over Income Pct Growth Rate") +
  geom_smooth(method='lm') #, se=FALSE) # or to get 95% conf level drop se
LM_Pop_Inc
'''
C<- ggplot(pop_inc_2, aes(y = pop_inc_2$population , x = pop_inc_2$national_income )) +
    geom_point(color="red", size = 2 , fill = "red") +
    ggtitle("Population and Income")
C

C2<- ggplot(pop_inc_2, aes(y = pop_inc_2$population , x = pop_inc_2$national_income )) +
  geom_point(color="red", size = 2 , fill = "red") +
  labs(x= "National Income", y= "Population")+
  theme( plot.title = element_text(hjust = 0.5))+
  ggtitle("Population and Income")
C2

#labs(x= "Year", y= "Fertility Rate")+
#theme( plot.title = element_text(hjust = 0.5))+

#What is correlation btwn growth rate pct and inc growth rate pct?
cor( pop_inc_2$pop_growth_rate_pct , pop_inc_2$Income_growth_rate_pct)
# -0.2849001
##################################################################
#   Line Graph Pop  1960 to 2017                                 #
##################################################################

tot_pop<- skinny_data %>%
  filter(variable == 'Population, total')
View(tot_pop)
str(tot_pop)
tot_pop$value <- as.numeric(tot_pop$value)

write_csv(tot_pop, "Population_from_1960.csv")


##############      MODEL CHART ####################
#https://www.datanovia.com/en/blog/ggplot-title-subtitle-and-caption/

Population_graph <- ggplot(tot_pop ,aes(x= year , y= value))+
  geom_line(color = "red")+
  labs(x= "Year", y= "Population")+
  theme( plot.title = element_text(hjust = 0.5))+
  ggtitle("Population")
Population_graph

# Population Grown 1960 as Pct
head(total_population)
View(total_population)
 total_population<- read_csv("total_population.csv")

 Population_Pct_graph <- ggplot(total_population ,aes(x= year , y= pop_growth_rate_pct ))+
   geom_line(color = "red")+
   labs(x= "Year", y= "Population Growth Rate (Pct)")+
   theme( plot.title = element_text(hjust = 0.5))+
   ggtitle("Population Growth Rate Pct")
 Population_Pct_graph
 
 
 #####################################
 #      FERTILITY RATES
 ####################################
 #Fertility rate, total (births per woman)
 '''
 tot_pop<- skinny_data %>%
  filter(variable == 'Population, total')
 '''
 
 Fertility<- skinny_data %>%
   filter(variable == 'Fertility rate, total (births per woman)')
Fertility          
 
write_csv(Fertility, "Fertility.csv")

Fertility<- read_csv("Fertility.csv")
 head(Fertility)

 Fertility$Pct_change[is.na(Fertility$Pct_change)] <- 0
str(Fertility)

Fertility_Rates_All_Years<-
  ggplot(Fertility,aes(x= year, y = value))+
  geom_line(color = "red")+
  labs(x= "Year", y= "Fertility Rate")+
  theme( plot.title = element_text(hjust = 0.5))+
  ggtitle("Fertility Rates")
Fertility_Rates_All_Years

#labs(x= "Year", y= "Population")+
  #theme( plot.title = element_text(hjust = 0.5))+
#

Fertility_Rates_All_Years_As_Pct<-
  ggplot(Fertility,aes(x= year, y = Pct_change))+
  geom_line(color = "red")+
  ggtitle("Fertility Rates AS Pct")+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=2)
Fertility_Rates_All_Years_As_Pct
 
 grid.arrange(Fertility_Rates_All_Years , Fertility_Rates_All_Years_As_Pct , ncol=2 )
 
 #################################
 #   FERTILITY 1969 - 2017
 #################################
 Fertility_until_2017<- 
            filter(Fertility, year < 2018)
 Fertility_until_2017
 View(Fertility_until_2017)
 
 Fertility_Rates_To_2018<-
   ggplot(Fertility,aes(x= year, y = value))+
   geom_line(color = "red")+
   ggtitle("Fertility Rates 1960 through 2017")
 Fertility_Rates_To_2018
 
 Fertility_Rates_To_2018_As_Pct<-
   ggplot(Fertility,aes(x= year, y = Pct_change))+
   geom_line(color = "red")+
   ggtitle("Fertility Rates as Pct 1960 through 2017")+ geom_hline(yintercept=0, linetype="dashed", 
                                                                   color = "black", size=2)
 Fertility_Rates_To_2018_As_Pct
 
  grid.arrange( Fertility_Rates_To_2018 ,  Fertility_Rates_To_2018_As_Pct)
 '''
  Population_Pct_graph <- ggplot(total_population ,aes(x= year , y= pop_growth_rate_pct ))+
   geom_line(color = "red")+
   ggtitle("Population Growth Rate Pct")
 Population_Pct_graph
 '''
 
'''
lmodel<- lm(pop_inc_2$pop_growth_rate_pct) ~ pop_inc_2$Growth_rate_pct) , data = pop_inc_2)

#The Intercept  and the slope
lmodel$coefficients
#Intercept             Slope
#  1.27650150        -0.07513535 

# And the reverse
cor()
                
summary(lmodel)

plot(lmodel)  # hit RETURN TO STEP THROGH 4 GRAPHS.
'''



'''
#What is the correlation coefficient btwn pop growth and inc growth ?  -0.2849001   
cor( pop_inc_2$pop_growth_rate_pct , pop_inc_2$Growth_rate_pct)

lmodel<- lm(pop_inc_2$pop_growth_rate_pct) ~ pop_inc_2$Growth_rate_pct) , data = pop_inc_2)

#The Intercept  and the slope
lmodel$coefficients
#Intercept             Slope
#  1.27650150        -0.07513535 
'''
