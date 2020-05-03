
#Fitted_Lines
# https://aosmith.rbind.io/2018/11/16/plot-fitted-lines/

#https://stats.stackexchange.com/questions/30975/how-to-add-non-linear-trend-line-to-a-scatter-plot-in-r

n <- 100
x <- seq(n)
y <- rnorm(n, 50 + 30 * x^(-0.2), 1)
Data <- data.frame(x, y)

head(Data,3)

plot( y ~ x , Data)

# fit a loess line
loess_fit <- loess(y ~ x, Data)
lines(Data$x, predict(loess_fit), col = "blue")

# fit a non-linear regression
nls_fit <- nls(y ~ a + b * x^(-c), Data, start = list(a = 80, b = 20, 
                                                      c = 0.2))
lines(Data$x, predict(nls_fit), col = "red")

###############
# http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/
library(tidyverse)
library(ggpubr)
library(datarium)
theme_set(theme_pubr())

# Load the package
data("marketing", package = "datarium")
head(marketing, 4)

ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point() +
  stat_smooth()


# OLS REGRESSIONEXAMPLE
#https://www.r-bloggers.com/ordinary-least-squares-ols-linear-regression-in-r/

########
df <- data.frame(x = c(1:100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)
my.formula <- y ~ x
p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_point()
p

head(pop_inc_2)
my.formula <- y ~ x

p <- ggplot(data = pop_inc_2, aes(x = pop_inc_2$Growth_rate_pct, y = pop_inc_2$pop_growth_rate_pct)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_point()
p

###########################
 #2 Y axis
#https://www.zstat.pl/2018/07/19/ggplot2-with-2-y-axes/
library(ggplot2)
library(dplyr)
library(ggpubr) # ggarrange
library(TTR) # ROC function
library(lubridate) # ymd

# calculate roc
roc <- ROC(BJsales, type = "discrete")

# prepare some fake dates for the x-axis
dates <- ymd("1970-01-01") + months(seq_along(roc))

# final dataset
salesData <- data_frame(date = dates, sales = as.numeric(BJsales), roc = as.numeric(roc))

head(salesData)

pSales <- ggplot(salesData, aes(x = date, y = sales)) + geom_line()
pSales

pRoc <- ggplot(salesData, aes(x = date, y = roc)) + geom_line(color = "red")
pRoc

ggarrange(pSales, pRoc)

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

rescaledVals <- calc_fudge_axis(salesData$sales, y2 = salesData$roc)

# check if the ranges are equal
range(rescaledVals$yf, na.rm = TRUE) == range(salesData$sales, na.rm = TRUE)

salesData <- salesData %>% mutate(rocScaled = rescaledVals$yf)

pFinal <- ggplot(salesData) + 
  geom_line(aes(x = date, y = sales)) + 
  geom_line(aes(x = date, y = rocScaled), color = "red") + 
  scale_y_continuous(
    sec.axis = dup_axis(breaks = rescaledVals$breaks, labels = paste0(rescaledVals$labels * 100, "%"), name = "ROC")
  )

# Compare final plot wiht sales plot
ggarrange(pFinal, pSales)

# Compare final plot wiht roc plot
ggarrange(pFinal, pRoc)

# add horizontal line to the second axis
pFinal + geom_hline(yintercept = rescaledVals$zero, linetype = "dashed")



