
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







