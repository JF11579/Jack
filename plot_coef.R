
#https://api.rpubs.com/robincrlee/broom-coef

#Visualize model coefficients with broom

library(broom)
library(ggplot2)

a <- 5
b <- 3
x1 <- rnorm(1000, 40, 25)
x2 <- rep(seq(1:100), 10)
n_over <- sum()
# x2[x2>70] <- rnorm()
e <- rnorm(1000, 5, 10)

y <-a*x1 - b*x2 + e
data <- data.frame(y,x1,x2)
data
summary(data)
