#https://rdrr.io/cran/rdrobust/man/rdplot.html


library(rdrobust)
library(Hmisc)


x<-runif(1000,-1,1)
y<-5+3*x+2*(x>=0)+rnorm(1000)
rdplot(y,x)

head(x)
describe(x)
