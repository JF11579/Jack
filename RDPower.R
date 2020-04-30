

library(rdpower)
library(Hmisc)
#?rdpower

#References
#M.D. Cattaneo, R. Titiunik and G. Vazquez-Bare. (2019). Power Calculations for Regression Discontinuity Designs. Stata Journal, 19(1): 210-245.
'''
#DEFINITIONS
 TAU: a stat to measure ordinal assoc btw 2 quantities
 Power function: i) Prob of corectly rejecting Null 
ii) prob of avoiding Typw 2 error
    Power of hypoth test is btwn 0 & 1
As Power gets close to 1 it is better at rejecting false hypotheses
Beta usually set a 0.2

'''

'''
RD Power

data is a matrix of Y and R contianing the outcome variable 
(Y ,I suppose, and R  the input, I suppose.)

CUTOFF.  the RD cutoff default is 0.

TAU: specifies the treatment effect under the alternative 
at whichthe power function is evaluated

ALPHA

'''

111) Power is 1 -Beta (Beta is accepting false hypotheses)
#Examples
# Toy dataset
X <- array(rnorm(2000),dim=c(1000,2))
R <- X[,1] + X[,2] + rnorm(1000)
Y <- 1 + R -.5*R^2 + .3*R^3 + (R>=0) + rnorm(1000)
# Power against tau = 1  # TAUa statistic to measure 
                          #the ordinal assc btwn 2 quantities.
tmp <- rdpower(data=cbind(Y,R),tau=1)
# Power against tau = 1 including covariates
tmp <- rdpower(data=cbind(Y,R),tau=1,covs=X)

summary(X)
X

summary(R)
describe(R)
R

Y
describe(Y)


