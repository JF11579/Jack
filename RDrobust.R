#https://rdrr.io/cran/rdrobust/man/rdplot.html




library(rdrobust)
library(Hmisc)


x<-runif(1000,-1,1)  #generate a column (aka a VECTOR) of numbers btwn -1 and 1

y<-5+3*x+2*(x>=0)+rnorm(1000) # based on x but keeping only postive numbers.
rdplot(y,x) 

head(x)
describe(x)
head(y)
describe(y)


  #################
# I tried running this , plugging in year for X
#But it does not work with dates.
#So I converted it to numeric but it still cannot handle that.  No biggy.
# I will add a column of numbers starting with 1 for the earliest date and so on
# and use these numbers for X looking up what number 1/1/1978 got 
head(Nat_Inc)
x<- Nat_Inc$year
y<- 5+3*x+2*(x>=1978)
rdplot(y,x) 

Nat_Inc_2 <- Nat_Inc
Nat_Inc_2<-Nat_Inc_2  %>%
      mutate(observation = 1:n())
#Adjusted net national income (current US$)	1978-04-29	127722875535	9
head(Nat_Inc_2)

write_csv(Nat_Inc_2, "Nat_Inc_2.csv")
X_Date
head(X_Date)

View(Nat_Inc_2)

Nat_Inc_3<-Nat_Inc_2

Nat_Inc_3%>%
  mutate(scall = )

head(Nat_Inc_2)
x<- Nat_Inc_2$observation
y<- 5+3*x+2*(x>=9)
rdplot(y,x) 





s = sort(rexp(100))
s
range01<- function(x){(x-min(x)) / (max(x) -min(x))}
range01(s)

t<- Nat_Inc_2$observation
t
range01<- function(x){(x-min(x)) / (max(x) -min(x))}
A<-range01(t)
A
str(A)

B<- cbind(Nat_Inc_3,A)
View(B)
      




x <- runif(20, -10, 10)  # 20 numbers ranging from -10 to 10
x
rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100
rescale(x)
x

my.df <- data.frame(x=rnorm(100, mean=10), sex=sample(c("M","F"), 100, rep=T))
my.df_2 <- group_by(my.df, sex) %>% mutate(x.sd = as.numeric(scale(x)))
my.df
str(my.df)

A<-scale(my.df$x)
A

View(my.df)
View(my.df_2)
?scale

require(stats)
x <- matrix(1:10, ncol = 2)
(centered.x <- scale(x, scale = FALSE))
cov(centered.scaled.x <- scale(x)) # all 1
(centered.x <- scale(x, scale = TRUE))

x
?sweep

dat <- data.frame(x = rnorm(10, 30, .2), y = runif(10, 3, 5))
scaled.dat <- scale(dat)

dat

scaled.dat





