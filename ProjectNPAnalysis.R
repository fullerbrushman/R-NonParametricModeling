communities <- read.csv("C:\\Users\\Mark Fuller\\Downloads\\communities.csv", header=TRUE)
##or equivalent of where you stored/downloaded the csv file
data=data.frame(agePct12t21=communities$agePct12t21,agePct12t29=communities$agePct12t29,agePct65up=communities$agePct65up,pctUrban=communities$pctUrban,PctPopUnderPov=communities$PctPopUnderPov,violent.crimes=communities$ViolentCrimesPerPop)

library(fANCOVA)
# load np package (for npreg function)
if(!require(np)){
  install.packages("np")
  library("np")
}
library('bigsplines')

############ Just Age

#### Smoothing Spline Model ####
x <- data$agePct12t29
y <- data$violent.crimes
plot(x,y)
points(smooth.spline(x,y), type='l', col='forestgreen')
####  ####

#### Local Averaging Model ####
locavg <- supsmu(data$agePct12t29, data$violent.crimes)

points(locavg$x, locavg$y, type='l', col='blue')
####  ####


#### Local Regression Model ####
locreg=loess(violent.crimes~agePct12t29, data=data, criterion='gcv')
summary(locreg)

xnew <- seq(min(data$agePct12t29), max(data$agePct12t29), length=100)
points(xnew, predict(locreg, as.vector(xnew)), type='l', col='red')
####  ####

#### Kernel Regression Model ####
bw <- npregbw(formula=data$violent.crimes~data$agePct12t29)
kerreg <- npreg(bws=bw, txdat=data$agePct12t29, tydat=data$violent.crimes)

points(sort(data$agePct12t29), fitted(kerreg), type='l', col='purple')
####  ####

##################################

############ With Covariates

library('mgcv')

#### Smoothing Spline Model ####
# Is basically adding smoothers together for each of the covariates
gamfit <- gam(violent.crimes ~ s(agePct12t29) + s(pctUrban) + s(PctPopUnderPov), data=data)
summary(gamfit)

plot(data$agePct12t29, data$violent.crimes, main="Generalized Additive Smoothing Model", xlab='% 12-29 years old', ylab='violent crimes per 100,000 population')
xnew <- seq(min(data$agePct12t29), max(data$agePct12t29), length=100)
x1new <- seq(min(data$pctUrban), max(data$pctUrban), length=100)
x2new <- seq(min(data$PctPopUnderPov), max(data$PctPopUnderPov), length=100)
newdata=data.frame(agePct12t29=as.vector(xnew), pctUrban=as.vector(x1new), PctPopUnderPov=as.vector(x2new))
points(xnew, predict(gamfit, newdata=newdata), type='l', col='forestgreen', lwd=2)
####  ####

plot(data$agePct12t29, data$violent.crimes, main="Both Models with ", xlab='% 12-29 years old', ylab='violent crimes per 100,000 population')

#### Local Regression Model ####
plot(data$agePct12t29, data$violent.crimes, main="Adjusted Local Regression Model", xlab='% 12-29 years old', ylab='violent crimes per 100,000 population')
locreg=loess(violent.crimes~agePct12t29+pctUrban+PctPopUnderPov, data=data, criterion='gcv')
summary(locreg)

xnew <- seq(min(data$agePct12t29), max(data$agePct12t29), length=100)
x1new <- seq(min(data$pctUrban), max(data$pctUrban), length=100)
x2new <- seq(min(data$PctPopUnderPov), max(data$PctPopUnderPov), length=100)
newdata=data.frame(agePct12t29=as.vector(xnew), pctUrban=as.vector(x1new), PctPopUnderPov=as.vector(x2new))
points(xnew, predict(locreg, newdata=newdata), type='l', col='red', lwd=2)
####  ####

#### Kernel Regression Model ? ####
bw <- npregbw(formula=data$violent.crimes~data$agePct12t29+data$pctUrban+data$PctPopUnderPov)
kerreg <- npreg(bws=bw, gradients = TRUE)

points(sort(data$agePct12t29), fitted(kerreg), type='l', col='purple')
####  ####

##################################

cor(data$pctUrban, data$agePct12t29)
cor(data$PctPopUnderPov, data$agePct12t29)

hist(data$violent.crimes)
hist(data$agePct12t29)
hist(data$pctUrban)
hist(data$PctPopUnderPov)





