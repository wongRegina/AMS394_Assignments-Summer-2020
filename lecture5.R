# import csv data file
getwd()   #  setwd() to change current directory
data <- read.csv("Galton.csv")
data
y <- data$Height
x1 <- data$Father
x2 <- data$Mother
x3 <- as.numeric(data$Gender)-1  


# multiple linear regression
mod <- lm(y ~ x1+x2+x3)
summary(mod)

# obtain confidence intervals for model parameters
confint(mod,level=0.95)   
#confint(mod,conf.level=0.95)


# check model goodness of fit
par(mfrow=c(2,2))
plot(mod)


# general linear model: glm()
x3 <- data$Gender
mod1 <- glm(y~x1+x2+factor(x3))
summary(mod1)

x3<-relevel(factor(x3),ref="M")
mod1<-glm(y~x1+x2+factor(x3))
summary(mod1)


# install.packages("ISLR")
library(ISLR)
summary(Hitters)

# There are some missing values here, so before we proceed we will remove them:
Hitters=na.omit(Hitters)
names(Hitters)


# best subset regression
# install.packages("leaps")
library(leaps)
regfit.full=regsubsets(Salary~.,data=Hitters) # full model: lm(Salary~.,data=Hitters)
summary(regfit.full)

#  It gives by default best-subsets up to size 8; lets increase that to 19, i.e. all the variables
regfit.full=regsubsets(Salary~.,data=Hitters, nvmax=19)
reg.summary=summary(regfit.full)
summary(regfit.full)
names(reg.summary)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],pch=20,col="red")

# There is a plot method for the `regsubsets`  object
plot(regfit.full,scale="Cp")  # Cp can be replaced by AIC/BIC
coef(regfit.full,10)

# forward stepwise selection
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
plot(regfit.fwd,scale="Cp")


# Ridge Regression and the Lasso
# If the package does not contain the model formula language, like `glmnet`, we can set up an `x` and `y` by hand.
library(glmnet)
x=model.matrix(Salary~.-1,data=Hitters) 
y=Hitters$Salary

# First we will fit a ridge-regression model. This is achieved by calling `glmnet` with `alpha=0` (see the helpfile). There is also a `cv.glmnet` function which will do the cross-validation for us. 
fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)

# Now we fit a lasso model; for this we use the default `alpha=1`
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)

# Suppose we want to use our earlier train/validation division to select the `lambda` for the lasso.
lasso.tr=glmnet(x[train,],y[train])
lasso.tr
pred=predict(lasso.tr,x[-train,])
dim(pred)
rmse= sqrt(apply((y[-train]-pred)^2,2,mean))
plot(log(lasso.tr$lambda),rmse,type="b",xlab="Log(lambda)")
lam.best=lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr,s=lam.best)












logret <- read.table("d_logret_6stocks.txt",header=T)
logret
dim(logret)
names(logret)
logret[1:10,-c(2,4)]
levels(logret$Date)
subset.1  <-  logret[logret[,2]>0.01,seq(1,5)]
plot(logret$Pfizer, logret$Intel)
