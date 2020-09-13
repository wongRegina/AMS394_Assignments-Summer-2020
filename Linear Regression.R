library(MASS)
library(ISLR) # install.packages("ISLR") before using it

### simple linear regression
names(Credit)
?Credit
plot(Balance~Income,data = Credit)  # plot(x,y)
# plot(Credit$Balance~Credit$Income)
fit1 = lm(Balance~Income,data=Credit) # lm(y~x,data=)
fit1
summary(fit1)
abline(fit1,col="red")
names(fit1)
confint(fit1)  # confidence interval of estimated coefficients, default 95%
predict(fit1,data.frame(Income=c(50,100,150)),interval="confidence")

### multiple linear regression
fit2 = lm(Balance~Income+Age,data=Credit)  # lm(y~x1+x2+x3+x4,data=)
summary(fit2)
fit3 = lm(Balance~.,Credit)  # full model: all variables lm(y~.,data=)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)  # plot the model residuals, checking error is constant or normally distributed
fit4 = update(fit3,~.-ID-Married)  # remove varibales ID and married
?update
summary(fit4)

### nonlinear terms and interactions
fit5 = lm(Balance~Income+Income*Student,Credit)  # balance = b0 + b1xincome + b2x(income*student)
summary(fit5)
fit6 = lm(Balance~Income + I(Income^2),Credit); summary(fit6) # balance = b0 + b1xincome + b2xincome^2
attach(Credit)
par(mfrow=c(1,1))
plot(Balance~Income)
points(Income,fitted(fit6),col="red",pch=20)
fit7 = lm(Balance~poly(Income,4))   # polynomial of 4th order wrt Income
points(Income,fitted(fit7),col="blue",pch=20)
plot(1:20,1:20,pch=1:20,cex=2)


