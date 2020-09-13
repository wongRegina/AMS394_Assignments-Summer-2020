library(ISwR)

filename = "d_logret_6stocks.txt"
table <- read.table(filename,header=T)
attach(table)

result <- t.test(Intel, mu = 0, level =0.05)
result

result2 <- wilcox.test(Intel, mu = 0)
result2

twoSampleT <- t.test(Pfizer, Intel, paired = TRUE)
twoSampleT

twoSampleWil <- wilcox.test(Pfizer, Intel, paired = TRUE)
twoSampleWil


BP26 <- c(152, 157,179,185,178, 149)
shapiro.test(BP26)
BP5 <- c(384, 369, 354, 367, 375, 423)
shapiro.test(BP5)
BPT <-t.test(BP5, BP26, alternative = "greater", var.equal = TRUE)
BPT


corneal <- matrix(c(488,478,480,426, 440, 410, 458, 460,
                    484, 478, 492, 444, 436, 398, 464, 476), nrow=2,byrow=TRUE)
corneal
difference <- corneal[1,] -corneal[2,]
difference
shapiro.test(difference)
cornealT <- t.test(difference, level=0.10)
cornealT

mean <- mean(difference)
mean
sd <- sd(difference)
sd
length <-length(difference)
length
cv <- 1.895 # From t table(two tail, df = sd -1 (7))
error <- cv* (sd/sqrt(length))
error
lowerBound <- mean - error
upperBound <- mean + error
cat("[",lowerBound, "," , upperBound,"]")

marketing <- matrix(c(2.4,1.6, 2.0, 2.6, 1.4, 1.6, 2.0, 2.2, 
                      225, 184, 220, 240, 180, 184, 186, 215), nrow=2,byrow=TRUE)
x <- marketing[1,]
y <- marketing[2,]
x
y
correlation <-cor(x, y)
correlation

LSRE <- lm(y~ x)
coefficient<- coef(lm(y~x))[[2]]
 Intercept<- coef(lm(y~x))[[1]]
expectedCost <- 1.8 
expectedValue <- coefficient * expectedCost + Intercept
expectedValue * 1000

predict(LSRE,newdata=data.frame(x=1.8))



filename = "C:/Users/wongr/Desktop/AMS 394/d_logret_6stocks.txt"
table <- read.table(filename,header=T)
attach(table)
regressCoe <-lm(Intel~Citigroup)
regressCoe
coef(regressCoe)
regressWoCoe <- lm(Intel~-1 + Citigroup)
regressWoCoe
coef(regressWoCoe)

cor(Intel, Citigroup)
cor.test(Intel, Citigroup)

fit <- lm(Pfizer ~ Exxon + Citigroup)
fit
fit <- lm(Pfizer ~ -1 + Exxon + Citigroup)
fit 
attach(rmr)
plot(body.weight,metabolic.rate, main="Metabolic Rate vs Body Weight", 
     ylab = "Metabolic Rate(kcal/24h)", xlab = " Body Weight(kg)")
abline(lm(metabolic.rate~body.weight))
fit <- lm(metabolic.rate ~ body.weight, data = rmr)
fit
predict(fit,newdata=data.frame(body.weight=80))


