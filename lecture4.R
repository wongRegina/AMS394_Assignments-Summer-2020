age <- c(19,25,30,42,46,52,57,62,70)
bp <-c(122,125,126,129,130,135,138,142,145)
fit = lm(bp~age)
summary(fit)

# simple linear  regression
library(ISwR)
data(thuesen)
attach(thuesen)
lm(short.velocity~blood.glucose)
summary(lm(short.velocity~blood.glucose))

plot(blood.glucose,short.velocity)
abline(lm(short.velocity~blood.glucose))


# residuals and fitted values
fit <-lm(short.velocity~blood.glucose)
summary(fit)
fitted(fit)
resid(fit)
plot(blood.glucose,short.velocity)
abline(fit)

plot(blood.glucose,short.velocity)
lines(blood.glucose,fitted(fit))

plot(blood.glucose,short.velocity)
lines(blood.glucose[!is.na(short.velocity)],fitted(fit))

blood.glucose
short.velocity
blood.glucose[!is.na(short.velocity) & !is.na(blood.glucose)]
lines(blood.glucose[!is.na(short.velocity) & !is.na(blood.glucose)],fitted(fit))



plot(blood.glucose,short.velocity)
abline(fit)
segments(blood.glucose,fitted(fit),blood.glucose,short.velocity)


bg.na = blood.glucose[!is.na(short.velocity) & !is.na(blood.glucose)]
sv.na = short.velocity[!is.na(short.velocity) & !is.na(blood.glucose)]
plot(bg.na,sv.na)
abline(fit)
segments(bg.na,fitted(fit),bg.na,sv.na)
?segments


plot(fitted(fit),resid(fit))
qqnorm(resid(fit))



# correlation

attach(thuesen)
cor(blood.glucose,short.velocity)
cor(blood.glucose,short.velocity,use="complete.obs")
cor(bg.na,sv.na)
cor(thuesen,use="complete.obs")
cor.test(blood.glucose,short.velocity)

