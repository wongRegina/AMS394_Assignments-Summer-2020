# random sampling
sample(1:40,5)
sample(c("H","T"), 10, replace=T)
sample(c("succ", "fail"), 10, replace=T, prob=c(0.9, 0.1))

# Probability calculations and combinatorics
choose(40,5)

prod(40:36)
40*39*38*37*36
prod(40:36)/prod(1:5)
?factorial
factorial(4)

# densities (continuous r.v.): dnorm, dbeta, dchisq, etc.
x <- seq(-4,4,0.1)
# seq(from,to,by)
plot(x,dnorm(x),type="l")
curve(dnorm(x), from=-4, to=4)

x <- seq(0,1,0.01)
plot(x,dbeta(x,4,5),type="l")
curve(dbeta(x,4,5), from=0, to=1)

x <- seq(0,100,1)
plot(x,dchisq(x,40),type="l")
curve(dchisq(x,40), from=0, to=100)

x <- seq(0,100,1)
plot(x,dgamma(x,30),type="l")
curve(dgamma(x,30), from=0, to=100)

# mass functions (discrete r.v.): dbinom, dpois, etc.
x <- 0:50
plot(x,dbinom(x,size=50,prob=.33),type="h")
#curve(dbinom(x,size=50,prob=.33), from=0, to=50)

x <- 0:50
plot(x,dpois(x,10),type="h")



# cumulative functions: pnorm, pbeta, pchisq, etc.
pnorm(160,mean=132,sd=13)
pbinom(16,size=20,prob=.5)

pbeta(0.8,4,5)
pchisq(100,40)
pchisq(10,40)
pgamma(100,30)
pgamma(10,30)

ppois(2,10)

x <- seq(-4,4,0.1)
plot(x,pnorm(x),type="l")
curve(pnorm(x), from=-4, to=4)

x <- 0:50
plot(x,pbinom(x,size=50,prob=.33))
#curve(pbinom(x,size=50,prob=.33), from=0, to=50)



# quantiles
qnorm(0.5)
qnorm(0.5,1,2)
qnorm(0.025)
xbar=83
sigma=12
n<-5
sem<-sigma/sqrt(n)
sem
xbar+sem*qnorm(0.025)
xbar+sem*qnorm(0.975)
xbar-sem*qnorm(0.025)

qbinom(0.5,size=20,prob=.5)
qbeta(0.5,4,5)
qbeta(1,4,5)

qchisq(0.5,40)
qchisq(1,40)

qgamma(0.5,30)
qgamma(1,30)

qpois(0.5,10)
qpois(1,10)



# random number generators
rnorm(10,mean=7,sd=5)
rbinom(10,size=20,prob=.5)
rbeta(10,4,5)
rchisq(30,40)
rgamma(50,30)
rpois(20,10)



# Summary statistics for a single group
x <- rnorm(50)
mean(x)
sd(x)
var(x)
sd(x)^2 
median(x)
quantile(x)
pvec <- seq(0,1,0.1)
quantile(x,pvec)
quantile(x,c(0.1,0.4))



#	Graphic display of distributions --- Histograms, empirical distributions, Q-Q plot, Boxplot
x<-rnorm(50)
hist(x)

x<-c(1.5,2.5,3.5,4.5,5.5,6.5,8.5,9.5,12.5)
y<-c(5,7,12,2,1,4,14,2,3)
z<-rep(x,y)
brk<-c(0,1,2,3,5,7,9,10,11,13)
hist(z,breaks=brk)


### empirical distribution function
x<-rnorm(100)
n <- length(x)
plot(sort(x),(1:n)/n,type="s",ylim=c(0,1))
plot(sort(x),(1:n)/n,type="l",ylim=c(0,1))
plot(x,pnorm(x))

#qq plot
qqnorm(x)

# BoxPlot
library(ISwR)
data(IgM)
par(mfrow=c(1,2))
boxplot(IgM)
boxplot(log(IgM))
par(mfrow=c(1,1))

par(mfrow=c(1,2))
boxplot(log(IgM))
boxplot(IgM)


par(mfcol=c(2,2))
boxplot(log(IgM))
boxplot(IgM)
boxplot(sin(IgM))
boxplot(cos(IgM))


par(mfrow=c(2,2))
boxplot(log(IgM))
boxplot(IgM)
boxplot(sin(IgM))
boxplot(cos(IgM))



# Summary statistics by groups
data(red.cell.folate)
attach(red.cell.folate)
xbar=tapply(folate,ventilation,mean)
?tapply
s=tapply(folate,ventilation,sd)
n=tapply(folate,ventilation,length)
cbind(mean=xbar,std.dev=s,n=n)


data(juul)
tapply(igf1,tanner,mean)
tapply(igf1,tanner,mean,na.rm=T)




# Graphics for grouped data
# Histograms
data(energy)
names(energy)
attach(energy)
expend.lean<-expend[stature=="lean"]
expend.obese<-expend[stature=="obese"]
par(mfrow=c(2,1))
hist(expend.lean,breaks=10,xlim=c(5,13),ylim=c(0,4),col="white")
hist(expend.obese,breaks=10,xlim=c(5,13),ylim=c(0,4),col="grey")
par(mfrow=c(1,1))

# Parallel boxplot
boxplot(expend~stature)
boxplot(expend.lean , expend.obese)



# tables
caff.marital <- matrix(c(652,1537,598,242,36,46,38,21,218, 327,106,67), nrow=3,byrow=T)
colnames(caff.marital) <- c("0","1-150","151-300",">300")
rownames(caff.marital) <- c("Married","Prev.married","Single")
caff.marital


# graphic plot of tables
# barplot
barplot(prop.table(t(caff.marital)),legend.text=colnames(caff.marital),col=c("white","blue","green","black"))

# dotcharts
dotchart(t(caff.marital))

# pie charts
opar<- par (mfrow=c(2,2), mex=0.8, mar=c(1,1,2,1))
slices<- c("white", "grey80", "grey50", "black")
pie(caff.marital["Married",], main="Married", col=slices)
pie(caff.marital["Prev.married",],   main="Previously married", col=slices)
pie(caff.marital["Single",], main="Single", col=slices)



