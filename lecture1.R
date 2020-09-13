library(ISwR)

# How many seconds in a year?
60*60*24*365

# remainder
56 %% 10

# natural log, log base 10 and log base 2
log(100)
log10(100)
log(100,2)
?log()

# exponential
exp(1)
exp(100)

# power
2 ^ 3

# square root
sqrt(16)

# Define a variable
x = 12.3
x

# R is case sensitive
X

# another way to define a variable
z <- 456.7
z
w < - 456.7 # error due to the space

# Equal is better for assignment

# Define a vector
V = c(1,2,3,4,5)
V
V^3
V * 3
# summation
sum(V)

# mean and standard deviation
mean(V)
sd(V)

v = c(1,3,-5,10,-7)
summary(v)

length(v)

# choose a subvector
1:3 # First index is 1
v[1:3]

v1 = v[which(v>0)] # Like map function
v1

# matrices
m1 = matrix(1:9,nrow=3,ncol=3)
m1
m2 = matrix(1:9,nrow=3,ncol=3,byrow=TRUE)
m2
m1+m2 # need to be the same size

dim(m1)

# element-wise multiplication
m1 * 3
m1 * m2
# matrix multiplication
m1 %*% m2

m3 = matrix(1:6,nrow=3)
m3
m1 * m3
m1 %*% m3

# submatrix
m1[2,2]
m1[1:2,]
m1[c(1,3),2:3]
diag(1:3)

m4 = diag(1:3) + matrix(c(0,1,2,0,0,1,0,0,0),nrow=3)
m4
# tranpose
t(m4)

# matrix inverse
m5 = solve(m4)
m5
m4 %*% m5

# generate a sequence
# seq(from,to,by/length)
x = seq(1,10,by=3)
x
y = seq(1,10,length.out=5)
y

# replicate a vector x
z = rep(1,5)
z
# rep(x,times)

x = c(1,2,3)
y = c(4,5,6)
# row-wise bind
rbind(x,y)
# col-wise bind
cbind(x,y)

v = 1: 5
v
mode(v)

a =  "Hello AMS 394 students :)"
a
mode(a)

b = (v == 2)
b
mode(b)

cars = c("bmw","toyota","hyundai","fort")
# sample with replacement or without replacement
mysample = sample(cars,10,replace=TRUE)
mysample

mysample2 = sample(cars,3,replace=FALSE)
mysample2

# count frequency
table(mysample)

# Help
help(package = "stats")
?runif
help(runif)


# loops and conditional execution
x = runif(100)
x
summary(x)
sum(x[x>0.5 & x<0.8])

y = 0
for(i in 1 : 100){
  if(x[i]>0.5 & x[i]<0.8){
    y = y + x[i]
  }
}
y == sum(x[x>0.5 & x<0.8])

weight = c(60,72,57,90,95,72)
height = c(1.75,1.80,1.65,1.9,1.74,1.91)
bmi = weight/height^2
bmi
sum(bmi)
mean(bmi)
median(bmi)
sd(bmi)
cov(weight,height) # covariance
cor(weight,height) # corelation

cov(weight,height)/(sd(weight)*sd(height))

t.test(bmi,mu=22.5)# t test

# plot graphics
plot(height,weight)
plot(height,weight,pch=2)
hh = seq(1.65,1.90,by=0.05)
lines(hh,22.5*hh^2)
plot(height,weight,pch=2,type='l')

# scatter plot: plot(x,y)
# histogram: hist(x)
# boxplot: boxplot(x)
# add points: points(x,y)
# add lines: lines(x,y)
# add text:  text(x,y,labels)
# add legend: legend(x,y,legend)

x = rnorm(100)
hist(x)
y = c(rnorm(100,0,1),rnorm(100,1,1))
z = rep(c(0,1),each=100)
z
boxplot(y~z,xlab='Z',ylab="y")

x = seq(0,0.5,by=0.1)
y1 = c(0.25,0.715,1.66,3.18,5.08,6.97)
y2 = c(0.25,1.05,2.9,5.6,8.04,9.04)
plot(c(0,0.5),c(0,10),type="n",xlab="Effect Size",ylab="Power",main="Power Comparison")
points(x,y1,pch=2,col="blue")
lines(x,y1,lty=1,col="blue")
points(x,y2,pch=19,col="red")
lines(x,y2,lty=2,col="red")
legend("topleft",legend=c("design1","design2"),pch=c(2,19),lty=c(1,2),col=c("blue","red"))

x = rnorm(100)
hist(x,freq=F)
curve(dnorm(x),add=T)

hist.with.normal = function(x,xlab=deparse(substitute(x)),...){
  h = hist(x,plot=F,...)
  s = sd(x)
  m = mean(x)
  ylim = range(0,h$density,dnorm(0,sd=s))
  hist(x,freq=F,ylim=ylim,xlab=xlab,...)
  curve(dnorm(x,m,s),add=T)
}

hist.with.normal(x)

graph.data = function(x,y){
  plot(x,y,type='n',xlab="",ylab="",axes=F)
  points(x,y)
  axis(2,at=seq(0,2.2,0.3))
  axis(1)
  box()
  title(main='ok')
  title(sub="sub")
}
x = runif(50,0,2)
y = runif(50,0,2)
graph.data(x,y)

y = 12345
x = y/2
while (abs(x^2-y)>0.1){
  x = (x +y/x)/2
}
x
x^2-y

mydat = read.table("filename.format",header=T, sep="\t")
write.table(mydat,file="target directory/filename.format",seq="\t")
