x <- c(202.2,203.4,200.5,202.5,206.3,198.0,203.7,200.8,201.3,199.0)
d <- x - 200
wilcox.test(d)
?wilcox.test
wilcox.test(x,mu=200)


drug <- c(6.9,7.6,7.3,7.6,6.8,7.2,8.0,5.5,5.8,7.3,8.2,6.9,6.8,5.7,8.6)
placebo <- c(6.4,6.7,5.4,8.2,5.3,6.6,5.8,5.7,6.2,7.1,7.0,6.9,5.6,4.2,6.8)
wilcox.test(drug,placebo,exact=FALSE,correct=TRUE,alternative="greater")


library(ISwR)
data(intake)
attach(intake)
intake
t.test(pre,post,paired=T)

### Example1
Before=c(12.1,10.6,13.4,13.8,15.5)
After=c(12.0,11.0,14.1,11.2,15.3)
#create the paired differences, Diff#
Diff=Before-After
shapiro.test(Diff)
#We can barely conclude Diff follows normal distribution since p=0.08496 is small but still > 0.05, while < 0.10


#One-sample t- test for Diff#
t.test(Diff)
#p-value = 0.5707; 95% CI is [-1.26036,1.98036]

#Alternatively, we can perform the paired-samples t- test directly use the original Before and After data as follows
t.test(Before, After, paired=TRUE)


### Example2
sheep1<-Drug.treated.sheep<-c(18,43,28,50,16,32,13)
sheep2<-Untreated.sheep<-c(40,54,26,63,21,37,39)
#Normality test for each population#
shapiro.test(sheep1)
shapiro.test(sheep2)
#F-test for equal variances#
var.test(sheep1,sheep2)
#F-test shows the ratio of two variances should equal to 1
t.test(sheep1,sheep2,var.equal=TRUE,a="l")
wilcox.test(sheep1,sheep2,conf.int=TRUE ,a="l")


### Example3
Nd<-New.drug<-c(0,10,-3,15,2,27,19,21,18,10)
Od<-Old.drug<-c(8,-4,7,5,10,11,9,12,7,8) 
shapiro.test(Nd) #p=0.7339#
shapiro.test(Od) #p=0.01674# 
#Hence we conclude that the population distribution of old drugs (Od) is not a normal distribution#
wilcox.test(Nd,Od,exact=F)
wilcox.test(Nd,Od,exact=F,alternative="g")





#Exercise1#
df<-data.frame(
  Farm=c(1,2,3,4,5,6,7),VarietyA=c(48.2,44.6,49.7,40.5,54.6,47.1,51.4),VarietyB=c(41.5,40.1,44.0,41.2,49.8,41.7,46.8)
);
t(df) 
#difference the data frame#
diff=df[,2]-df[,3]
diff 
#Get the data ready for tests#
shapiro.test(diff)
#p=0.01498<0.05, which means diff does not follow normal distribution#
#Hence, we cannot use t-test#
wilcox.test(diff) 
#p=0.03125, which suggests wo should reject null hypothesis#
#and think there is differences in mean yields for the two varieties of corn#

boxplot(diff)
qqnorm(diff)
qqline(diff)
#ususally we use qqnorm and qqline together# 
#Also by q-qplot, we cannot think diff follows normal distribution



# Exercise2
At<-Above.town<-c(4.8,5.2,5.0,4.9,5.1)
Bt<-Below.town<-c(5.0,4.7,4.9,4.8,4.9)
shapiro.test(At)
shapiro.test(Bt)
boxplot(At)
qqnorm(At)
qqline(At)
boxplot(Bt)
qqnorm(Bt)
qqline(Bt)
#The results from shapiro.test and output images show At and Bt follow normal distribution#
#Hence, we can use t.test#
var.test(At,Bt) #F-test#
#p=0.5421, so the variances of At and Bt are equal#
t.test(At,Bt,var.qual=TRUE)
#p=0.1507>0.05#
#Hence, we should accept null hypothesis that there is no differences between At and Bt#




