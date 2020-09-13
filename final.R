library(ISwR)

#1
attach(ToothGrowth)
half <- subset(ToothGrowth, dose == 0.5)
one <- subset(ToothGrowth, dose == 1.0)
two <- subset(ToothGrowth, dose == 2.0)
shapiro.test(half[,1])
shapiro.test(one[,1])
shapiro.test(two[,1])
#(a)
  anova(lm(len~dose, data = ToothGrowth))
#(b)
  pairwise.t.test(len, dose, p.adj = "bonf")
#(c)
  VC <- subset(ToothGrowth, supp == "VC")
  OJ <- subset(ToothGrowth, supp == "OJ")
  shapiro.test(VC[,1])
  shapiro.test(OJ[,1])
  t.test(VC[,1], OJ[,1])
#(d)
  anova(lm(len~supp + dose, data = ToothGrowth))

#2
attach(kfm)
  dlMilk <- kfm$dl.milk
  sex <- kfm$sex
  weight <- kfm$weight
  mlSuppl <- kfm$ml.suppl
  matWeight <- kfm$mat.weight
  matHeight <- kfm$mat.height
  #(a)
    reg <- lm(dlMilk~sex + weight + mlSuppl + matWeight+ matHeight)
    summary(reg)
  #(b)
    summary(regsubsets((dl.milk ~ (as.numeric(sex)) + (weight) + (ml.suppl) + (mat.weight) + (mat.height)), kfm))

    
#3
    install.packages("corrplot")
    library(corrplot)
  X<- c(30, 29, 31, 47, 40, 27, 46, 50)
  Y<- c(70, 68, 72, 93, 84, 65, 91, 96)
  Z<- c(25, 27, 17, 20, 10, 38, 36, 29)
  fulldata <- data.frame(X=X,Y=Y,Z=Z)
  #a
  cor(fulldata)
    corrplot(fulldata)
  #b
  LX = log(X)
  cor(LX, Y)
  #c
  lm(Y ~ LX)
  line <- lm(Y ~ LX)
  summary(lm(Y ~ LX))$r.squared
  #e
  plot(LX,Y)
  newX = seq(min(LX), max(LX), by = 0.05)
  conf_interval <- predict(line, newdata=data.frame(x=newx), interval="confidence",
                           level = 0.95)
  abline(-100.76, 50.22) 
  matlines(newx, conf_interval[,2:3], col = "blue", lty=2)
  
#4
  x1<- c(34,2,1,33,22,10,16,36,42,44)
  x2<- c(32,20,36,45,19,50,26,41,2,44)
  x3<- c(44,11,17,18,16,9,2,30,48,15)
  x4<- c(43,9,46,34,30,49,15,23,44,11)
  y<- c(283,54,77,175,135,72,61,232,322,196)
  #b
    lm(y~ (x1) + (x2)+ (x3)+ (x4))
    summary(lm(y~ (x1) + (x2)+ (x3)+ (x4)))
  #a
    lm(y~ (-1 +x1) + (-1 +x2)+ (-1 +x3)+ (-1+x4))
    summary(lm(y~ (-1 +x1) + (-1 +x2)+ (-1 +x3)+ (-1+x4)))
  #c
    summary(lm(y~ (x1) + (x2)))$r.squared 
    summary(lm(y~ (x1) + (x3)))$r.squared 
    summary(lm(y~ (x1) + (x4)))$r.squared 
    summary(lm(y~ (x2) + (x3)))$r.squared 
    summary(lm(y~ (x2) + (x4)))$r.squared 
    summary(lm(y~ (x3) + (x4)))$r.squared 
    
#5
    A<- c(570,530,540,535,585,537,590)
    B<- c(555,512,510,520,510,512,570)
    C<- c(512,518,555,502,510,520,516)
    D<- c(505,508,512,520,543,523,517)
    shapiro.test(A)
    shapiro.test(B)
    shapiro.test(C)
    shapiro.test(D)
    #a
    y<- c(A, B, C, D)
    x <- c(rep("A",7), rep("B", 7), rep("C",7), rep("D", 7))
    anova(lm(y~factor(x)))
    #b
    TukeyHSD(aov(lm(y~factor(x))))

    
#6
  age <- c(rep(c(rep(1, 5), rep(2,5)), 2))
  group <- c(rep("C", 10), rep("P", 10))
  cValues <- c(7,6,6,5,6,9,8,8,9,7)
  pValues <- c(9,8,9,9,8,6,7,6,6,5)
  fullValues <- c(cValues, pValues)
  anova(lm(fullValues~group + age + group*age))
    
    
    
    
    
    
    
    