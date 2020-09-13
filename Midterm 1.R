# 1.
  #(1)
   num1 <- sample(c(5,15,25,35),100,replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.4))
   num1
  #(2)
    value1 =0
    value2= 0
    value3=0
    value4=0
    x = runif(100)
    rv = c() 
    for( i in x) {
      if(i < 0.1) {
        value1 = value1 + 1
        rv = c(rv, 5)
      }
      else if (i < 0.3) {
        value2 = value2 + 1
        rv = c(rv, 15)
      }
      else if(i < 0.6){
        value3 = value3 + 1
        rv = c(rv, 25)
      }
      else{
        value4 = value4 + 1
        rv = c(rv, 35)
      }
    }
    rv
  #(3)
    hist(x = num1, main = 'Random Vector with Sample', xlab= 'values', ylab= "Frequency", xlim= c(0, 50),breaks= 4)
    hist(x = rv, main = 'Random Vector with Sample', xlab= 'values', ylab= "Frequency", xlim= c(0, 50),breaks= 4)
    label <-c(5,15,25,35)
    values <-c(value1, value2, value3, value4)
    pie(values, label, main = 'Random Vector without Sample')
    
#2.
  #(1) 
    rchisq(15, 9, ncp = 0)
  #(2)
    dpois(7, 3, log = FALSE)
  #(3)
    1 - pnorm(14, mean = 12, sd =6)
  #(4)
    pexp(5,2) - pexp(2,2)
  #(5)
    dbinom(5, 12, .6)

#3.
  library(ISwR)
  data(lung)
    #(1)
    mean(lung[,1])
    var(lung[,1]) 
    #(2)
    shapiro.test(lung[,1])
    #(3)
    t.test(lung[,1], mu = 3.3, alternative = "less")
    
#4. 
  withDrug <- c(49, 60, 65, 52, 58)
  noDrug <- c(75, 82, 79, 65, 77, 83)
  shapiro.test(withDrug)
  shapiro.test(noDrug)
  t.test(withDrug, noDrug, paired = FALSE, var.equal = FALSE, alternative = "less")
  
#5.
  salespersons  <- matrix(c(12, 18, 25, 9, 14, 16, 
                            36, 25, 26, 15, 17, 18), nrow=2,byrow=TRUE)
  salespersons
  difference <- salespersons[2,] - salespersons [1,]    
  difference    
  shapiro.test(difference)
  wilcox.test(difference, mu = 0, alternative = "greater")    

#6.    
  filename = "d_logret_6stocks.txt"
  table <- read.table(filename,header=T)
  attach(table)
  #(1)
  lm(GenMotor ~ Citigroup)
  summary(lm(GenMotor ~ Citigroup))    
  #(2)
  fit <- lm(GenMotor ~ Citigroup)
  predict(fit,newdata=data.frame(Citigroup = 0.05))
  #(3)
  cor(GenMotor, Citigroup)
  cor.test(GenMotor, Citigroup)

#7. 
  #(1) 
    library(MASS)
    data(cement)
    summary(lm(y~ (-1 +x1) + (-1 +x2)+ (-1 +x3)+ (-1+x4), cement))
  #(2)
  
  #(3)
    summary(regsubsets(y~ (x1) + (x2)+ (x3)+ (x4), cement,nvmax =2))
    
    
    