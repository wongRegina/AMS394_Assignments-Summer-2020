# 1. Consider the following heights 1.55, 1.92, 1.60, 1.75, 1.58, 1.67, 1.63, 1.82, 1.76, 1.77, 1.72, 1.85.
  # (1) Assign all these heights as vector "height". 
    height= c(1.55, 1.92, 1.60, 1.75, 1.58, 1.67, 1.63, 1.82, 1.76, 1.77, 1.72, 1.85)
  
  # (2) Compute the mean and standard deviation of "height"?
    mean(height)
    sd(height)
  
  # (3) What is the length of "height"?
    length(height)

  # (4) How many heights are less than 1.65?
    length(height[which(height>1.65)])

  # (5) Show if each height is larger than 1.60 and smaller than 1.75.
    subset(height, 1.60<height & height<1.75)
    
# 2  Use the following script, we can generate a 3x4 matrix: tmp <- matrix(rnorm(12), 3, 4)
    tmp <- matrix(rnorm(12), 3, 4)
    
  # (1) Compute the sum of the second and third row, respectively.
    sum(tmp[2,])
    sum(tmp[3,])
    
  # (2) Compute the product of second and fourth column, respectively.
    sum(tmp[,2])
    sum(tmp[,4])
    
  # (3) Show the dimension of the matrix.
    dim(tmp)
    
  # (4) Use "cat" function to output elements in the second row that are less than 0.2.
    cat(subset(tmp[2,],tmp[2,]< 0.2), sep = " ", fill =FALSE)
    
# 3
  # (1) Use "sample" function to generate a random vector that follows a multinomial distribution with probability (0.2, 0.3, 0.5).
    sample(c('1','2','3'),100,replace = TRUE, prob = c(0.2,0.3,0.5))
    
  # (2) Without use the sample function, generate a random vector that follows a multinomial distribution with probability (0.2, 0.3, 0.5). 
    # Hint: Using runif (or rbinom). Please write several lines of R scripts to simulate a multinomial distribution with probability (0.2, 0.3, 0.5).
    x = runif(100)
    rv = c() 
    for( i in x) {
      if(i < 0.2) {
        rv = c(rv, '1')
      }
      else if (i < 0.5) {
        rv = c(rv, '2')
      }
      else{
        rv = c(rv, '3')
      }
    }
    rv
    
# 4. Calculate the probability for each of the following events:
  # (1) A normally distributed variable with mean 15 and standard deviation 3 is less than 16.
    pnorm(16, mean=15, sd =3)
    
  # (2) X < 8 in a chi-square distribution with 10 degrees of freedom.
    pchisq(8,10)
    
  # (3) Getting 5 out of 10 successes in a binomial distribution with probability 0.4.
    dbinom(5, 10, 0.4)
    
  # (4) X = 5 in a Poisson distribution with ?? is 3. 
    dpois(5,3)
    
# 5. Generate 100 exponentially distributed random variables with rate 2 and plot the empirical distribution function.     
    x=rexp(100, rate=2)
    n= length(x)
    plot(sort(x),(1:n)/n,type="s",ylim=c(0,1))
    
# 6. Over the past 5 years, the mean time for a warehouse to fill a buyer's order has been 25 minutes.
  #  Officials of the company believe that the length of time has increased recently, either due to a change in
  #  the workforce or due to a change in customer purchasing policies. The processing time (in minutes) was
  #  recorded for a random sample of 15 orders processed over the past month.
  #  28 25 27 31 10 26 30 15 55 12 24 32 28 42 38
    time = c(28, 25, 27, 31, 10, 26, 30, 15, 55, 12, 24, 32, 28, 42, 38)
  
  # (1) Check the normality of the data   
    shapiro.test(time)
  # "Since the p-vale of the Shapiro-Wilk normality test is less than 0.05, we can conlude normatlity")   
    
  # (2) Please test the research hypothesis at the significance level ?? = 0.05
    t.test(time,	mu=25,	alternative="greater")
    
    