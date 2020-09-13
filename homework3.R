library(ISwR)

# 1.
  filename = "d_logret_6stocks.txt"
  table <- read.table(filename,header=T)
  attach(table)
  #(1)
  PonE <- lm(Pfizer~Exxon)
  coef(PonE)
  #(2)
  anova(PonE)
  #(3)
  g1 <- Pfizer
  g2 <- Exxon
  g3 <- Citigroup
  y <- c(g1, g2, g3)
  x <- c(rep(1,64), rep(2,64), rep(3,64))
  singleAnova <- lm(y~factor(x))
  anova(singleAnova)

#2
  library(ISwR)
  attach(juul)
  #(1)
  juulFit <- lm(igf1 ~ factor(tanner))
  anova(juulFit)
  #(2)
  tapply(igf1, tanner, mean, na.rm = T)
  #(3)
  pairwise.t.test(x = igf1, g = tanner, p.adj = 'bonferroni')
  
#3
  harpyEagle <- c(100, 102, 99, 105, 97, 98, 92, 108)
  goldenEagle <- c(90, 88, 84, 97, 80, 91, 86, 82)
  whiteTailEagle <- c(72, 77, 64, 75, 66, 80, 82, 79)
  #(1)
  shapiro.test(harpyEagle)
  shapiro.test(goldenEagle)
  shapiro.test(whiteTailEagle)
  #(2)
  bartlett.test(list(harpyEagle, goldenEagle, whiteTailEagle))
  #(3)
  dataFull<- c(harpyEagle, goldenEagle, whiteTailEagle)
  species <- c(rep("harpyEagle",8), rep("goldenEagle",8), rep("whiteTailEagle",8))
  fullDataEagle <- data.frame(species = species, wingLength = dataFull)
  reg <- lm(wingLength ~ species, data = fullDataEagle)
  resEagle<- reg$residuals
  fitEagle<- reg$fitted.values
  plot(fitEagle, resEagle)
  #(4)
  dataFull<- c(harpyEagle, goldenEagle, whiteTailEagle)
  species <- c(rep("harpyEagle",8), rep("goldenEagle",8), rep("whiteTailEagle",8))
  fullDataEagle <- data.frame(species = species, wingLength = dataFull)
  anova(lm(wingLength ~ species, data = fullDataEagle))
  
#4
  ageGroup <-c(rep("15-18", 3), rep ("12-14", 3))
  groupAScores <- c( 92,88,76,82,62,74)
  fullGroupA <- data.frame(mathScores = groupAScores, Age = ageGroup)
  groupBScores <- c( 94,88,73,76,91,78)
  fullGroupB <- data.frame(mathScores = groupBScores, Age = ageGroup)
  groupCScores <- c( 95,92,84,94,98,82)
  fullGroupC <- data.frame(mathScores = groupCScores, Age = ageGroup)
  #(1)
  totalScores <- c(groupAScores, groupBScores, groupCScores)
  groups <- c(rep("A", 6),rep("B", 6), rep("C", 6))
  anova(lm(totalScores~ factor(groups)))
  #(2)
  pairwise.t.test(totalScores, groups, 	p.adjust="bonferroni")
  #(3)
  testScores <-data.frame(totalScores, groups)
  TukeyHSD(aov(totalScores~groups, data = testScores), conf.level = 0.95)
  
#5
  ageGroup <-c(rep("15-18", 3), rep ("12-14", 3))
  groupAScores <- c( 92,88,76,82,62,74)
  fullGroupA <- data.frame(mathScores = groupAScores, Age = ageGroup)
  groupBScores <- c( 94,88,73,76,91,78)
  fullGroupB <- data.frame(mathScores = groupBScores, Age = ageGroup)
  groupCScores <- c( 95,92,84,94,98,82)
  fullGroupC <- data.frame(mathScores = groupCScores, Age = ageGroup)
  totalScores <- c(groupAScores, groupBScores, groupCScores)
  fullTestScoreData <- data.frame(testScores, ageGroup = c(rep(ageGroup,3)))
  #(1)
  anova(lm(totalScores~groups + ageGroup, data = fullTestScoreData))
  #(2)
  ageGroupSub <-c(rep(1, 3), rep (2, 3))
  fullTestScoreData <- data.frame(testScores, ageGroup = c(rep(ageGroupSub,3)))
  group <- fullTestScoreData$groups
  testScore <- fullTestScoreData$totalScores
  age <- fullTestScoreData$ageGroup
  interaction.plot(group, age, testScore)
  #(3)
  qqplot(testScore, age)
  qqnorm(testScore)
  reg <- lm(totalScores~groups + ageGroup, data = fullTestScoreData)
  res <- reg$residuals
  fit <- reg$fitted.values
  plot(fit, res)
  
  
  
  
  