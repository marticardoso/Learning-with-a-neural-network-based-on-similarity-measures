rm(list = ls())
set.seed(104)

# Set environment
setwd(".")

#First we load some useful function for the model selection task
set.seed(123)
source('SNNBagging.R')

library(rattle.data)
library(faraway)
library(mlbench)

##################
# Classification #
##################

nRuns <- 1
ini <- milisec()
s <- sample(nrow(wine), 100)
set.seed(1234)
mult.ensA <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn.bagging(Type ~ ., subset = s, wine, bagging.method = 'A', runDaisyOnce = FALSE,  useGlobalDaisyTransformations = TRUE, nSNN = 20)
  mult.ensA[i] <- r1$testAccuracy
}
myToc(ini)
mean(mult.ensA)

mult.ensA2 <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn.bagging(Type ~ ., subset = s, wine, bagging.method = 'A2')
  mult.ensA2[i] <- r1$testAccuracy
}
mean(mult.ensA2)

mult.ensB <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn.bagging(Type ~ ., subset = s, wine, regularization = FALSE, bagging.method = 'B')
  mult.ensB[i] <- r1$testAccuracy
}
mean(mult.ensB)

mult.ensBReg <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn.bagging(Type ~ ., subset = s, wine, regularization = TRUE, bagging.method = 'B')
  mult.ensBReg[i] <- r1$testAccuracy
}
mean(mult.ensBReg)

#Test with one SNN
mult.snn <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn(Type ~ ., wine, subset = s, x = TRUE)
  mult.snn[i] <- r1$testAccuracy
}
mean(mult.snn)

##################
# Logical output #
##################

wine2 <- wine
wine2$Type <- NULL
wine2$Type1 <- as.factor(wine$Type == 1)
levels(wine2$Type1) <- c('NT1', 'T1')
s <- sample(nrow(wine), 100)

bin.ensA <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn.bagging(Type1 ~ ., subset = s, wine2, bagging.method='A')
  bin.ensA[i] <- r1$testAccuracy
}
mean(bin.ensA)

bin.ensA2 <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn.bagging(Type1 ~ ., subset = s, wine2, bagging.method = 'A2')
  bin.ensA2[i] <- r1$testAccuracy
}
mean(bin.ensA2)

bin.ensB <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn.bagging(Type1 ~ ., subset = s, wine2, bagging.method = 'B')
  bin.ensB[i] <- r1$testAccuracy
}
mean(bin.ensB)

bin.ensBReg <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn.bagging(Type1 ~ ., subset = s, wine2, bagging.method = 'B')
  bin.ensBReg[i] <- r1$testAccuracy
}
mean(bin.ensBReg)

bin.ensC <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn.bagging(Type1 ~ ., subset = s, wine2, bagging.method = 'C')
  bin.ensC[i] <- r1$testAccuracy
}
mean(bin.ensC)

bin.snn <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn(Type1 ~ ., subset = s, wine2, bagging.method = 'B')
  bin.snn[i] <- r1$testAccuracy
}
mean(bin.snn)

##############
# Regression #
##############

data(BostonHousing)
dim(BostonHousing)
s <- sample(nrow(BostonHousing), 400)

reg.ensA <- numeric(nRuns)
for (i in 1:nRuns) {
  reg.lm <- snn.bagging(medv ~ ., BostonHousing, subset = s, nclust.method = "U", bagging.method = 'A')
  reg.ensA[i] <- reg.lm$nrmse
}
mean(reg.ensA)

reg.ensB <- numeric(nRuns)
for (i in 1:nRuns) {
  reg.lm <- snn.bagging(medv ~ ., BostonHousing, subset = s, regularization = FALSE, nclust.method = "U", bagging.method = 'B')
  reg.ensB[i] <- reg.lm$nrmse
}
mean(reg.ensB)

reg.ensBReg <- numeric(nRuns)
for (i in 1:nRuns) {
  reg.lm <- snn.bagging(medv ~ ., BostonHousing, subset = s, regularization = TRUE, nclust.method = "U", bagging.method = 'B')
  reg.ensBReg[i] <- reg.lm$nrmse
}
mean(reg.ensBReg)


reg.ensC <- numeric(nRuns)
for (i in 1:nRuns) {
  reg.lm <- snn.bagging(medv ~ ., BostonHousing, subset = s, regularization = FALSE, nclust.method = "U", bagging.method = 'C')
  reg.ensC[i] <- reg.lm$nrmse
}
mean(reg.ensC)


reg.snn <- numeric(nRuns)
for (i in 1:nRuns) {
  reg.lm <- snn(medv ~ ., BostonHousing, subset = s)
  reg.snn[i] <- reg.lm$nrmse
}
mean(reg.snn)

# Numeric output
set.seed(1234)
s <- sample(nrow(prostate), 60)
reg.lm <- snn.bagging(lpsa ~ ., prostate, subset = s, nclust.method = "U", bagging.method = 'A')
reg.lm$mse
reg.lm$nrmse


sampleTwoThirds <- function(ds) {
  sample(nrow(ds), floor(2 / 2.5 * nrow(ds)))
}

ds2 <- read.csv('./datasets/regression/CASP.csv')
s.d2 <- sampleTwoThirds(ds2)

reg.lm <- snn.bagging(RMSD ~ ., ds2, subset = s.d2, nclust.method = "U", bagging.method = 'A', nSNN = 200)
reg.lm$nrmse

summary(lm(RMSD ~ ., ds2))


ds3 <- read.csv('./datasets/regression/SkillCraft1_Dataset.csv')
s.d3 <- sampleTwoThirds(ds3)
#Timeout error
snn.ds3 <- snn.bagging(LeagueIndex ~ ., ds3, nclust.method = "U", bagging.method = 'A', subset = s.d3, nSNN = 200)
snn.ds3$nrmse



daisyObject <- daisy2_noComputation(ds2, metric = "gower")

data.train.inputs