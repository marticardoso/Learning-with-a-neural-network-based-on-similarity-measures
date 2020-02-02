# In this file, there are some unit tests that check the right execution of the Ensemble of SNNs (only test the functionality) 
# (this is not an experiment)

rm(list = ls())
set.seed(104)

#First we load some useful function for the model selection task
set.seed(123)
source('SNNBagging.R')
source('test/benchmarkutils.R')
source('test/loadDatasets.R')

library(rattle.data)
library(faraway)
library(mlbench)

##################
# Classification #
##################

nRuns <- 10
ini <- milisec()
s <- sample(nrow(wine), 100)
set.seed(1234)
mult.ensA <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn.bagging(Type ~ ., subset = s, wine, bagging.method = 'A', nSNN = 20)
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

mult.ensC <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn.bagging(Type ~ ., subset = s, wine, bagging.method = 'C')
  mult.ensC[i] <- r1$testAccuracy
}
mean(mult.ensC)

set.seed(1234)
mult.ensE <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn.bagging(Type ~ ., subset = s, wine, bagging.method = 'E', nSNN = 10)
  mult.ensE[i] <- r1$testAccuracy
}
mean(mult.ensE)

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

bin.ensE <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn.bagging(Type1 ~ ., subset = s, wine2, bagging.method = 'E', nSNN = 10)
  bin.ensE[i] <- r1$testAccuracy
}
mean(bin.ensE)

bin.snn <- numeric(nRuns)
for (i in 1:nRuns) {
  r1 <- snn(Type1 ~ ., subset = s, wine2)
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

reg.ensE <- numeric(nRuns)
for (i in 1:nRuns) {
  reg.lm <- snn.bagging(medv ~ ., BostonHousing, subset = s, regularization = FALSE, nclust.method = "U", bagging.method = 'E', nSNN = 10)
  reg.ensE[i] <- reg.lm$nrmse
}
mean(reg.ensE)

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




############
## Horse Colic (multi) ##
############

CensusInfo <- LoadCensus()
ds7 <- CensusInfo$dataset
s <- sampleTwoThirds(ds7)
snn.ds7 <- snn.bagging(Target ~ ., ds7, subset = s, regularization = FALSE, nSNN = 10)
snn.ds7$testAccuracy
snn.ds7$testContingencyTable
