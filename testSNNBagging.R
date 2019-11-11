rm(list = ls())
set.seed(104)

# Set environment
setwd(".")


library(rattle.data)
library(faraway)
library(mlbench)


#First we load some useful function for the model selection task
source('SNNBagging.R')
acc.ens <- numeric(20)
s <- sample(nrow(wine), 100)
for (i in 1:20) {
  r1 <- snn.bagging(Type ~ ., subset = s, regularization = TRUE, wine, trace = FALSE)
  r1$testContingencyTable
  acc.ens[i] <- r1$testAccuracy
}
mean(acc.ens)

#Test with one SNN
acc.snn <- numeric(20)
for (i in 1:20) {
  r1 <- snn(Type ~ ., wine, subset = s, x = TRUE)
  acc.snn[i] <- r1$testAccuracy
}

##############
# Regression #
##############

# Numeric output
set.seed(1234)
s <- sample(nrow(prostate), 60)
reg.lm <- snn.bagging(lpsa ~ ., prostate, subset = s, nclust.method = "U")
reg.lm$mse
reg.lm$nrmse


data(BostonHousing)
dim(BostonHousing)

r.ens <- numeric(10)
s <- sample(nrow(BostonHousing), 400)
for (i in 1:10) {
  reg.lm <- snn.bagging(medv ~ ., BostonHousing, subset = s, nclust.method = "U")
  r.ens[i] <- reg.lm$nrmse
}

r.snn <- numeric(10)
for (i in 1:10) {
  reg.lm <- snn(medv ~ ., BostonHousing, subset = s)
  r.snn[i] <- reg.lm$nrmse
}
