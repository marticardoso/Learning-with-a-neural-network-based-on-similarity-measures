

rm(list = ls())
set.seed (104)

# Set environment
setwd(".")


library(rattle.data)
library(faraway)
library(mlbench)
library(tree)

#First we load some useful function for the model selection task
source('SNN.R')
source('testUtils.R')

#Regression

data(Ozone)
summary(Ozone)

data(Servo)
summary(Servo)

#####################
## Automobile ##
#####################
autoInfo <- LoadAutomobileDS()
ds1 <- autoInfo$dataset
s <- sampleTwoThirds(ds1)
snn.ds1 <- snn(price ~ ., ds1, subset = s, regularization = FALSE)
snn.ds1$nrmse

#Compare to decision tree
model.tree <- tree(price ~ ., data = ds1[s,])
nrmse(predict(model.tree, ds1[-s,]), ds1[-s,]$price)

runRegressionEnsembleTests(price ~ ., ds1)

#############
## AutoMPG ##
#############

info <- LoadAutoMPGDS()
ds2 <- info$dataset
s.d2 <- sampleTwoThirds(ds2)

snn.ds2 <- snn(mpg~.,ds2,subset=s.d2,regularization=FALSE)
snn.ds2$nrmse

#Compare to decision tree
model.tree <- tree(mpg ~ ., data = ds2[s.d2,])
nrmse(predict(model.tree, ds2[-s.d2,]), ds2[-s.d2,]$mpg)

r <- runRegressionEnsembleTests(mpg ~ ., ds2)


#####################
## Communities and Crime Data Set ##
#####################
# Nrows: 9568
ds4 <- LoadCommunitiesDataset()$dataset
s.d4 <- sampleTwoThirds(ds4)
#Timeout error
snn.ds4 <- snn(Target~.,ds4,subset=s.d4,regularization=FALSE)
snn.ds4$nrmse

#Compare to decision tree
model.tree <- tree(Target ~ ., data = ds4[s.d4,])
nrmse(predict(model.tree, ds4[-s.d4,]), ds4[-s.d4,]$Target)

r <- runRegressionEnsembleTests(Target ~ ., ds4)


#####################
## MV Data Set ##
#####################
# Nrows: 9568
ds4 <- LoadMvDataset()$dataset
s.d4 <- sampleTwoThirds(ds4)
#Timeout error
snn.ds4 <- snn.bagging(Target ~ ., ds4, subset = s.d4, regularization = FALSE)
snn.ds4$nrmse

#Compare to decision tree
model.tree <- tree(Target ~ ., data = ds4[s.d4,])
nrmse(predict(model.tree, ds4[-s.d4,]), ds4[-s.d4,]$Target)

#####################
## Concrete Compressive Strength Data Set ##
#####################
library("readxl")
ds6 <- data.frame(read_excel('./datasets/regression/Concrete_Data.xls'))
colnames(ds6) <- c('Cement','BlastFurnace','FlyAsh','Water','Superplasticizer', 'Coarse','Fine', 'Age','CCS')
s.d6 <- sampleTwoThirds(ds6)
#Timeout error
snn.ds6 <- snn(CCS~.,ds6,subset=s.d6,regularization=FALSE, p.control=list(method='Opt'))
snn.ds6$nrmse

#Compare to only lm
lm.ds6 <- lm(CCS~.,ds6)
1-summary(lm.ds6)$r.squared

#####################
## Wave dataset##
#####################
ds7 <- LoadWaveDataset()$dataset

s.d7 <- sampleTwoThirds(ds7)
#Timeout error
snn.ds7 <- snn.bagging(Target ~ ., ds7, subset = s.d7, regularization = FALSE)
snn.ds7$nrmse

model.tree <- tree(Target ~ ., data = ds7[s.d7,])
nrmse(predict(model.tree, ds7[-s.d7,]), ds7[-s.d7,]$Target)



