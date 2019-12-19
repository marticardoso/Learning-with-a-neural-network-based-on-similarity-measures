

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


#####################
## Heart ##
#####################

heartInfo <- LoadHeartDataset()
ds1 <- heartInfo$dataset
s <- sampleTwoThirds(ds1)
snn.ds1 <- snn(Target ~ ., ds1, subset = s, regularization = FALSE)
snn.ds1$testAccuracy

#Compare to decision tree
model.tree <- tree(Target ~ ., data = ds1[s,])
pred <- predict(model.tree, ds1[-s,], type='class')
accuracy(pred, ds1[-s,]$Target)


r <- runClassificationEnsembleTests(Target ~ ., ds1)

#####################
## Mammographic ##
#####################

mammographicInfo <- LoadMammographicDataset()
ds2 <- mammographicInfo$dataset
s <- sampleTwoThirds(ds2)
snn.ds2 <- snn(severity ~ ., ds2, subset = s, regularization = TRUE)
snn.ds2$testAccuracy
snn.ds2$testContingencyTable

#Compare to decision tree
model.tree <- tree(severity ~ ., data = ds2[s,])
pred <- predict(model.tree, ds2[-s,], type = 'class')
accuracy(pred, ds2[-s,]$severity)

#####################
## Mushroom ##
#####################

MushroomInfo <- LoadMushroomDataset()
ds3 <- MushroomInfo$dataset
s <- sampleTwoThirds(ds3)
snn.ds2 <- snn(Target ~ ., ds3, subset = s, regularization = FALSE, clust.control = list(clust.method = "R"))
snn.ds2$testAccuracy
snn.ds2$testContingencyTable

r <- runClassificationEnsembleTests(Target ~ ., ds3)

#Compare to decision tree
model.tree <- tree(Target ~ ., data = ds3[s,])
pred <- predict(model.tree, ds3[-s,], type = 'class')
accuracy(pred, ds3[-s,]$Target)


#####################
## Adult  ##
#####################

AdultInfo <- LoadAdultDataset()
ds4 <- AdultInfo$dataset
s <- sampleTwoThirds(ds4)
snn.ds4 <- snn(Target ~ ., ds4, subset = s, regularization = FALSE, clust.control = list(clust.method = "R"))
snn.ds4$testAccuracy
snn.ds4$testContingencyTable

#Compare to decision tree
model.tree <- tree(Target ~ ., data = ds4[s,-14])
pred <- predict(model.tree, ds4[-s,-14], type = 'class')
accuracy(pred, ds4[-s,]$Target)


#####################
## Annealing  ##
#####################

AnnealingInfo <- LoadAnnealing()
ds4 <- AnnealingInfo$dataset
s <- sampleTwoThirds(ds4)
snn.ds4 <- snn(Target ~ ., ds4, subset = s, regularization = FALSE, clust.control = list(clust.method = "R"))
snn.ds4$testAccuracy
snn.ds4$testContingencyTable

#Compare to decision tree
model.tree <- tree(Target ~ ., data = ds4[s, ])
pred <- predict(model.tree, ds4[-s, ], type = 'class')
accuracy(pred, ds4[-s,]$Target)


############
## Horse Colic (multi) ##
############

HorseInfo <- LoadHorseColicV1()
ds5 <- HorseInfo$dataset
s <- sampleTwoThirds(ds5)
snn.ds5 <- snn(Target ~ ., ds5, subset = s, regularization = FALSE)
snn.ds5$testAccuracy
snn.ds5$testContingencyTable

#Compare to decision tree
model.tree <- tree(Target ~ ., data = ds5[s,])
pred <- predict(model.tree, ds5[-s,], type = 'class')
accuracy(pred, ds5[-s,]$Target)

############
## Horse Colic (bi) ##
############

HorseInfo <- LoadHorseColicV2()
ds6 <- HorseInfo$dataset
s <- sampleTwoThirds(ds6)
snn.ds6 <- snn(Target ~ ., ds6, subset = s, regularization = FALSE)
snn.ds6$testAccuracy
snn.ds6$testContingencyTable

#Compare to decision tree
model.tree <- tree(Target ~ ., data = ds6[s,])
pred <- predict(model.tree, ds6[-s,], type = 'class')
accuracy(pred, ds6[-s,]$Target)




ds7 <- LoadDiabetis()$dataset
summary(ds7)
s.d7 <- sampleTwoThirds(ds7)
#Timeout error
snn.ds7 <- snn.bagging(readmitted ~ ., ds7, subset = s.d7)
snn.ds7$testContingencyTable
ds7$discharge_disposition_id <- NULL
ds7$admision_source_id <- NULL
ds7$medical_specialty <- NULL
ds7$diag_1 <- NULL
ds7$diag_2 <- NULL
ds7$diag_3 <- NULL

model.tree <- randomForest(readmitted ~ ., data = ds7[s.d7,], na.action = na.roughfix)
pred <- predict(model.tree, ds7[-s.d7,], type = 'class')
table(pred, ds7[-s.d7,]$readmitted)

for (i in 1:ncol(ds7)) {
  if (is.factor(ds7[, i])) {
    if (nlevels(ds7[, i]) > 30) {
      print(i)
    }
  }
}


############
## Horse Colic (multi) ##
############

CensusInfo <- LoadCensus()
ds7 <- CensusInfo$dataset
s <- sampleTwoThirds(ds7)
snn.ds7 <- snn.bagging(Target ~ ., ds7, subset = s, regularization = FALSE)
snn.ds7$testAccuracy
snn.ds7$testContingencyTable

#Compare to decision tree
model.tree <- randomForest(Target ~ ., data = ds7[s, c(-22, -23, -33, -34, -35)], na.action = na.omit)
pred <- predict(model.tree, ds7[-s, c(-22, -23, -33, -34, -35)], type = 'class')
accuracy(pred, ds7[-s,]$Target)


