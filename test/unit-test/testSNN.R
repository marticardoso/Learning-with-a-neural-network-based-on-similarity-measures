# In this file, there are some unit tests that check the right execution of the SNN (only test the functionality) 
# (this is not an experiment)

############
## Tests ###
############

rm(list = ls())
set.seed (104)


library(rattle.data)
library(faraway)
library(mlbench)
library(tree)

#First we load some useful function for the model selection task
source('SNN.R')
source("test/testUtils.R")
source('test/loadDatasets.R')

s <- sample(nrow(wine), 100)

set.seed(1234)
r1 <- snn(Type ~ ., wine, subset = s, regularization = FALSE, standardizeSimils = TRUE)
r1$testContingencyTable
r1$testAccuracy
summary(r1)
r2 <- snn(Type~.,wine,subset=s,regularization=TRUE)
r2$testContingencyTable

set.seed(32245)
r4 <- snn(Type~.,wine,subset=sample(nrow(wine),100),regularization=FALSE, clust.method = "Random", p.control=list(method='CV'))
r4$testContingencyTable

response = predict(r1,wine,c("response","prob"))
response$response
table(response$response, wine$Type)

(pred <- predict (r1, wine[1:20,], type="response"))

#Test with logical / factor of two modalities
wine2 <- wine
wine2$Type <- NULL
wine2$Type1 <- wine$Type==1

r3 <- snn(Type1~.,wine2,subset=sample(nrow(wine),100), regularization=FALSE)
r3$testContingencyTable
pred <- predict(r3,wine2)

r4 <- snn(Type1~.,wine2,subset=sample(nrow(wine),100), regularization=TRUE)
r4$testContingencyTable



# SNN.FIT
r4 <- snn.fit(wine[,-1],wine$Type, regularization=FALSE)


# Numeric output
set.seed(1234)
data(BostonHousing)
dim(BostonHousing)

s <- sample(nrow(BostonHousing),400)
reg.lm <- snn(medv ~ ., BostonHousing, subset = s, regularization = FALSE,
              clust.control = list(clust.method = "PAM", nclust.method = "C"),
              p.control = list(method = 'CV'))
reg.lm$mse
reg.lm$nrmse

reg.lm <- snn(medv ~ ., BostonHousing, subset = s, regularization = FALSE,
              clust.control = list(clust.method = "PAM", nclust.method = "C"),
              p.control = list(method = 'Opt'))
reg.lm$mse
reg.lm$nrmse

reg.ridge <- snn(medv~.,BostonHousing,subset=s, regularization=TRUE)
#reg.ridge$testReal
#reg.ridge$testResponse-reg.ridge$testReal
reg.ridge$mse
reg.ridge$nrmse


# Test PIMA dataset

pima <- LoadPimaDataset()$dataset
simil.types <- LoadPimaDataset()$simil.types
learn <- sample(nrow(pima), 500)
# Split target and predictors
pima2.glm <- snn(Target~.,pima,subset=learn, simil.types=simil.types)
pima2.glm$testContingencyTable
pima2.glm$testAccuracy

pima2.glm <- snn(Target~.,pima,subset=learn, regularization=TRUE, simil.types=simil.types)
pima2.glm$testContingencyTable
pima2.glm$testAccuracy

####################
## Other datasets ## 
####################

# Classification

data(BreastCancer)
summary(BreastCancer)

BreastCancer$Id <- NULL
bc.snn <- snn(Class~.,BreastCancer,subset=sample(nrow(BreastCancer),500))
bc.snn$testContingencyTable

## GLASS ##
data(Glass)
summary(Glass)

glass.snn <- snn(Type~.,Glass,subset=sample(nrow(Glass),150))
glass.snn$testContingencyTable
sum(diag(table(predict(glass.snn,Glass, c("response")), Glass$Type)))

# Check with only multinom
tmp <- multinom(Type~.,Glass)
sum(diag(table(predict(tmp,Glass), Glass$Type)))

## ##
data(HouseVotes84)
summary(HouseVotes84)

hv84.snn <- snn(Class~.,HouseVotes84,subset=sample(nrow(HouseVotes84),300))
hv84.snn$testContingencyTable



#Regression

horse.colic <- LoadHorseColicV2()$dataset
summary(horse.colic)
s <- sample(nrow(horse.colic), 300)
hc.snn <- snn(Target ~ ., horse.colic, subset = s)
hc.snn$testContingencyTable
hc.snn$testAccuracy


horse.colic <- LoadHorseColicV1()$dataset
summary(horse.colic)
s <- sample(nrow(horse.colic), 300)
hc.snn <- snn(Target ~ ., horse.colic, subset = s)
hc.snn$testContingencyTable
hc.snn$testAccuracy


#####################
## Automobile ##
#####################
set.seed(2)
automobile <- LoadAutomobileDS()
s <- sampleTwoThirds(automobile$dataset)
snn.ds1 <- snn(automobile$formula, automobile$dataset, subset = s, simil.types = automobile$simil.types)
snn.ds1$nrmse

#Compare to decision tree
model.tree <- tree(price ~ ., data = automobile$dataset[s,])
nrmse(predict(model.tree, automobile$dataset[-s,]), automobile$dataset[-s,]$price)

#############
## AutoMPG ##
#############

info <- LoadAutoMPGDS()
ds2 <- info$dataset
s.d2 <- sampleTwoThirds(ds2)

snn.ds2 <- snn(mpg ~ ., ds2, subset = s.d2, regularization = FALSE)
snn.ds2$nrmse

#Compare to decision tree
model.tree <- tree(mpg ~ ., data = ds2[s.d2,])
nrmse(predict(model.tree, ds2[-s.d2,]), ds2[-s.d2,]$mpg)


#####################
## Communities and Crime Data Set ##
#####################
# Nrows: 9568
ds4 <- LoadCommunitiesDataset()$dataset
s.d4 <- sampleTwoThirds(ds4)
#Timeout error
snn.ds4 <- snn(Target ~ ., ds4, subset = s.d4, regularization = FALSE, clust.control = list(clust.method = "R"))
snn.ds4$nrmse

#Compare to decision tree
model.tree <- tree(Target ~ ., data = ds4[s.d4,])
nrmse(predict(model.tree, ds4[-s.d4,]), ds4[-s.d4,]$Target)



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
pred <- predict(model.tree, ds1[-s,], type = 'class')
accuracy(pred, ds1[-s,]$Target)

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


#Compare to decision tree
model.tree <- tree(Target ~ ., data = ds3[s,])
pred <- predict(model.tree, ds3[-s,], type = 'class')
accuracy(pred, ds3[-s,]$Target)



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
model.tree <- tree(Target ~ ., data = ds4[s,])
pred <- predict(model.tree, ds4[-s,], type = 'class')
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
