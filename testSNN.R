############
## Tests ###
############

rm(list = ls())
set.seed (104)

# Set environment
setwd(".")


library(rattle.data)
library(faraway)
library(mlbench)

#First we load some useful function for the model selection task
source('SNN.R')

# Example full call
s <- snn(Type~. 
         wine, 
         subset=s, 
         regularization = FALSE,
         standardizeSimils = TRUE,
         clust.control=list(
           clust.method="PAM", 
           clust.metric="euclidean", 
           clust.stand=FALSE, 
           nclust.method="U", 
           hp =0.1
         ),
         p.control=list(
           method='Opt', # (Opt, CV or G)
           #Opt fields
           maxIter= 100,
           pToler=1e-6,
           objFuncToler=1e-6,
           validation=TRUE,
           val.subset=NULL,
           #CV fields
           ps=seq(0.1,2,0.1), #Tested p values
           kFolds=10, # Number of folds
           useAccuracy, # Use accuracy to decide
           #G fields
           type=1 # type when G method (1 to 5) 
         ),
         GD.control=list(
           eps_f=1e-8,
           eps_p=1e-5,
           eps_g=1e-8,
           alphaMax=1,
           maxIter=100
         )
      )


s <- sample(nrow(wine), 100)

set.seed(1234)
r1 <- snn(Type ~ ., wine, subset = s, regularization = FALSE, standardizeSimils = TRUE, x = TRUE)
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


# Snn.createmodel
#mf <-r1$dataframe
#t <-snn.createClassificationModel(df, method="lasso")



# Numeric output
set.seed(1234)
s <- sample(nrow(prostate),60)
reg.lm <- snn(lpsa~.,prostate,subset=s)
#reg.lm$testReal
#reg.lm$testResponse-reg.lm$testReal
reg.lm$mse
reg.lm$nrmse

reg.ridge <- snn(lpsa~.,prostate,subset=s, regularization=TRUE, p.control=list(method='CV'))
#reg.ridge$testReal
#reg.ridge$testResponse-reg.ridge$testReal
reg.ridge$mse
reg.ridge$nrmse

#Boston housing
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


# PIMA
source('SNN.R')
pima <- read.table ("datasets/pima.dat", sep=",")
colnames(pima) <- c("Pregnancies","Plasma","Blood","Skin","Serum","BMI","Pedigree","Age","Target")
pima$Target <- factor(pima$Target, labels=c("No","Yes"))

Nlearn <- 500
learn <- 1:Nlearn

pima.glm <- snn(Target~.,pima,subset=learn)
pima.glm$testContingencyTable

#Preprocessing
sum(pima$Plasma==0) # 5
p <- rownames(subset(pima, Plasma==0))
pima[p,"Plasma"] <- NA

sum(pima$Blood==0) # 35
p <- rownames(subset(pima, Blood==0))
pima[p,"Blood"] <- NA

sum(pima$Skin==0) # 227
p <- rownames(subset(pima, Skin==0))
pima[p,"Skin"] <- NA

sum(pima$Serum==0) # 374
p <- rownames(subset(pima, Serum==0))
pima[p,"Serum"] <- NA

sum(pima$BMI==0) # 11
p <- rownames(subset(pima, BMI==0))
pima[p,"BMI"] <- NA

pima[,-9] <- scale(pima[,-9])

# For Gower
simil.types <- list(ordratio = c("Pregnancies", "Age"))

# Split target and predictors
pima2.glm <- snn(Target~.,pima,subset=learn, simil.types=simil.types)
pima2.glm$testContingencyTable
pima2.glm$testAccuracy

pima2.glm <- snn(Target~.,pima,subset=learn, regularization=TRUE, simil.types=simil.types)
pima2.glm$testContingencyTable
pima2.glm$testAccuracy

####################
## Other datasets ## (from mlbench)
####################




# Classification

data(BreastCancer)
summary(BreastCancer)

BreastCancer$Id <- NULL
bc.snn <- snn(Class~.,BreastCancer,subset=sample(nrow(BreastCancer),500))
bc.snn$testContingencyTable

data(DNA)
summary(DNA)

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


data(Ionosphere)
summary(Ionosphere)

data(LetterRecognition)
summary(LetterRecognition)

data(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes)

data(Satellite)
summary(Satellite)

data(Shuttle)
summary(Shuttle)

data(Sonar)
summary(Sonar)

data(Soybean)
summary(Soybean)

data(Vehicle)
summary(Vehicle)

data(Vowel)
summary(Vowel)

data(Zoo)
summary(Zoo)


#Regression

data(Ozone)
summary(Ozone)

data(Servo)
summary(Servo)




source('datasets/HorseColic23/HorseColic23.r')
summary(horse.colic)
s <- sample(nrow(horse.colic), 300)
hc.snn <- snn(Target ~ ., horse.colic, subset = s)
hc.snn$testContingencyTable
hc.snn$testAccuracy

#r1 <- snn.bagging(Target ~ ., horse.colic, subset = s, bagging.method = 'B', nSNN = 100)
#r1$testContingencyTable
#r1$testAccuracy


source('datasets/HorseColic24/HorseColic24.r')
summary(horse.colic)
s <- sample(nrow(horse.colic), 300)
hc.snn <- snn(Target ~ ., horse.colic, subset = s)
hc.snn$testContingencyTable
hc.snn$testAccuracy

r1 <- snn.bagging(Target ~ ., horse.colic, subset = s, bagging.method = 'B', nSNN = 100)
r1$testContingencyTable
r1$testAccuracy