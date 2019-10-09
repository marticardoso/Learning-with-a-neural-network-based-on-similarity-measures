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

r1 <- snn(Type~.,wine,subset=sample(nrow(wine),100),method="multinom", x=TRUE, p=0.2, hp=0.05)
r1$testContingencyTable
r2 <- snn(Type~.,wine,subset=sample(nrow(wine),100),method="ridge")
r2$testContingencyTable
r3 <- snn(Type~.,wine,subset=sample(nrow(wine),100),method="lasso")
r3$testContingencyTable

r4 <- snn(Type~.,wine,subset=sample(nrow(wine),100),method="multinom", clust.method = "Random")
r4$testContingencyTable

response = predict(r1,wine,c("response","prob"))
response$response
table(response$response, wine$Type)

(pred <- predict (r1, wine[1:20,], type="response"))

#Test with logical / factor of two modalities
wine2 <- wine
wine2$Type <- NULL
wine2$Type1 <- wine$Type==1

r3 <- snn(Type1~.,wine2,subset=sample(nrow(wine),100), hp=0.1,method="glm")
r3$testContingencyTable
pred <- predict(r3,wine2)

r4 <- snn(Type1~.,wine2,subset=sample(nrow(wine),100), method="ridge")
r4$testContingencyTable



# SNN.FIT
r4 <- snn.fit(wine[,-1],wine$Type, method="multinom")


# Snn.createmodel
#mf <-r1$dataframe
#t <-snn.createClassificationModel(df, method="lasso")



# Numeric output
set.seed(1234)
s <- sample(nrow(prostate),60)
reg.lm <- snn(lpsa~.,prostate,subset=s, method="lm", hp= 0.05, x=TRUE, y=TRUE)
reg.lm$testReal
reg.lm$testResponse-reg.lm$testReal
reg.lm$mse
reg.lm$nrmse

reg.ridge <- snn(lpsa~.,prostate,subset=s, method="ridge", hp=0.2, x=TRUE, y=TRUE)
reg.ridge$testReal
reg.ridge$testResponse-reg.ridge$testReal
reg.ridge$mse
reg.ridge$nrmse

reg.lasso <- snn(lpsa~.,prostate,subset=s, method="lasso", x=TRUE, y=TRUE)
reg.lasso$testReal
reg.lasso$testResponse-reg.lasso$testReal
reg.lasso$mse
reg.lasso$nrmse

#Boston housing
data(BostonHousing)
dim(BostonHousing)

s <- sample(nrow(BostonHousing),400)
reg.lm <- snn(medv~.,BostonHousing,subset=s, method="lm", hp= 0.1, x=TRUE, y=TRUE)
#reg.lm$testReal
#reg.lm$testResponse-reg.lm$testReal
reg.lm$mse
reg.lm$nrmse

reg.ridge <- snn(medv~.,BostonHousing,subset=s, method="ridge", hp=0.05, x=TRUE, y=TRUE)
reg.ridge$testReal
#reg.ridge$testResponse-reg.ridge$testReal
reg.ridge$mse
reg.ridge$nrmse

reg.lasso <- snn(medv~.,BostonHousing,subset=s, method="lasso", x=TRUE, y=TRUE)
reg.lasso$testReal
#reg.lasso$testResponse-reg.lasso$testReal
reg.lasso$mse
reg.lasso$nrmse


# PIMA
source('SNN.R')
pima <- read.table ("datasets/pima.dat", sep=",")
colnames(pima) <- c("Pregnancies","Plasma","Blood","Skin","Serum","BMI","Pedigree","Age","Target")
pima$Target <- factor(pima$Target, labels=c("No","Yes"))

Nlearn <- 500
learn <- 1:Nlearn

pima.glm <- snn(Target~.,pima,subset=learn, method="glm", hp=0.02)
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
pima2.glm <- snn(Target~.,pima,subset=learn, method="glm", simil.types=simil.types, hp=0.04)
pima2.glm$testContingencyTable
pima2.glm$testAccuracy

