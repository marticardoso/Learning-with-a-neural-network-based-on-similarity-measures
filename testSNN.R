############
## Tests ###
############

rm(list = ls())
set.seed (104)

# Set environment
setwd(".")


library(rattle.data)
library(faraway)


#First we load some useful function for the model selection task
source('SNN.R')

r1 <- snn(Type~.,wine,subset=sample(nrow(wine),100),method="glm")
r1$testTab
r2 <- snn(Type~.,wine,subset=sample(nrow(wine),100),method="ridge")
r2$testTab
r3 <- snn(Type~.,wine,subset=sample(nrow(wine),100),method="lasso")
r3$testTab

r4 <- snn(Type~.,wine,subset=sample(nrow(wine),100),method="glm", clust.method = "Uniform")
r4$testTab

response = predict(r1,wine,c("response","prob"))
response$response
table(response$response, wine$Type)

(pred <- predict (r1, wine[1:10,], type="response"))

#Test with logical / factor of two modalities
wine2 <- wine
wine2$Type <- NULL
wine2$Type1 <- wine$Type==1

r3 <- snn(Type1~.,wine2,subset=sample(nrow(wine),100), method="glm")
r3$testTab
pred <- predict(r3,wine2)

r4 <- snn(Type1~.,wine2,subset=sample(nrow(wine),100), method="ridge")
r4$testTab



# SNN.FIT
r4 <- snn.fit(wine[,-1],wine$Type)


# Snn.createmodel
df <-r1$dataframe
t <-snn.createClassificationModel(df, method="lasso")


# Numeric output
s <- sample(nrow(prostate),60)
r5 <- snn(lpsa~.,prostate,subset=s, method="lm")
r5$testReal
r5$testResponse-r5$testReal
r5$mse
