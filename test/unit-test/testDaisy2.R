# In this file, there are some unit tests that check the right execution of daisy2 functions

############
## Daisy Tests ###
############
rm(list = ls())
set.seed (104)

source('test/loadDatasets.R')
library(rattle.data)
library(faraway)
library(mlbench)

#First we load some useful function for the model selection task
source('SNN.R')

subWine <- wine[1:10,]
d1.Res <- daisy2(subWine, metric="gower", type=list())
(d1.Matrix <- as.matrix(d1.Res))

d2.Res <- daisy2.newObservations(subWine, d1.Res)
(d2.Matrix <- as.matrix(d2.Res))
d1.Matrix-d2.Matrix 
#OK

d3.Res <- daisy2.newObservations(wine[1:2,], d1.Res) #Sub dataset
(d3.Matrix <- as.matrix(d3.Res))
d1.Matrix[1:2,1:2]-d3.Matrix  
#OK

d4.Res <- daisy2.newObservations(wine[1:20,], d1.Res) #Larger dataset
(d4.Matrix <- as.matrix(d4.Res))
d1.Matrix-d4.Matrix[1:10,1:10]
#OK


tmp <- daisy2(wine[20:30,], metric = "gower", fixUnbalancedDiss = TRUE)
tmpV <- 1 - as.matrix(tmp)

predTmp <- daisy2.newObservations(wine[20:30,], attr(tmp, 'daisyObj'))
tmpV2 <- 1 - as.matrix(predTmp)
sum(tmpV - tmpV2) # Should be 0

predTmp <- daisy2.newObservations(wine[20:25,], attr(tmp, 'daisyObj'), wine[26:30,])
tmpV2 <- 1 - as.matrix(predTmp)
tmpV[7:11, 1:6] - tmpV2 # Should be 0 in the queried similarities

dObj <- daisy2_noComputation(wine[20:30,], metric = "gower", fixUnbalancedDiss = FALSE)

predTmp2 <- daisy2.newObservations(wine[20:25,], dObj, wine[26:30,])
tmpV3 <- 1 - as.matrix(sqrt(predTmp2))

sum(tmpV3 - tmpV2) # Should be 0

###############
# Try with PIMA
###############
pima <- LoadPimaDataset()$dataset
simil.types <- LoadPimaDataset()$simil.types

subDs <- pima[1:10,]
d1.Res <- daisy2(subDs, metric="gower", type=simil.types)
(d1.Matrix <- as.matrix(d1.Res))


d2.Res <- daisy2.newObservations(subDs, d1.Res)
(d2.Matrix <- as.matrix(d2.Res))
d1.Matrix-d2.Matrix 
#OK

d3.Res <- daisy2.newObservations(subDs[1:2,], d1.Res) #Sub dataset
(d3.Matrix <- as.matrix(d3.Res))
d1.Matrix[1:2,1:2]-d3.Matrix  
#OK

d4.Res <- daisy2.newObservations(pima[1:100,], d1.Res) #Larger dataset
(d4.Matrix <- as.matrix(d4.Res))
d1.Matrix-d4.Matrix[1:10,1:10]
#OK

