############
## Daisy Tests ###
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

###############
# Try with PIMA
###############
pima <- read.table ("datasets/pima.dat", sep=",")
colnames(pima) <- c("Pregnancies","Plasma","Blood","Skin","Serum","BMI","Pedigree","Age","Target")
pima$Target <- factor(pima$Target, labels=c("No","Yes"))

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

