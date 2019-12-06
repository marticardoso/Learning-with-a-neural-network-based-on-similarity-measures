

rm(list = ls())
set.seed (104)

# Set environment
setwd(".")


library(rattle.data)
library(faraway)
library(mlbench)

#First we load some useful function for the model selection task
source('SNN.R')


#Regression

data(Ozone)
summary(Ozone)

data(Servo)
summary(Servo)


sampleTwoThirds <- function(ds){
  sample(nrow(ds),floor(2/3*nrow(ds)))
}

#####################
## AIRFOIL DATASET ##
#####################

ds1 <- read.table('./datasets/regression/airfoil_self_noise.dat')
colnames(ds1) <- c('frequency', 'angle', 'chord', 'velocity', 'thickness', 'pressure')
s <- sampleTwoThirds(ds1)
snn.ds1 <- snn(pressure~.,ds1,subset=s,regularization=FALSE, p.control=list(method='Opt'))
snn.ds1$nrmse

#Compare to only lm
lm.ds1 <- lm(pressure~.,ds1)
1-summary(lm.ds1)$r.squared


#####################
## Physicochemical Properties of Protein Tertiary Structure DATASET ##
#####################

ds2 <- read.csv('./datasets/regression/CASP.csv')
s.d2 <- sampleTwoThirds(ds2)
#Timeout error
#snn.ds2 <- snn(RMSD~.,ds2,subset=s.d2,regularization=FALSE)
snn.ds2$nrmse

#Compare to only lm
lm.ds2 <- lm(RMSD~.,ds2)
1-summary(lm.ds2)$r.squared



#####################
## SkillCraft1 Master Table Dataset ##
#####################

ds3 <- read.csv('./datasets/regression/SkillCraft1_Dataset.csv')
s.d3 <- sampleTwoThirds(ds3)
#Timeout error
snn.ds3 <- snn(LeagueIndex ~ ., ds3, subset = s.d3, regularization = FALSE)
snn.ds3$nrmse

#Compare to only lm
lm.ds3 <- lm(LeagueIndex~.,ds3)
1-summary(lm.ds3)$r.squared

#####################
## Combined Cycle Power Plant Data Set ##
#####################
# Nrows: 9568
library("xlsx")
ds4 <- read.xlsx('./datasets/regression/CCPP/Folds5x2_pp.xlsx',1)
s.d4 <- sampleTwoThirds(ds4)
#Timeout error
#snn.ds4 <- snn(PE~.,ds4,subset=s.d4,regularization=FALSE)
snn.ds4$nrmse

#Compare to only lm
lm.ds4 <- lm(LeagueIndex~.,ds3)
1-summary(lm.ds4)$r.squared

#####################
## Forest Fires Data Set ##
#####################

ds5 <- read.csv('./datasets/regression/forestfires.csv')
s.d5 <- sampleTwoThirds(ds5)
#Timeout error
snn.ds5 <- snn(log(area+1)~.,ds5,subset=s.d5,regularization=FALSE)
snn.ds5$nrmse

#Compare to only lm
lm.ds5 <- lm(log(area+1)~.,ds5)
1-summary(lm.ds5)$r.squared


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
## Concrete Compressive Strength Data Set ##
#####################
library("readxl")
ds7 <- read.csv('./datasets/regression/winequality-white.csv', dec='.',sep=';')
ds7 <- read.csv('./datasets/regression/winequality-red.csv', dec = '.', sep = ';')
s.d7 <- sampleTwoThirds(ds7)
#Timeout error
snn.ds7 <- snn(quality~.,ds7,subset=s.d7,regularization=FALSE, p.control=list(method='Opt2'))
snn.ds7$nrmse

#Compare to only lm
lm.ds7 <- lm(quality~.,ds7)
1-summary(lm.ds7)$r.squared



