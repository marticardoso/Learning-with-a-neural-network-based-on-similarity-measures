
# Tests fp_utils_allOpt
rm(list = ls())
set.seed(104)

# Set environment
setwd(".")


library(rattle.data)
library(faraway)
library(ggplot2)
library(mlbench)

source('SNN.R')
source('fp_utils.R')
source('benchmarkutils.R')

# Run a first time to generate Simils and W
set.seed(123)
s <- sample(nrow(prostate), 60)
snn.res <- snn(lpsa ~ ., prostate, subset = s, x = TRUE, y = TRUE)

gSimils <- snn.res$simil.matrix.prot
gT <- snn.res$y
# Objective function
r <- optimize_p_oneOpt(gSimils, gT, pInitial = 10.1)

## Optimize using optim function

#Define function to optimize
func <- function(args) {
  n <- length(args)
  p <- args[n]
  w <- args[1:n - 1]
  opt2.E.regression(p, w, gSimils, gT)
}

#Gradient function
grad <- function(args) {
  n <- length(args)
  p <- args[n]
  w <- args[1:n - 1]
  opt2.dE.regression(p, w, gSimils, gT)
}
initialValues <- c(numeric(ncol(gSimils) + 1) + 1, 100)
res <- optim(initialValues, func, grad, method = "BFGS")

# Compute the LM model
gDs <- data.frame(gSimils)
gDs$Target <- gT
lm.r <- lm(Target ~ ., gDs)
summary(lm.r)
func(c(coef(lm.r), 10000000))

r$newP
coef(lm.r)




initialValues <- c(numeric(ncol(gSimils) + 1) + 1, 100)
func(initialValues)
grad(initialValues)


res <- optim(initialValues, func, grad, method = "BFGS")




r <- optimize_p_oneOpt(gSimils, gT, pInitial = 10.1)



ini <- milisec()
for (i in 1:50) {
  r <- optimize_p_oneOpt(gSimils, gT, pInitial = 10.1)
}
fulltime <- milisec(ini)



#########################
# Binomial

gTFact <- factor(gT > 2)

r <- optimize_p_oneOpt(gSimils, gTFact, pInitial = 0.1)
r

#Define function to optimize
func <- function(args) {
  n <- length(args)
  p <- args[n]
  w <- args[1:n - 1]
  E.binomial(p = p, simils = gSimils, t = gTFact, w = w, reg = FALSE)
}

#Gradient function
grad <- function(args) {
  n <- length(args)
  p <- args[n]
  w <- args[1:n - 1]
  opt2.dE.binomial(p, w, gSimils, gTFact)
}
initialValues <- c(numeric(ncol(gSimils) + 1) + 1, 10000)
func(initialValues)
grad(initialValues)
res <- optim(initialValues, func, grad, method = "BFGS")

# Compute the LM model
gDs <- data.frame(gSimils)
gDs$Target <- gTFact
glm.r <- glm(Target ~ ., gDs, family = "binomial")
coef(glm.r)
round(res$par,2)
func(c(coef(glm.r), 10000000))
func(res$par)

