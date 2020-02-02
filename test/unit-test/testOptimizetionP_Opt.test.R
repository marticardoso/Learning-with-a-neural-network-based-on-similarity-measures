# In this file, there are some unit tests that check the right execution of the Optimization procedure when setting p
# (this is not an experiment)

##################
# Testing for Optimization of p : Opt method (optimize w and p at the same time)
#
# File: fp_utils.R

rm(list = ls())
set.seed(104)

library(rattle.data)
library(faraway)
library(ggplot2)
library(mlbench)

source('fp_utils.R')
source('SNN.R')

###################
# Regression case #
###################

# Run a first time to generate Simils and T
set.seed(123)
s <- sample(nrow(prostate), 60)
snn.res <- snn(lpsa ~ ., prostate, subset = s, x = TRUE, y = TRUE)

gSimils <- as.matrix(snn.res$learn.data[, - ncol(snn.res$learn.data)])
gT <- snn.res$y

# Run full optimization
r <- optimize_p_oneOpt(gSimils, gT, pInitial = 10.1)
r

# Optimize using optim function

# Function to optimize
func <- function(args) {
  n <<- length(args)
  p <<- args[n]
  w <<- args[1:n - 1]
  E.regression(p=p, w=w, simils=gSimils, t=gT)
}
#Gradient
grad <- function(args) {
  n <- length(args)
  p <- args[n]
  w <- args[1:n - 1]
  opt2.dE.regression(p, w, gSimils, gT)
}
initialValues <- c(numeric(ncol(gSimils) + 1) + 1, 100)
res <- optim(initialValues, func, grad, method = "BFGS")
res

# Compare with LM model
gDs <- data.frame(gSimils)
gDs$Target <- gT
lm.r <- lm(Target ~ ., gDs)
#Compare obj function (similar results)
func(c(coef(lm.r), 10000000))
res$value
# Compare w (similar results)
coef(lm.r)
res$par

# Test with low p
r <- optimize_p_oneOpt(gSimils, gT, pInitial = 0.1)
r


#################
# Binomial case #
#################

# Use same dataset, transform to a factor
gTFact <- factor(gT > 2)

# Run full optimization
r <- optimize_p_oneOpt(gSimils, gTFact, pInitial = 0.1)
r

# Optimize using optim function

# Function to optimize
func <- function(args) {
  n <- length(args)
  p <- args[n]
  w <- args[1:n - 1]
  E.binomial(p = p, simils = gSimils, t = gTFact, w = w, reg = FALSE)
}

# Gradient function
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
res
# Compare with GLM model
gDs <- data.frame(gSimils)
gDs$Target <- gTFact
glm.r <- glm(Target ~ ., gDs, family = "binomial")

coef(glm.r)
round(res$par, 2) # Similar coefficients

func(c(coef(glm.r), 10000))
func(res$par) # Similar obj func



####################
# Multinomial case #
####################

# Transform to a factor
gT2 <- gT
gT2[gT < 2] <- 2
gT2[gT >4] <- 4
gTFact <- factor(round(gT2))

# Run full optimization
r <- optimize_p_oneOpt(gSimils, gTFact, pInitial = 0.1)
r

# Optimize using optim function

# Function to optimize
func <- function(args) {
  n <- length(args)
  p <- args[n] 
  w <- matrix(args[1:n - 1], ncol = nlevels(gTFact) - 1)
  E.multinomial(p = p, simils = gSimils, t = gTFact, w = w, reg = FALSE)
}

# Gradient function
grad <- function(args) {
  n <- length(args)
  p <- args[n]
  w <- matrix(args[1:n - 1], ncol = nlevels(gTFact) - 1)
  opt2.dE.multinomial(p, w, gSimils, gTFact)
}
initialValues <- c(numeric((ncol(gSimils) + 1)*(nlevels(gTFact)-1)) + 1, 10000)
func(initialValues)
grad(initialValues)
res <- optim(initialValues, func, grad, method = "BFGS")
res$value
res$par
func(res$par)
round(grad(res$par), 4)

# Compare with the MULTINOMIAL model
gDs <- data.frame(gSimils)
gDs$Target <- gTFact
mult.r <- multinom(Target ~ ., gDs)
coef(mult.r)
round(matrix(res$par[1:length(res$par) - 1], nrow = 2), 2) # Similar results

func(c(t(coef(mult.r)), 10000))
func(res$par)
round(grad(res$par),4) # Small gradients

# Check with p = 0.1
initialValues <- c(t(coef(mult.r)), 0.1)
func(initialValues)
res <- optim(initialValues, func, grad, method = "BFGS")
res$value
res$par
