# Source code of the Mixture of Expert 
library(dplyr)
library(magrittr)
library(ggplot2)

# Function that predicts the output of the Ensemble of SNNs given an model (MoE)
MoE.predict <- function(object, x, snnX) {
  if (is.data.frame(x)) x <- data.matrix(x)
  if (is.data.frame(snnX)) snnX <- data.matrix(snnX)
  b <- object$b
  M <- ncol(b)
  o1 <- cbind(1, x) %*% b
  exp_o1 <- exp(o1)
  bettas <- exp_o1 / matrix(rep(rowSums(exp_o1), M), ncol = M)
  # Fix NaN
  conflictRules <- is.nan(rowSums(bettas))
  if (any(conflictRules)) {
    bettas[conflictRules,] <- t(apply(o1[conflictRules,, drop = FALSE], 1, function(row) as.numeric(row == max(row)) / sum(row == max(row))))
  }
  if (object$type == 'multinomial') {
    bettas <- bettas[, rep(1:ncol(bettas), each = object$numLevels)]
  }
  y <- bettas * snnX
  
  if (object$type == 'multinomial') {
    z <- matrix(0, nrow = nrow(x), ncol = object$numLevels)
    for (i in 1:object$numLevels) {
      tmp <- y[, (0:(ncol(b) - 1)) * object$numLevels + i]
      z[, i] <- rowSums(tmp)
    }
  }
  else {
    z <- rowSums(y)
  }
  z
}

# Functio that creates the MoE model (optimize)
MoE.optimize <- function(x, snnX, t, type, bIni = NULL, predByM = NULL) {
  # predByM: a list containing the features of x that can be used in each SNN
  m <- ncol(snnX)
  p <- ncol(x)
  x <- data.matrix(x)
  snnX <- data.matrix(snnX)

  if (type == "numeric") {
    MoE.E <- MoE.E.regression
    MoE.dE <- MoE.dE.regression
  }
  else if (type == "binomial") {
    MoE.E <- MoE.E.binomial
    MoE.dE <- MoE.dE.binomial
  }
  else if (type == "multinomial") {
    m <- m / nlevels(t)
    MoE.E <- MoE.E.multinomial
    MoE.dE <- MoE.dE.multinomial
  }
  else
    stop('Not yet implemented :)')

  if (is.null(predByM)) { # predByM is null, use all ps
    predByM <- list()
    for (i in 1:m)
      predByM[[i]] <- 1:p
  }

  # Given a vector of optimized arguments, it computes the corresponding matrix p x m (taking into account predByM)
  extractBFromArgs <- function(args) {
    b <- matrix(0, p + 1, m)
    ctr <- 1
    for (i in 1:length(predByM)) {
      nI <- length(predByM[[i]])
      b[1, i] <- args[ctr]
      ctr <- ctr+1
      b[1 + predByM[[i]], i] <- args[ctr:(ctr + nI - 1)]
      ctr <- ctr + nI
    }
    b
  }

  # Given a matrix, it returns a vector containing only the relevant coefficients.
  BToVector <- function(b) {
    r <- c()
    for (i in 1:length(predByM)) {
      r <- c(r, b[c(1, predByM[[i]] + 1), i])
    }
    r
  }

  # Objective function
  func <- function(args) {
    b2 <- extractBFromArgs(args)
    MoE.E(x = x, snnX = snnX, t = t, b = b2)
  }

  #Gradient function
  grad <- function(args) {
    b2 <- extractBFromArgs(args)
    r <- -(MoE.dE(x = x, snnX = snnX, t = t, b = b2))
    BToVector(r)
  }

  if (is.null(bIni)) bIni <- vector("numeric", length = length(unlist(predByM)) + m)
  res <- optim(bIni, func, grad, method = "BFGS")

  z <- list()
  z$b <- extractBFromArgs(res$par)
  z$type <- type
  if (type == "multinomial") {
    z$numLevels <- nlevels(t)
  }
  class(z) <- "MoE"
  z
}

## Below are the Error function and the derivatives used in the MoE

##
# Regression 
##

# b = n x m
MoE.E.regression <- function(x, snnX, t, b) {
  M <- ncol(b)
  o1 <- cbind(1, x) %*% b
  exp_o1 <- exp(o1)
  bettas <- exp_o1 / matrix(rep(rowSums(exp_o1), M), ncol = M)
  # Fix NaN
  conflictRules <- is.nan(rowSums(bettas))
  if (any(conflictRules)) {
    bettas[conflictRules,] <- t(apply(o1[conflictRules,, drop = FALSE], 1, function(row) as.numeric(row == max(row)) / sum(row == max(row))))
  }
  y <- bettas * snnX


  z <- list()
  z$E <- sum((rowSums(y) - t) ^ 2)
  z$pred <- y
  return(z$E)
}

MoE.dE.regression <- function(x, snnX, t, b) {

  M <- ncol(b)
  o1 <- cbind(1, x) %*% b
  exp_o1 <- exp(o1)
  sumRow_exp_o1 <- matrix(rep(rowSums(exp_o1), M), ncol = M)
  bettas <- exp_o1 / sumRow_exp_o1
  # Fix NaN
  conflictRules <- is.nan(rowSums(bettas))
  if (any(conflictRules)) {
    bettas[conflictRules,] <- t(apply(o1[conflictRules,, drop = FALSE], 1, function(row) as.numeric(row == max(row)) / sum(row == max(row))))
  }
  y <- bettas * snnX

  E <- (rowSums(y) - t)
  if (any(is.infinite(exp_o1))) exp_o1 <- o1
  sumRow_snn_exp_o1 <- rowSums(snnX * exp_o1)
  sumRow_exp_o1 <- rowSums(exp_o1)
  sumRow_exp_o1_2 <- sumRow_exp_o1 ^ 2


  E_sumRow_exp_o1__div__sumRow_exp_o1_2 <- E / sumRow_exp_o1
  E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2 <- E * sumRow_snn_exp_o1 / sumRow_exp_o1_2

  res <- -(t(cbind(1, x)) %*% (snnX * exp_o1 * matrix(rep(E_sumRow_exp_o1__div__sumRow_exp_o1_2, M), ncol = M))
  - t(cbind(1, x)) %*% (exp_o1 * matrix(rep(E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2, M), ncol = M)))
  return(res)
}

##
# Binomial 
##

MoE.E.binomial <- function(x, snnX, t, b) {
  M <- ncol(b)
  o1 <- cbind(1, x) %*% b
  exp_o1 <- exp(o1)
  bettas <- exp_o1 / matrix(rep(rowSums(exp_o1), M), ncol = M)
  # Fix NaN
  conflictRules <- is.nan(rowSums(bettas))
  if (any(conflictRules)) {
    bettas[conflictRules,] <- t(apply(o1[conflictRules,, drop = FALSE], 1, function(row) as.numeric(row == max(row)) / sum(row == max(row))))
  }
  y <- bettas * snnX
  y2 <- rowSums(y)

  if (is.logical(t)) isClass2 <- t
  else isClass2 <- as.numeric(t) == 2
  y2[!isClass2] <- 1 - y2[!isClass2]
  z <- ln(y2 + 1e-50) # 1e-50 -> fix ln(0)
  z <- -sum(z) / length(t)
  return(z)
}

MoE.dE.binomial <- function(x, snnX, t, b) {

  M <- ncol(b)
  o1 <- cbind(1, x) %*% b
  exp_o1 <- exp(o1)
  sumRow_exp_o1 <- matrix(rep(rowSums(exp_o1), M), ncol = M)
  bettas <- exp_o1 / sumRow_exp_o1
  # Fix NaN
  conflictRules <- is.nan(rowSums(bettas))
  if (any(conflictRules)) {
    bettas[conflictRules,] <- t(apply(o1[conflictRules,, drop = FALSE], 1, function(row) as.numeric(row == max(row)) / sum(row == max(row))))
  }
  y <- bettas * snnX
  y2 <- rowSums(y)
  if (is.logical(t)) isClass2 <- t
  else isClass2 <- as.numeric(t) == 2
  E <- numeric(length(t))
  E[!isClass2] <- -1 / (1 - y2[!isClass2] + 1e-50) # 1e-50 -> fix ln(0)
  E[isClass2] <- 1 / (y2[isClass2] + 1e-50) # 1e-50 -> fix ln(0)

  sumRow_snn_exp_o1 <- rowSums(snnX * exp_o1)
  sumRow_exp_o1 <- rowSums(exp_o1)
  sumRow_exp_o1_2 <- sumRow_exp_o1 ^ 2


  E_sumRow_exp_o1__div__sumRow_exp_o1_2 <- sumRow_exp_o1 * E / sumRow_exp_o1_2
  E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2 <- E * sumRow_snn_exp_o1 / sumRow_exp_o1_2

  res <- -(t(cbind(1, x)) %*% (snnX * exp_o1 * matrix(rep(E_sumRow_exp_o1__div__sumRow_exp_o1_2, M), ncol = M))
  - t(cbind(1, x)) %*% (exp_o1 * matrix(rep(E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2, M), ncol = M)))


  return(-res)
}

##
# Multinomial 
##

MoE.E.multinomial <- function(x, snnX, t, b) {
  M <- ncol(b)
  o1 <- cbind(1, x) %*% b
  exp_o1 <- exp(o1)
  bettas <- exp_o1 / matrix(rep(rowSums(exp_o1), M), ncol = M)
  # Fix NaN
  conflictRules <- is.nan(rowSums(bettas))
  if (any(conflictRules)) {
    bettas[conflictRules,] <- t(apply(o1[conflictRules,, drop = FALSE], 1, function(row) as.numeric(row == max(row)) / sum(row == max(row))))
  }

  snnXValidClass <- matrix(0, nrow = nrow(x), ncol = M)
  for (i in 1:nlevels(t)) {
    isClass <- t == levels(t)[i]
    snnXValidClass[isClass,] <- snnX[isClass, (0:(M - 1)) * nlevels(t) + i]
  }
  y <- bettas * snnXValidClass

  y2 <- rowSums(y)
  z <- -sum(ln(y2 + 1e-50)) # 1e-50 -> fix ln(0)
  return(z)
}

MoE.dE.multinomial <- function(x, snnX, t, b) {

  M <- ncol(b)
  o1 <- cbind(1, x) %*% b
  exp_o1 <- exp(o1)
  sumRow_exp_o1 <- matrix(rep(rowSums(exp_o1), M), ncol = M)
  bettas <- exp_o1 / sumRow_exp_o1
  # Fix NaN
  conflictRules <- is.nan(rowSums(bettas))
  if (any(conflictRules)) {
    bettas[conflictRules,] <- t(apply(o1[conflictRules,, drop = FALSE], 1, function(row) as.numeric(row == max(row)) / sum(row == max(row))))
  }

  snnXValidClass <- matrix(0, nrow = nrow(x), ncol = M)
  for (i in 1:nlevels(t)) {
    isClass <- t == levels(t)[i]
    snnXValidClass[isClass,] <- snnX[isClass, (0:(M - 1)) * nlevels(t) + i]
  }

  y <- bettas * snnXValidClass
  y2 <- rowSums(y)

  E <- 1 / (y2 + 1e-50) # 1e-50 -> fix ln(0)

  sumRow_snn_exp_o1 <- rowSums(snnXValidClass * exp_o1)
  sumRow_exp_o1 <- rowSums(exp_o1)
  sumRow_exp_o1_2 <- sumRow_exp_o1 ^ 2


  E_sumRow_exp_o1__div__sumRow_exp_o1_2 <- sumRow_exp_o1 * E / sumRow_exp_o1_2
  E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2 <- E * sumRow_snn_exp_o1 / sumRow_exp_o1_2

  res <- -(t(cbind(1, x)) %*% (snnXValidClass * exp_o1 * matrix(rep(E_sumRow_exp_o1__div__sumRow_exp_o1_2, M), ncol = M))
  - t(cbind(1, x)) %*% (exp_o1 * matrix(rep(E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2, M), ncol = M)))

  return(-res)
}

# Utils

ln <- function(v) {
  z <- v
  if (is.matrix(v) || (is.numeric(v) && length(v) > 1)) {
    z[v != 0] <- log(v[v != 0])
  }
  else if (is.numeric(v) && length(v) == 1) {
    if (v == 0) z <- 0
    else z <- log(v)
    }
  z
}