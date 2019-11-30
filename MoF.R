library(dplyr)
library(magrittr)
library(ggplot2)

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
  y <- bettas * snnX
  z <- rowSums(y)
  z
}

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

MoE.dE.regression_for <- function(x, snnX, t, b) {

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

  sumRow_snn_exp_o1 <- rowSums(snnX * exp_o1)
  sumRow_exp_o1 <- rowSums(exp_o1)
  sumRow_exp_o1_2 <- sumRow_exp_o1 ^ 2

  res <- matrix(0, (p + 1), m)

  for (j in 1:m) {
    p1 <- snnX[, j] * exp_o1[, j] * sumRow_exp_o1
    p2 <- sumRow_snn_exp_o1 * exp_o1[, j]
    y <- (p1 - p2) / sumRow_exp_o1_2
    res[1, j] <- -sum(E * y)

  }
  for (j in 1:m) {
    for (k in 1:p) {
      p1 <- snnX[, j] * exp_o1[, j] * x[, k] * sumRow_exp_o1
      p2 <- sumRow_snn_exp_o1 * exp_o1[, j] * x[, k]
      y <- (p1 - p2) / sumRow_exp_o1_2
      res[k + 1, j] <- -sum(E * y)
    }
  }
  return(res)
}

MoE.dE.regression_semifor <- function(x, snnX, t, b) {

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

  sumRow_snn_exp_o1 <- rowSums(snnX * exp_o1)
  sumRow_exp_o1 <- rowSums(exp_o1)
  sumRow_exp_o1_2 <- sumRow_exp_o1 ^ 2

  snnX_exp_o1 <- snnX * exp_o1
  res <- matrix(0, (p + 1), m)

  E_sumRow_exp_o1__div__sumRow_exp_o1_2 <- sumRow_exp_o1 * E / sumRow_exp_o1_2
  E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2 <- E * sumRow_snn_exp_o1 / sumRow_exp_o1_2

  for (j in 1:m) {
    p1 <- snnX[, j] * exp_o1[, j] * E_sumRow_exp_o1__div__sumRow_exp_o1_2
    p2 <- exp_o1[, j] * E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2
    res[1, j] <- -sum(p1 - p2)

  }
  for (j in 1:m) {
    for (k in 1:p) {
      p1 <- snnX[, j] * exp_o1[, j] * x[, k] * E_sumRow_exp_o1__div__sumRow_exp_o1_2
      p2 <- exp_o1[, j] * x[, k] * E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2
      res[k + 1, j] <- -sum(p1 - p2)
    }
  }

  return(res)
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

  sumRow_snn_exp_o1 <- rowSums(snnX * exp_o1)
  sumRow_exp_o1 <- rowSums(exp_o1)
  sumRow_exp_o1_2 <- sumRow_exp_o1 ^ 2


  E_sumRow_exp_o1__div__sumRow_exp_o1_2 <- sumRow_exp_o1 * E / sumRow_exp_o1_2
  E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2 <- E * sumRow_snn_exp_o1 / sumRow_exp_o1_2

  res <- -(t(cbind(1, x)) %*% (snnX * exp_o1 * matrix(rep(E_sumRow_exp_o1__div__sumRow_exp_o1_2, M), ncol = M))
  - t(cbind(1, x)) %*% (exp_o1 * matrix(rep(E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2, M), ncol = M)))
  return(res)
}


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
  z <- numeric(length(t))
  z[!isClass2] <- ln(1 - y2[!isClass2])
  z[isClass2] <- ln(y2[isClass2])
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
  E[!isClass2] <- -1 / (1 - y2[!isClass2])
  E[isClass2] <- 1 / y2[isClass2]
  E[!isClass2 && y2 == 1] <- 0
  E[isClass2 && y2 == 0] <- 0

  sumRow_snn_exp_o1 <- rowSums(snnX * exp_o1)
  sumRow_exp_o1 <- rowSums(exp_o1)
  sumRow_exp_o1_2 <- sumRow_exp_o1 ^ 2


  E_sumRow_exp_o1__div__sumRow_exp_o1_2 <- sumRow_exp_o1 * E / sumRow_exp_o1_2
  E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2 <- E * sumRow_snn_exp_o1 / sumRow_exp_o1_2

  res <- -(t(cbind(1, x)) %*% (snnX * exp_o1 * matrix(rep(E_sumRow_exp_o1__div__sumRow_exp_o1_2, M), ncol = M))
  - t(cbind(1, x)) %*% (exp_o1 * matrix(rep(E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2, M), ncol = M)))


  return(-res)
}


MoE.optimize <- function(x, snnX, t, type, bIni=NULL) {
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
  else
    stop('Not yet implemented :)')


  func <- function(args) {
    b2 <- matrix(args, p + 1, m)
    MoE.E(x = x, snnX = snnX, t = t, b = b2)
  }

  grad <- function(args) {
    b2 <- matrix(args, p + 1, m)
    - as.vector(MoE.dE(x = x, snnX = snnX, t = t, b = b2))
  }

  if (is.null(bIni)) bIni <- vector("numeric", length = m * (p + 1))
  res <- optim(bIni, func, grad, method = "BFGS")

  z <- list()
  z$b <- matrix(res$par, p + 1, m)
  class(z) <- "MoE"
  z
}


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