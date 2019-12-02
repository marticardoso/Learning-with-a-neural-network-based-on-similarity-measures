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


MoE.E.multinomialOld <- function(x, snnX, t, b) {
  M <- ncol(b)
  o1 <- cbind(1, x) %*% b
  exp_o1 <- exp(o1)
  bettas <- exp_o1 / matrix(rep(rowSums(exp_o1), M), ncol = M)
  # Fix NaN
  conflictRules <- is.nan(rowSums(bettas))
  if (any(conflictRules)) {
    bettas[conflictRules,] <- t(apply(o1[conflictRules,, drop = FALSE], 1, function(row) as.numeric(row == max(row)) / sum(row == max(row))))
  }
  bettasExt <- bettas[, rep(1:ncol(bettas), each = nlevels(t))]
  y <- bettasExt * snnX


  tot <- array(0, dim = c(dim(bettas), nlevels(t)))
  for (i in 1:nlevels(t)) {
    tot[,, i] <- y[, (0:(ncol(bettas) - 1)) * nlevels(t) + i]
  }
  y2 <- apply(tot, c(1, 3), sum)

  #y2 <- rowSums(y)

  z <- numeric(length(t))
  for (i in 1:nlevels(t)) {
    isClass <- t == levels(t)[i]
    z[isClass] <- ln(y2[isClass, i] + 1)
  }
  z <- -sum(z)
  return(z)
}


MoE.dE.multinomialOld <- function(x, snnX, t, b) {

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
  bettasExt <- bettas[, rep(1:ncol(bettas), each = nlevels(t))]
  y <- bettasExt * snnX

  tot <- array(0, dim = c(dim(bettas), nlevels(t)))
  for (i in 1:nlevels(t)) {
    tot[,, i] <- y[, (0:(ncol(bettas) - 1)) * nlevels(t) + i]
  }
  y2 <- apply(tot, c(1, 3), sum)
  #y2 <- rowSums(y)
  snnXValidClass <- matrix(0, nrow = nrow(x), ncol = M)
  for (i in 1:nlevels(t)) {
    isClass <- t == levels(t)[i]
    snnXValidClass[isClass,] <- snnX[isClass, (0:(M - 1)) * nlevels(t) + i]
  }


  E <- numeric(length(t))
  for (i in 1:nlevels(t)) {
    isClass <- t == levels(t)[i]
    E[isClass] <- 1 / (y2[isClass, i] + 1)
    E[isClass && y2[, i] == 0] <- 0
  }

  sumRow_snn_exp_o1 <- rowSums(snnXValidClass * exp_o1)
  sumRow_exp_o1 <- rowSums(exp_o1)
  sumRow_exp_o1_2 <- sumRow_exp_o1 ^ 2


  E_sumRow_exp_o1__div__sumRow_exp_o1_2 <- sumRow_exp_o1 * E / sumRow_exp_o1_2
  E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2 <- E * sumRow_snn_exp_o1 / sumRow_exp_o1_2

  res <- -(t(cbind(1, x)) %*% (snnXValidClass * exp_o1 * matrix(rep(E_sumRow_exp_o1__div__sumRow_exp_o1_2, M), ncol = M))
  - t(cbind(1, x)) %*% (exp_o1 * matrix(rep(E_sumRow_snn_exp_o1__div__sumRow_exp_o1_2, M), ncol = M)))

  return(-res)
}
