## This file contains all functions related to the activation function (fp) and the way to set it
## (optimization procedure, k-fold-CV, GCV)

library(e1071)
library(MASS)

## Fp functions
a <- function(p) -1/4+sqrt((1/2)^4 + p)
fp <- function(x,p){
  if(x<=0.5) return(-p/((x-0.5)-a(p)) - a(p))
  else       return(-p/((x-0.5)+a(p)) + a(p) + 1)
}
# Derivatives
da <- function(p) 1/(2*sqrt((1/2)^4 + p))
dfp <- function(x,p){
  if(x<=0.5) return((-((x-0.5) - a(p)) - p*da(p)) / (x-0.5-a(p))^2 - da(p))
  else       return((-((x-0.5) + a(p)) + p*da(p)) / (x-0.5+a(p))^2 + da(p))
}

# Apply fp and dfp to a matrix
apply.fp <- function(X, p) {
  a_p <- a(p)
  fp_X <- -p / ((X - 0.5) - a_p) - a_p
  X_L05 <- X > 0.5
  fp_X[X_L05] <- -p / ((X[X_L05] - 0.5) + a_p) + a_p + 1
  return(fp_X)
}

apply.dfp <- function(X, p) {
  a_p <- a(p)
  da_p <- da(p)

  dfp_X <- (-(X - 0.5 - a_p) - p * da_p) / (X - 0.5 - a_p) ^ 2 - da_p
  X_U05 <- X > 0.5
  dfp_X[X_U05] <- (-(X[X_U05] - 0.5 + a_p) + p * da_p) / (X[X_U05] - 0.5 + a_p) ^ 2 + da_p
  return(dfp_X)
}

# Error function of a model with p and w
E.func <- function(p, simils, t, w, reg = FALSE, lambda = 0) {
  if (is.logical(t) || (is.factor(t) && nlevels(t) == 2))
    return(E.binomial(p, simils, t, w, reg, lambda))

  else if (is.factor(t))
    return(E.multinomial(p, simils, t, w, reg, lambda))

  else if (is.numeric(t))
    return(E.regression(p, simils, t, w, reg, lambda))

  else
    stop(gettextf("E func ('%s') is not supported.", class(t)))
  }

# Error function (for a GLM model)
E.func.from_model <- function(p, simils, t, model) {
  return(E.func(p, simils, t, extractCoefficients(model), isRegularization(model), model$lambda))
}

# Get accuracy/NRMSE of a model
accuracyOrNRMSE <- function(p, simils, t, model) {
  if (is.logical(t) || (is.factor(t) && nlevels(t) == 2))
    return(accuracy.binomial(p, simils, t, model))

  else if (is.factor(t))
    return(accuracy.multinomial(p, simils, t, model))

  else if (is.numeric(t))
    return(NRMSE.regression(p, simils, t, model))

  else
    stop(gettextf("accuracy func ('%s') is not supported.", class(t)))
  }

################
# Regression Error function (E) #
################

# E function (error) for regression= t - snn(x)
E.regression <- function(p, simils, t, w, reg = FALSE, lambda = 0) {
  if (p <= 0) return(NA)
  snn.res <- apply.fp(simils, p)
  snn.res <- cbind(1, snn.res) %*% w
  z <- 1 / 2 * (sum((t - snn.res) ^ 2)) / length(t)
  if (reg) z <- z + 1 / 2 * lambda * (sum(w[-1] ^ 2))
  return(z)
}


NRMSE.regression <- function(p, simils, t, model) {
  if (p <= 0) return(NA)
  w <- extractCoefficients(model)
  X <- apply.fp(simils, p)
  response <- cbind(1, X) %*% w
  z <- sum((t - response) ^ 2) / ((length(t) - 1) * var(t))
  return(z)
}

##############
# Binomial Error function (E) #
##############

E.binomial <- function(p, simils, t, w, reg = FALSE, lambda = 0) {
  if (p <= 0) return(NA)
  X <- apply.fp(simils, p)
  X <- cbind(1, X) %*% w
  nnRes <- sigmoid(X)
  if (is.logical(t)) isClass2 <- t
  else isClass2 <- as.numeric(t) == 2
  z <- numeric(length(t))
  z[!isClass2] <- ln(1 - nnRes[!isClass2] + 1e-50)
  z[isClass2] <- ln(nnRes[isClass2] + 1e-50)

  z <- -sum(z) / length(t)
  if (reg) z <- z + 1 / 2 * lambda * (sum(w ^ 2))
  return(z)
}

accuracy.binomial <- function(p, simils, t, model) {
  if (p <= 0) return(NA)
  w <- extractCoefficients(model)
  X <- apply.fp(simils, p)

  if (any(class(model) == "glmnet")) X <- as.matrix(X)
  else X <- data.frame(X)
  prob <- predict(model, X, type = "response")
  response <- prob >= 0.5
  tab <- table(Truth = t, Pred = response)
  return(sum(diag(tab)) / sum(tab))
}

##############
# Multinom Error function (E) #
##############

E.multinomial <- function(p, simils, t, w, reg = FALSE, lambda = 0) {
  if (p <= 0) return(NA)
  X <- apply.fp(simils, p)
  X <- cbind(1, X)
  X <- X %*% w
  if(nlevels(t)>ncol(w)) X <- cbind(0, X)

  exp_X <- exp(X)
  nnetRes <- exp_X / matrix(rep(rowSums(exp_X), nlevels(t)), ncol = nlevels(t))
  # Fix NaN
  conflictRules <- is.nan(rowSums(nnetRes))
  if (any(conflictRules)) {
    nnetRes[conflictRules,] <- t(apply(X[conflictRules,, drop = FALSE], 1, function(row) as.numeric(row == max(row)) / sum(row == max(row))))
  }

  z <- class.ind(t) * ln(nnetRes + 1e-50)
  z <- -sum(z) / length(t)
  if (reg) z <- z + 1 / 2 * lambda * (sum(w ^ 2))
  return(z)
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


accuracy.multinomial <- function(p, simils, t, model) {
  if (p <= 0) return(NA)
  w <- extractCoefficients(model)
  X <- apply.fp(simils, p)
  if (substring(colnames(X)[1], 1, 1) != 'X') colnames(X) <- paste('X', colnames(X), sep = "")
  if (any(class(model) == "glmnet")) X <- as.matrix(X)
  else X <- data.frame(X)
  response <- predict(model, X, type = "class")
  tab <- table(Truth = t, Pred = response)
  return(sum(diag(tab)) / sum(tab))
}


# Extract coefficients of the model
extractCoefficients <- function(model){
  w <- coef(model)  #Extract w
  if (any(class(model) == "glmnet")) {
    if (is.list(w)) w <- t(matrix(unlist(lapply(w, as.matrix)), ncol = length(w)))
    else w <- w[, 1]
  }
  # Remove X from names
  if(is.vector(w))
    names(w) <- gsub('X','',names(w)) 
  else if (is.matrix(w)) {
    colnames(w) <- gsub('X', '', colnames(w))
    w <- t(w)
  }
  return(w)
}

isRegularization <- function(model){
  gMOdel <<- model
  if(any(class(model)=="ridgelm")) return(TRUE)
  if(any(class(model)=="glmnet")) return(TRUE)
  return(FALSE)
}

######################
# VALUE OF P: BY GCV #
######################
optimize_p_GCV <- function(simils, y, control = NULL,regularization=FALSE,..., trace=TRUE){
  
  if(!is.numeric(y)) stop("Only regression model supported")
    
  ps <- seq(0.1,2,0.1)
  if(regularization) lambdas <- 10^seq(-5,2,length=50)
  else lambdas <- c(0)
  
  # Extract control info
  if(!is.null(control)){
    if(!is.null(control$ps)) ps <- control$ps
  }
  
  
  ps.ObjFunc <- numeric(length(ps))
  
  grid.search <- matrix(0,length(ps), length(lambdas))
  
  #Iterate for ps
  for(i in 1:length(ps)){
    ds <- data.frame(apply.fp(simils, ps[i]))
    ds$Target <- y

    r <- lm.ridge(Target~.,ds, lambda = lambdas)
    grid.search[i,] <- r$GCV
    
    lambda.id <- which.min(r$GCV)[1]
    best.lambda <- lambdas[lambda.id]
    grid.search[i,] <- r$GCV
    ps.ObjFunc[i] <- min(r$GCV)
    
    if(trace) cat("p= ", ps[i], " - ObjFunc: ", ps.ObjFunc[i], "\n")
  }
  
  z <- list()
  
  min.GCV <- which(grid.search == min(grid.search), arr.ind = TRUE)
  z$bestP <- ps[min.GCV[1]]
  z$lambda <- lambdas[min.GCV[2]]
  
  z$ps <- ps
  z$lambdas <- lambdas
  z$GCV <- min(grid.search)
  z$grid.search <- grid.search
  z
}

######################
# VALUE OF P: K-foldCV #
######################
# Optimize p using k fold cross validation method
optimize_p_kFoldCV <- function(simils, t, regularization = FALSE, control = NULL,..., trace=TRUE){
  # Extract control info
  ps <- seq(0.1, 2, 0.1)
  if (regularization) lambdas <- 10 ^ seq(-5, 2, length = 50)
  else lambdas <- c(0)
  kFolds <- 10
  useAccuracy <- TRUE
  if(!is.null(control)){
    if (!is.null(control$ps)) ps <- control$ps
    if (!is.null(control$lambdas)) lambdas <- control$lambdas
    if(!is.null(control$kFolds)) kFolds <- control$kFolds
    if(!is.null(control$useAccuracy)) useAccuracy <- control$useAccuracy
  }
  
  foldid <- sample(rep(seq(kFolds), length = length(t)))
  
  ps.ObjFunc <- matrix(0, length(ps), length(lambdas))
  ps.ObjFunc.sd <- matrix(0, length(ps), length(lambdas))
  
  #Iterate for ps
  for (i in 1:length(ps)) {
    #Iterate for lambdas
    for (j in 1:length(lambdas)) {
      foldRes <- numeric(kFolds)
      #Iterate for folds
      for (k in 1:kFolds) {
       
        # Split train-val
        simils_train <- simils[foldid != k,]
        t_train <- t[foldid != k]
        simils_val <- simils[foldid == k,]
        t_val <- t[foldid == k]

        model <- optimize_p_create_model_given_p(simils_train, t_train, ps[i], regularization = regularization, lambda = lambdas[j], ...)
        if (!useAccuracy) foldRes[k] <- E.func.from_model(ps[i], simils_val, t_val, model)
        else foldRes[k] <- accuracyOrNRMSE(ps[i], simils_val, t_val, model)
        }
      ps.ObjFunc[i, j] <- mean(foldRes)
      ps.ObjFunc.sd[i, j] <- sd(foldRes)

    }
    if (trace) cat("p= ", ps[i], "\n")
  }
  
  z <- list()
  minInd <- which(ps.ObjFunc == min(ps.ObjFunc), arr.ind = TRUE)
  z$bestP <- ps[minInd[1]]
  z$ps <- ps
  if (regularization) {
    z$lambdas <- lambdas
    z$bestLambda <- lambdas[minInd[2]]
  }
  z$E <- ps.ObjFunc
  z$E.sd <- ps.ObjFunc.sd
  z
}

# Create a model given a p value (optimize w given a p value)
optimize_p_create_model_given_p <- function(simils, y, p, ..., trace = TRUE) {
  learn.data <- data.frame(apply.fp(simils, p))
  learn.data$Target <- y
  if (is.factor(y) || is.logical(y)) {
    model <- snn.createClassificationModel(learn.data, trace = FALSE, ...)
  }
  else if (is.numeric(y)) {
    model <- snn.createRegressionModel(learn.data, trace = FALSE, ...)
  }
  return(model)
}

######################################
# VALUE OF P: OPTIMIZATION PROCEDURE!#
######################################
# Optimization procedure to set the value of p
optimize_p_oneOpt <- function(simils, t, pInitial = 0.1, ..., trace = TRUE) {

  if (is.numeric(t)) {
    E <- E.regression
    dE <- opt2.dE.regression
    initialW <- numeric(ncol(simils) + 1)
  }
  else if (is.logical(t) || (is.factor(t) && nlevels(t) == 2)) {
    E <- E.binomial
    dE <- opt2.dE.binomial
    initialW <- numeric(ncol(simils) + 1)
  }
  else if (is.factor(t) && nlevels(t) > 2) {
    E <- E.multinomial
    dE <- opt2.dE.multinomial
    initialW <- matrix(1, ncol(simils) + 1, nlevels(t) - 1)
  }
  else stop('Not implemented (fp_utils)')
  #Function to optimize
  func <- function(args) {
    n <- length(args)
    p <- args[n]
    if (is.matrix(initialW)) w <- matrix(args[1:n - 1], ncol = ncol(initialW))
    else w <- args[1:n - 1]
    E(p = p, simils = simils, t = t, w = w, reg = FALSE)
  }

  #Gradient function
  grad <- function(args) {
    n <- length(args)
    p <- args[n]
    if (is.matrix(initialW)) w <- matrix(args[1:n - 1], ncol = ncol(initialW))
    else w <- args[1:n - 1]
    dE(p, w, simils, t)
  }
  initialValues <- c(initialW, pInitial)
  res <- optim(initialValues, func, grad, method = "BFGS")
  z <- list()
  z$newP <- res$par[length(res$par)]
  if (z$newP < 1e-3) z$newP <- pInitial
  if (is.matrix(initialW)) z$w <- matrix(res$par[1:length(res$par) - 1], ncol = ncol(initialW))
  else z$w <- res$par[1:length(res$par) - 1]
  z$E <- res$value
  z
}

## Derivatives of the Error functions ##

opt2.dE.regression <- function(p, w, simils, t) {
  if (p <= 0) return(NA)
  simils <- as.matrix(simils)
  fp_X <- apply.fp(simils, p)
  snn.res <- cbind(1, fp_X) %*% w
  E <- (t - snn.res)

  # Compute dsnn : Weights
  dsnn.w <- fp_X * matrix(rep(E, ncol(fp_X)), ncol = ncol(fp_X))
  dsnn.w <- as.vector(-colSums(dsnn.w))
  dsnn.w0 <- -sum(E)
  # Compute dnn : p param
  dfp_X <- apply.dfp(simils, p)

  dsnn.p <- dfp_X %*% w[-1] # No intercept

  dsnn.p <- -sum(E * dsnn.p)
  res <- c(dsnn.w0, dsnn.w, dsnn.p) / length(t)

  return(res)
}

opt2.dE.binomial <- function(p, w, simils, t) {
  if (p <= 0) return(NA)
  # Compute net result
  fp_X <- apply.fp(simils, p)
  fp_X_w <- cbind(1, fp_X) %*% w
  snnRes <- sigmoid(fp_X_w)

  if (is.logical(t)) isClass2 <- t
  else isClass2 <- as.numeric(t) == 2
  # Compute dsnn : Weights
  nW <- length(w)
  dsnn.w <- matrix(rep(dsigmoid(fp_X_w), nW), ncol = nW) * cbind(1, fp_X)
  snnResColRep <- matrix(rep(snnRes, nW), ncol = nW)
  dsnn.w[!isClass2] <- -dsnn.w[!isClass2,] / (1 - snnResColRep[!isClass2,] + 1e-50)
  dsnn.w[isClass2] <- dsnn.w[isClass2,] / (snnResColRep[isClass2,] + 1e-50)
  dsnn.w <- colSums(dsnn.w)

  # Compute dnn : p param
  dfp_X <- apply.dfp(simils, p)
  dfp_X_w <- dfp_X %*% w[-1] # No intercpet
  dsnnRes <- dsigmoid(fp_X_w) * dfp_X_w  
  dsnn.p <- numeric(length(t))
  dsnn.p[!isClass2] <- -dsnnRes[!isClass2] / (1 - snnRes[!isClass2] + 1e-50)
  dsnn.p[isClass2] <- dsnnRes[isClass2] / (snnRes[isClass2] + 1e-50)

  dsnn.p <- sum(dsnn.p)

  res <- -c(dsnn.w, dsnn.p)
  res
}

opt2.dE.multinomial <- function(p, w, simils, t) {
  if (p <= 0) return(NA)
  # Compute net result
  fp_X <- apply.fp(simils, p)
  fp_X_withInt <- cbind(1, fp_X)
  fp_X_w <- fp_X_withInt %*% w
  fp_X_w <- cbind(0, fp_X_w)
  exp_X <- exp(fp_X_w)
  sumRow_exp_X <- matrix(rep(rowSums(exp_X), nlevels(t)), ncol = nlevels(t))
  nnetRes <- exp_X / sumRow_exp_X
  # Fix NaN
  conflictRules <- is.nan(rowSums(nnetRes))
  if (any(conflictRules)) {
    nnetRes[conflictRules,] <- t(apply(fp_X_w[conflictRules,, drop = FALSE], 1, function(row) as.numeric(row == max(row)) / sum(row == max(row))))
  }

  # Compute dsnn : Weights

  dsnn.w <- matrix(0, nrow(w), ncol(w))

  for (i in 2:nlevels(t)) {
    isClass <- t == levels(t)[i]
    p1 <- colSums(fp_X_withInt[isClass,, drop = FALSE])
    dsnn.w[, i - 1] <- dsnn.w[, i - 1] + p1
  }

  p2 <- t(fp_X_withInt) %*% nnetRes[, 2:nlevels(t)]
  dsnn.w <- dsnn.w - p2

  # Compute dnn : p param
  dfp_X <- apply.dfp(simils, p)
  dfp_X_w <- dfp_X %*% w[-1,] # No intercpet
  dfp_X_w <- cbind(0, dfp_X_w)
  dsnn.p <- sum(class.ind(t) * dfp_X_w) - sum(rowSums(exp_X * dfp_X_w) / rowSums(exp_X))

  res <- -c(dsnn.w, dsnn.p)
  res
}



