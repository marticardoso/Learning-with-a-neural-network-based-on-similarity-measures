library(e1071)
library(MASS)


a <- function(p) -1/4+sqrt((1/2)^4 + p)
fp <- function(x,p){
  if(x<=0.5) return(-p/((x-0.5)-a(p)) - a(p))
  else       return(-p/((x-0.5)+a(p)) + a(p) + 1)
}
#Derivates
da <- function(p) 1/(2*sqrt((1/2)^4 + p))
dfp <- function(x,p){
  if(x<=0.5) return((-((x-0.5) - a(p)) - p*da(p)) / (x-0.5-a(p))^2 - da(p))
  else       return((-((x-0.5) + a(p)) + p*da(p)) / (x-0.5+a(p))^2 + da(p))
}

#Apply fp and dfp to a matrix
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


E.func.from_model <- function(p, simils, t, model) {
  return(E.func(p, simils, t, extractCoefficients(model), isRegularization(model), model$lambda))
}

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
# Regression E #
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
# Binomial E #
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
  prob <- predict(model, data.frame(X), type = "response")
  response <- prob >= 0.5
  tab <- table(Truth = t, Pred = response)
  return(sum(diag(tab)) / sum(tab))
}

##############
# Multinom E #
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

  #nnetRes <- t(apply(X, 1, function(row) { # Optimized in the line above
  #  res <- exp(row)/sum(exp(row))
  # If NaN, take the maximum one. NaN caused by precision
  #  if(any(is.nan(res))) res <- as.numeric(row==max(row))/sum(row==max(row))
  #  return(res)
  #}))

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
  colnames(X) <- paste('X', colnames(X), sep = "")
  response <- predict(model, data.frame(X), type = "class")
  tab <- table(Truth = t, Pred = response)
  return(sum(diag(tab)) / sum(tab))
}

# Step 1 of method 1:
# Create a model given a p value (optimize w given a p value)
optimize_p_create_model_given_p <- function(simils, y, p, ..., trace=TRUE){
  learn.data <- data.frame(apply.fp(simils, p))
  learn.data$Target <- y
  if(is.factor(y) || is.logical(y)){
    model <- snn.createClassificationModel(learn.data, trace=FALSE,...)
  }
  else if(is.numeric(y)){
    model <- snn.createRegressionModel(learn.data, trace=FALSE, ...)
  }
  return(model)
}

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

# Try a range of p, and return the one with highest value
optimize_p_test_range_of_values <- function(simils, t, ps = NULL){
  if(is.null(ps)) ps = seq(0.01,2,0.01)
  
  E.res <- sapply(ps, function(p) {
    model <- optimize_p_create_model_given_p(simils,t,p)
    hatDiag <- influence(model)$hat
    E.val <- E.func.from_model(p, simils,t, model)*length(hatDiag)/sum((1-hatDiag)^2)
    return(E.val)
  })
  
  
  z <- list()
  z$bestP <- ps[which.min(E.res)[1]]
  z$ps <- ps
  z$ps.E <- E.res
  z
}

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


# Method that approch the best p
# Inputs: 
#   - dissim <- dissimalirity matrix
#   - pam.res <- result of pam
optimize_p_method3 <- function(dissim, pam.res, type="avg", ..., trace=TRUE){

  # Approach 1:
  #  Similar to Calinski-Harabasz index
  N <- sum(pam.res$clusinfo[,"size"])
  K <- length(pam.res$id.med)
  sW <- sum(pam.res$clusinfo[,"av_diss"]*pam.res$clusinfo[,"size"])/(N-K) # Mean diss to the cluster medoid
  sB <- sum(dissim[pam.res$id.med,pam.res$id.med])/(K-1)^2 # Mean diss between medoids
  coef1 <- 2*sW/sB
  if(type=="m1") return(coef1)
  
  # Approach 2
  # Mean of observation Silhouette coefficients
  avg.sil <- pam.res$silinfo$avg.width
  coef2 <- max(1 - avg.sil, 1e-3) # cannot be 0
  if(type=="m2") return(coef2)
  
  # Approach 3
  # Mean of mean silhuette coefficients by cluster
  # sil = a(i)-b(i)/max(a(i),b(i)) <- a(i)=mean distance all points, b(i) min mean distance to other cluster
  # sil [-1,1]
  avg.sil2 <- mean(pam.res$silinfo$clus.avg.widths)
  coef3 <- max(1 - avg.sil2, 1e-3) # cannot be 0
  if(type=="m3") return(coef3)
  
  
  # Method 4
  # (within-between) clusters coefficient (between all observations)
  method4 <- function(clust.id){
    ids <- which(pam.res$clustering==clust.id)
    withincluster <- sum(dissim[ids,ids])/(length(ids)-1)^2
    betweenclust <- mean(dissim[ids,-ids])
    r <- (betweenclust-withincluster) /max(betweenclust,withincluster)
    return(r)
  }
  coef4 <- 1 - mean(sapply(1:length(pam.res$id.med), method4))
  if(type=="m4") return(coef4)
  
  # Method 5
  # within-between clusters coefficient (between observations and medoids)
  method5 <- function(clust.id){
    ids <- which(pam.res$clustering==clust.id)
    withincluster <- pam.res$clusinfo[clust.id,"av_diss"]
    betweenclust <- mean(dissim[ids,pam.res$id.med[-clust.id]])
    r <- (betweenclust-withincluster) /max(betweenclust,withincluster)
    return(r)
  }
  coef5 <- 1 - mean(sapply(1:length(pam.res$id.med), method5))
  if(type=="m5") return(coef4)
  
  # Method 6
   # avg clust diameter /diameter (not valid)
  #diam <- max(dissim)
  #coef6 <- mean(pam.res$clusinfo[,"diameter"])/diam
  
  # Method 7
  # avg separation/diameter (not valid)
  #diam <- max(dissim)
  #coef7 <- 1-mean(pam.res$clusinfo[,"separation"])/diam
  
  # Method 8
  # avg separation/mean distance (not valid)
  #coef8 <- mean(pam.res$clusinfo[,"separation"]/mean(pam.res$clusinfo[,"max_diss"]))
  
  # Result
  # size
  # max_diss = max distance between obs i medoid
  # av_diss = av distance between obs and medoid
  # diameter = max distance between two obs
  z <- list()
  z$results <- c(coef1,coef2,coef3,coef4,coef5)
  names(z$results) <- paste('m', 1:(length(z$results)), sep="")
  z$avg <- mean(z$results)
  z
}

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
    E(p=p, simils=simils, t=t, w=w, reg=FALSE)
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
  if(z$newP < 1e-3) z$newP <- pInitial
  if (is.matrix(initialW)) z$w <- matrix(res$par[1:length(res$par) - 1], ncol = ncol(initialW))
  else z$w <- res$par[1:length(res$par) - 1]
  z$E <- res$value
  z
}
