library(e1071)
library(MASS)

source("GD.R")

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

E.func <- function(p, simils, t, w, reg=FALSE,lambda = 0) {
  if(is.logical(t) ||(is.factor(t)&&nlevels(t)==2))
    return(E.binomial(p,simils,t,w,reg,lambda))
  
  else if(is.factor(t))
    return(E.multinomial(p,simils,t,w,reg,lambda))
  
  else if(is.numeric(t))
    return(E.regression(p,simils,t,w,reg,lambda))
  
  else 
    stop(gettextf("E func ('%s') is not supported.", class(t)))
}

dE.func <- function(p, simils, t, w) {
  if(is.logical(t)||(is.factor(t)&&nlevels(t)==2))
    return(dE.binomial(p,simils,t,w))
  
  else if(is.factor(t))
    return(dE.multinomial(p,simils,t,w))
  
  else if(is.numeric(t))
    return(dE.regression(p,simils,t,w))
  
  else 
    stop(gettextf("dE func ('%s') is not supported.", class(t)))
}

E.func.from_model <- function(p, simils, t, model) {
  return(E.func(p,simils,t,extractCoefficients(model), isRegularization(model), model$lambda))
}

dE.func.from_model <- function(p, simils, t, model) {
  return(dE.func(p,simils,t,extractCoefficients(model)))
}

accuracyOrNRMSE <- function(p, simils, t, model){
  if(is.logical(t) ||(is.factor(t)&&nlevels(t)==2))
    return(accuracy.binomial(p,simils,t, model))
  
  else if(is.factor(t))
    return(accuracy.multinomial(p,simils,t,model))
  
  else if(is.numeric(t))
    return(NRMSE.regression(p,simils,t,model))
  
  else 
    stop(gettextf("accuracy func ('%s') is not supported.", class(t)))
}

################
# Regression E #
################

# E function (error) for regression= t - snn(x)
E.regression <- function(p, simils, t, w, reg=FALSE, lambda = 0) {
  if(p<=0) return(NA)
  
  snn.res <- apply(simils[,names(w[-1])], c(1,2), function(x) fp(x,p))
  snn.res <- cbind(1,snn.res) %*% w
  
  z <- 1/2*(sum((t - snn.res)^2))/length(t)

  if(reg) z <- z + 1/2*lambda * (sum(w[-1]^2))
  return(z)
}

dE.regression <- function(p, simils, t, w) {
  if(p<=0) return(NA)
  
  snn.res <- apply(simils[,names(w[-1])], c(1,2), function(x) fp(x,p))
  snn.res <- cbind(1,snn.res) %*% w
 
  dsnn.res <- apply(simils[,names(w[-1])], c(1,2), function(x) dfp(x,p))
  dsnn.res <- dsnn.res %*% w[-1] # No intercept
  
  res <- -(1/length(t)) *sum((t - snn.res)*dsnn.res)
  
  return(res)
}

NRMSE.regression <- function(p, simils, t, model){
  if(p<=0) return(NA)
  
  w <- extractCoefficients(model)
  prototypes <- names(w[-1])
  
  X <- apply(simils[,prototypes], c(1,2), function(x) fp(x,p))
  response <- cbind(1,X) %*% w
  #response <- predict (model, data.frame(X))
  z <- sum((t - response)^2) / ((length(t)-1)*var(t))
  return(z)
}

##############
# Binomial E #
##############


E.binomial <- function(p, simils, t, w, reg=FALSE, lambda = 0){
  if(p<=0) return(NA)
  
  prototypes <- names(w)[-which(names(w) %in% c("(Intercept)"))]
  
  X <- apply(simils[,prototypes], c(1,2), function(x) fp(x,p))
  X <- cbind(1, X) %*% w
  nnRes <- sigmoid(X)
  
  z <- class.ind(t) * ln(cbind(1-nnRes, nnRes)) 
  z <- -sum(z)/length(t)
  if(reg) z <- z + 1/2*lambda * (sum(w^2))
  return(z)
}

dE.binomial <- function(p, simils, t, w){
  if(p<=0) return(NA)
  
  prototypes <- names(w)[-which(names(w) %in% c("(Intercept)"))]
  # Compute net result
  X <- apply(simils[,prototypes], c(1,2), function(x) fp(x,p))
  X <- cbind(1, X) %*% w #  w has intercept
  nnRes <- sigmoid(X)
  # Compute net derivative
  dX <- apply(simils[,prototypes], c(1,2), function(x) dfp(x,p))
  wNoInter <- w[-which(names(w) %in% c("(Intercept)"))] # Remove intercept
  dX <- dX %*% wNoInter # No intercpet
  dnnRes <- dsigmoid(X) * dX
  
  z <- class.ind(t) * cbind(-dnnRes/(1-nnRes), dnnRes/nnRes) #z <- t*dnnRes/nnRes - (1-t)*dnnRes/(1-nnRes)
  
  z[(1-nnRes)==0,1] <- 0
  z[nnRes == 1,  2] <- 0
  #Fix NaN
  #z[is.nan(z)] <- 0
  
  return(-sum(z)/length(t))
}

accuracy.binomial <- function(p, simils, t, model){
  if(p<=0) return(NA)
  
  w <- extractCoefficients(model)
  prototypes <- names(w)[-which(names(w) %in% c("(Intercept)"))]
  
  X <- apply(simils[,prototypes], c(1,2), function(x) fp(x,p))
  prob <- predict (model, data.frame(X), type="response")
  response <- prob>=0.5
  tab <- table(Truth=t,Pred=response)
  return(sum(diag(tab))/sum(tab))
}

##############
# Multinom E #
##############

E.multinomial <- function(p, simils, t, w, reg=FALSE, lambda = 0){
  if(p<=0) return(NA)
  
  prototypes <- colnames(w)[-which(colnames(w) %in% c("(Intercept)"))]
  X <- apply(simils[,prototypes], c(1,2), function(x) fp(x,p))
  X <- cbind(1, X)
  X <- X %*% t(w)
  X <- cbind(0,X)
  nnetRes <- t(apply(X, 1, function(row) {
    res <- exp(row)/sum(exp(row))
    # If NaN, take the maximum one. NaN caused by precision
    if(any(is.nan(res))) res <- as.numeric(row==max(row))/sum(row==max(row))
    return(res)
  }))
  
  #ToDo: fix Na
  z<-class.ind(t)*ln(nnetRes)
  z<- -sum(z)/length(t)
  if(reg) z <- z + 1/2*lambda * (sum(w^2))
  return(z)
}

ln <- function(v){
  z <- v
  if(is.matrix(v)|| (is.numeric(v) && length(v) > 1)){
    z[v!=0] <- log(v[v!=0])
  }
  else if(is.numeric(v) && length(v) == 1){
    if(v==0) z <-0
    else z <- log(v)
  }
  z
}

dE.multinomial <- function(p, simils, t, w){
  if(p<=0) return(NA)
  
  prototypes <- colnames(w)[-which(colnames(w) %in% c("(Intercept)"))]
  
  
  X <- apply(simils[,prototypes], c(1,2), function(x) fp(x,p))
  X <- cbind(1, X) # Add intercept column
  X <- X %*% t(w) #  w has intercept
  X <- cbind(0,X) # Added base class
  
  dX <- apply(simils[,prototypes], c(1,2), function(x) dfp(x,p))
  wNoInter <- w[,-which(colnames(w) %in% c("(Intercept)"))] # Remove intercept
  dX <- dX %*% t(wNoInter) # No intercpet
  dX <- cbind(0,dX) # First class has 0 derivative
  
  nnetRes <- t(apply(X, 1, function(r) exp(r)/sum(exp(r))))
  
  dnnetRes <- matrix(0,dim(nnetRes)[1], dim(nnetRes)[2])
  for(i in 1:nrow(dnnetRes)){
    dnnetRes[i,] <- (sum(exp(X[i,]))*exp(X[i,])*dX[i,]-exp(X[i,])*sum(exp(X[i,])*dX[i,]))/(sum(exp(X[i,])))^2
  }
  
  z <- class.ind(t)*dnnetRes/nnetRes
  return(-sum(z)/length(t))
}

accuracy.multinomial <- function(p, simils, t, model){
  if(p<=0) return(NA)
  
  w <- extractCoefficients(model)
  prototypes <- colnames(w)[-which(colnames(w) %in% c("(Intercept)"))]
  X <- apply(simils[,prototypes], c(1,2), function(x) fp(x,p))
  colnames(X) <- paste('X', colnames(X), sep="")
  response <- predict (model, data.frame(X), type="class")
  tab <- table(Truth=t,Pred=response)
  return(sum(diag(tab))/sum(tab))
}


##############
# Optimize p #
##############
# maxIter <- maximum number of iterations
# pToler <- p change tolerance
# objToler <- objective function change tolerance

optimize_p <- function(x.simils,y, pInitial= NULL, control=NULL,...){
  cat('#Optimization of p#\n')
  
  #Initialize parameters
  maxIter <- 100
  pToler <- 1e-6
  objFuncToler <-1e-6
  validation <- TRUE
  val.subset <- NULL
  if(!is.null(control)){
    if(!is.null(control$maxIter)) maxIter <- control$maxIter
    if(!is.null(control$pToler)) pToler <- control$pToler
    if(!is.null(control$objFuncToler)) objFuncToler <- control$objFuncToler
    if(!is.null(control$validation)) validation <- control$validation
    if(!is.null(control$val.subset)) val.subset <- control$val.subset
  }
  # Set validation and train sets
  if(validation){
    if(is.null(val.subset)){
      n<-nrow(x.simils)
      val.subset<-sample(1:n)[1:floor(n/3)]
    }
    x.simils.val <- x.simils [val.subset,]
    x.simils <- x.simils[-val.subset,]
    y.val <- y[val.subset]
    y <- y[-val.subset]
  }
  else{ x.simils.val <- y.val <- NULL}
  
  ps.evol <- c()
  E.learn.evol <- c()
  E.val.evol <- c()
  
  # If null, initialize p
  if(is.null(pInitial)){
    r <- optimize_p_initializeP(x.simils,y, x.simils.val, y.val,...)
    pInitial <- r$p
  }
  cat('pInitial = ', pInitial, '\n')
  
  bestP <- pInitial
  bestObjFunc <- 1e50
  iter = 1
  while (iter < maxIter){
    #Step 1
    model <- optimize_p_create_model_given_p(x.simils, y, bestP,...)
    print.optimizationLog_step1(iter, bestP, x.simils, y,model, x.simils.val, y.val)
    
    #Step 2
    optRes <- optimize_p_given_model(x.simils, y, model, bestP, simils.val = x.simils.val, t.val = y.val)
    print.optimizationLog_step2(iter, optRes,x.simils, y, model, x.simils.val, y.val)
    newP <- optRes$newP
    
    # Store evolutions
    ps.evol <- c(ps.evol,newP)
    E.learn.evol <- c(E.learn.evol, optRes$E)
    if(!is.null(x.simils.val)) E.val.evol <- c(E.val.evol, E.func.from_model(newP, x.simils.val, y.val, model))
    
    # Stopping criteria
    shouldBreak <- FALSE
    if((abs(bestP-newP) < pToler)){   shouldBreak <- TRUE; cat('Stopping criteria: p tolerance\n') }
    if((abs(bestObjFunc-optRes$E) < objFuncToler)){   shouldBreak <- TRUE; cat('Stopping criteria: obj func tolerance\n')}
    if(iter+1 == maxIter) cat('Stopping criteria: maxIterations\n')
    
    #Update fields
    bestP <- newP
    bestObjFunc <- optRes$E
    iter = iter + 1
    if(shouldBreak) break
  }
  z <- list()
  z$bestP <- bestP
  z$simils <- x.simils
  z$model <- model
  z$y <- y
  z$ps.evol <- ps.evol
  z$E.learn.evol <- E.learn.evol
  z$E.val.evol <- E.val.evol
  z
  
}

print.optimizationLog_step1 <- function(iter, bestP, x.simils, y,model, x.simils.val = NULL, y.val = NULL){
  cat('Iter', iter, '(1) p =', bestP)
  cat(' (E[learn]:',E.func.from_model(bestP, x.simils, y, model))
  if(!is.null(x.simils.val) && !is.null(y.val))
    cat(', E[val]:',E.func.from_model(bestP, x.simils.val, y.val, model))
  cat(', model:', getModelObjFunction(model))
  if(is.numeric(y)) accOrNrmseText <- "NRMSE"
  else accOrNrmseText <- "Acc"
  cat(', ', accOrNrmseText,'[learn]: ', accuracyOrNRMSE(bestP, x.simils, y, model), sep="")
  if(!is.null(x.simils.val) && !is.null(y.val))
    cat(', ', accOrNrmseText,'[val]: ', accuracyOrNRMSE(bestP, x.simils.val, y.val, model), sep="")
  #cat('Coefs: ', coef(model), "\n" )
  #cat('lambda: ', model$lambda, '\n')
  cat(')\n')
}

print.optimizationLog_step2 <- function(iter, optRes, x.simils, y,model, x.simils.val = NULL, y.val = NULL){
  p <- optRes$newP
  cat('Iter', iter, '(2) p =', p)
  cat(' (E[learn]:',E.func.from_model(p, x.simils, y, model))
  if(!is.null(x.simils.val) && !is.null(y.val))
    cat(', E[val]:',E.func.from_model(p, x.simils.val, y.val, model))
  
  cat(', optRes:', optRes$E)
  if(is.numeric(y)) accOrNrmseText <- "NRMSE"
  else accOrNrmseText <- "Acc"
  cat(', ',accOrNrmseText,'[learn]: ', accuracyOrNRMSE(p, x.simils, y, model), sep="")
  if(!is.null(x.simils.val) && !is.null(y.val))
    cat(', ',accOrNrmseText,'[val]: ', accuracyOrNRMSE(p, x.simils.val, y.val, model), sep="")
  cat(')\n')
}

# Try several initial p, and take the best one.
optimize_p_initializeP <- function(x.simils,y, x.simils.val = NULL, y.val = NULL,...){
  cat('Computing pInitial\n')
  pInitials <- seq(0.1,2,0.1)
  E.pInitials <- numeric(length(pInitials)) 
  for(i in 1:length(pInitials)){
    model <- optimize_p_create_model_given_p(x.simils, y, pInitials[i],...)
    if(!is.null(x.simils.val)) 
      E.pInitials[i] <- E.func.from_model(pInitials[i], x.simils.val, y.val, model)
    else 
      E.pInitials[i] <- E.func.from_model(pInitials[i], x.simils, y, model)
  }
  z <- list()
  z$p <- pInitials[which.min(E.pInitials)][1]
  z$E <- E.pInitials
  z
}

getModelObjFunction <- function(model){
  if(class(model)[1]=="glm"){
    return(model$deviance/2 /length(model$fitted.values))
  }
  if(class(model)[1]=="multinom"){
    return(model$value)
  }
  if(class(model)[1] == "lm"){
    return(mse(model$residuals))
  }
  if(any(class(model)=="glmnet")){
    return(-1)
  }
  return(0)
}

mse <- function(residuals) mean(residuals^2)

# Step 1 of method 1:
# Create a model given a p value (optimize w given a p value)
optimize_p_create_model_given_p <- function(simils, y, p, ...){
  learn.data <- data.frame(apply(simils, c(1,2), function(x) fp(x,p)))
  learn.data$Target <- y
  if(is.factor(y) || is.logical(y)){
    model <- snn.createClassificationModel(learn.data, p=p, trace=FALSE,...)
  }
  else if(is.numeric(y)){
    model <- snn.createRegressionModel(learn.data, trace=FALSE,p=p, ...)
  }
  return(model)
}

extractCoefficients <- function(model){
  w <- coef(model)  #Extract w
  if(any(class(model)=="glmnet")) w <- w[,1]
  # Remove X from names
  if(is.vector(w))
    names(w) <- gsub('X','',names(w)) 
  else if(is.matrix(w))
      colnames(w) <- gsub('X','',colnames(w)) 
  return(w)
}

isRegularization <- function(model){
  gMOdel <<- model
  if(any(class(model)=="ridgelm")) return(TRUE)
  if(any(class(model)=="glmnet")) return(TRUE)
  return(FALSE)
}

# Step 2 of method 1:
# Function that given a model/w vector, optimize the p value
optimize_p_given_model <- function(simils, t, model, pInitial = 0.1, simils.val = NULL, t.val = NULL, 
                                   useNMSE=TRUE, useAccuracy = FALSE, GD.control=NULL){
  w <- extractCoefficients(model)
  isReg <- isRegularization(model)
  lambda <- model$lambda
  
  #Function to optimize
  func <- function(p) E.func(p, simils, t, w, isReg, lambda)/var(t)*2
  #Gradient
  grad <- function(p) dE.func(p, simils, t, w)/var(t)*2
  
  GD.control <- list(alpha=1, maxIter=100)
  gdRes <- GD(f =  func,g = grad, x = pInitial, control=GD.control)

  z <- list()
  z$newP <- gdRes$x
  z$E <- gdRes$fx
  # Test set
  if(!is.null(simils.val)){
    
    func.val <- function(p) E.func(p, simils.val, t.val, w, isReg, lambda)/var(t)*2
    func2.val <- function(p) accuracyOrNRMSE(p, simils.val,t.val, model)
    
    grad.val <- function(p) dE.func(p, simils.val, t.val, w)
    
    if(!useAccuracy && !useNMSE) ps.E.val <- sapply(gdRes$xtrace, func.val)
    else ps.E.val <- sapply(gdRes$xtrace, func2.val)
    
    best.E.id <- which.min(ps.E.val)[1]
    z$p.val <-  gdRes$xtrace[best.E.id]
    z$E.val <- ps.E.val[best.E.id]
    cat(' new p [train]:',gdRes$x, ' new p [val]:', z$p.val, '\n')
    z$E <- z$E.val
    z$newP <- z$p.val
    
    gFuncVal <<- func.val
    gFunc2Val <<- func2.val
    gGradVal <<- grad.val
  }
  gGrad <<- grad
  gFunc <<- func
  z
}

# Step 2 of method 1:
# OLD IMPLEMENTATION
optimize_p_given_model.OLD <- function(simils, t, model, pInitial = 0.1, simils.val = NULL, t.val = NULL, useAccuracy = TRUE){
  w <- extractCoefficients(model)
  isReg <- isRegularization(model)
  lambda <- model$lambda
  optimPsEvolution <<- c() # ToDo: use local variable
  #Function to optimize
  func <- function(p) {
    if(p>0) optimPsEvolution <<- c(optimPsEvolution,p)
    E.func(p, simils, t, w, isReg, lambda)
  }
  #Gradient
  grad <- function(p) {
    dE.func(p, simils, t, w)
  }
  
  res <- optim(pInitial, func, grad, method = "CG",control = list(abstol = 1e-10, reltol = 1e-10,fnscale=1e-10))
  print(res$count)
  
  z <- list()
  z$newP <- res$value
  z$E <- res$par
  
  # Test set
  if(!is.null(simils.val)){
    g.ps.E.train <<- sapply(optimPsEvolution, function(p) E.func(p,simils,t,w,isReg,lambda))
    g.ps.E.val <- sapply(optimPsEvolution, function(p) E.func(p,simils.val,t.val,w,isReg,lambda))
    if(!useAccuracy) ps.E.val <- sapply(optimPsEvolution, function(p) E.func.from_model(p,simils.val,t.val,model))
    else ps.E.val <- sapply(optimPsEvolution, function(p) accuracyOrNRMSE(p, simils.val,t.val, model))
    best.E.id <- which.min(ps.E.val)
    z$p.val <-  optimPsEvolution[best.E.id]
    z$E.val <- ps.E.val[best.E.id]
    cat(' newP:',z$newP, ' newP [val]:', z$p.val, '\n')
    z$E <- z$E.val
    z$newP <- z$p.val
  }
  optimPsEvolution <<-NULL
  z
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


# Optimize p using k fold cross validation method
optimize_p_kFoldCV <- function(simils, prototypes, t, control = NULL,...){
  
  # Extract control info
  ps <- seq(0.01,2,0.01)
  kFolds <- 10
  useAccuracy <- FALSE
  if(!is.null(control)){
    if(!is.null(control$ps)) ps <- control$ps
    if(!is.null(control$kFolds)) kFolds <- control$kFolds
    if(!is.null(control$useAccuracy)) useAccuracy <- control$useAccuracy
  }
  
  foldid <- sample(rep(seq(kFolds), length = length(t)))
  
  ps.ObjFunc <- numeric(length(ps))
  ps.ObjFunc.sd <- numeric(length(ps))
  
  #Iterate for ps
  for(i in 1:length(ps)){
    foldRes <- numeric(kFolds)
    #Iterate for folds
    for(k in 1:kFolds){
      # Split train-val
      simils_train <- simils[foldid!=k,prototypes]
      t_train <- t[foldid!=k]
      simils_val <- simils[foldid==k,prototypes]
      t_val <- t[foldid==k]
      
      model <- optimize_p_create_model_given_p(simils_train,t_train,ps[i],...)
      
      if(!useAccuracy) foldRes[k] <- E.func.from_model(ps[i], simils_val,t_val, model)
      else foldRes[k] <- accuracyOrNRMSE(ps[i], simils_val,t_val, model)
    }
    
    ps.ObjFunc[i] <- mean(foldRes)
    ps.ObjFunc.sd[i] <- sd(foldRes)
    
    cat("p= ", ps[i], " - ObjFunc: ", ps.ObjFunc[i], " - sd: ", ps.ObjFunc.sd[i], "\n")
  }
  
  z <- list()
  z$bestP <- ps[which.min(ps.ObjFunc)[1]]
  z$ps <- ps
  z$E <- ps.ObjFunc
  z$E.sd <- ps.ObjFunc.sd
  z
}


# Method that approch the best p
# Inputs: 
#   - dissim <- dissimalirity matrix
#   - pam.res <- result of pam
optimize_p_method3 <- function(dissim, pam.res, type="avg", ...){

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
