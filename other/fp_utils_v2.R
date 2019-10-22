# Adding multinomial 
library(e1071)

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




E.multinom <- function(p, simils, t, model){
  
  w <- coef(model)  #Extract w
  colnames(w) <- gsub('X','',colnames(w)) # Remove X from names
  
  prototypes <- colnames(w)[-which(colnames(w) %in% c("(Intercept)"))]
  
  snn.res <- apply(simils[,prototypes], c(1,2), function(x) fp(x,p))
  snn.res <- cbind(1, snn.res)
  snn.res <- snn.res %*% t(w)
  snn.res <- cbind(0,snn.res)
  
  nnetRes <- t(apply(snn.res, 1, function(r) exp(r)/sum(exp(r))))
  
  real <- class.ind(t)
  
  res <- sum((nnetRes -real)^2)/(length(t)*3)
}

dE.multinom <- function(p, simils, t, model){
  
  w <- coef(model)  #Extract w
  colnames(w) <- gsub('X','',colnames(w)) # Remove X from names
  prototypes <- colnames(w)[-which(colnames(w) %in% c("(Intercept)"))]
  
  X <- apply(simils[,prototypes], c(1,2), function(x) fp(x,p))
  X <- cbind(1, X) # Add intercept column
  nnetRes <- X %*% t(w) # w has intercept
  nnetRes <- cbind(0,nnetRes) # Added base class
  
  snnRes <- t(apply(nnetRes, 1, function(r) exp(r)/sum(exp(r))))
  
  
  w0 <- w[,"(Intercept)"]                         #Intercept
  w <- w[,-which(colnames(w) %in% c("(Intercept)"))] # Remove intercept
  
  X <- apply(simils[,prototypes], c(1,2), function(x) dfp(x,p))
  dnnet <- X %*% t(w) # No intercept
  dnnet <- cbind(0,dnnet) # First class has 0 derivative
  
  dnnetRes <- dnnet
  for(i in 1:nrow(dnnetRes)){
    rowExpSum <- sum(exp(snnRes[i,]))
    dRowExpSum <- sum(exp(snnRes[i,])*dnnet[i,])
    for(j in 1:ncol(dnnetRes)){
      dnnetRes[i,j] <- exp(snnRes[i,j])*dnnet[i,j]*rowExpSum - exp(snnRes[i,j])*dRowExpSum
      dnnetRes[i,j] <- dnnetRes[i,j]/rowExpSum^2
    }
  }
  
  real <- class.ind(t)
  
  res <- -sum((real-snnRes) *dnnetRes)/(length(t)*3)
}


optimize_p <- function(x.simils,y, pInitial= 0.5,maxIter=100, toler=1e-5,...){
  cat('Optimization of p - pInitial = ', pInitial, '\n')
  bestP <- pInitial
  iter = 1
  while (iter < maxIter){
    model <<- optimize_p_create_model_given_p(x.simils, y, bestP,...)
    optRes <- optimize_p_given_model(x.simils, y, model, bestP)
    newP <- optRes$par
    cat('Iter ', iter, ' - p = ', newP, '(opt value:',optRes$value,')\n')
    if(abs(bestP-newP) < toler){
      break;
    }
    bestP <- newP
    iter = iter + 1
  }
  z <- list()
  z$bestP <- bestP
  z$simils <- x.simils
  z$model <- model
  z$y <- y
  z
  
}

# Step 1 of method 1:
# Create a model given a p value (optimize w given a p value)
optimize_p_create_model_given_p <- function(simils, y, p, ...){
  learn.data <- data.frame(apply(simils, c(1,2), function(x) fp(x,p)))
  learn.data$Target <- y
  if(is.factor(y) || is.logical(y)){
    model <- snn.createClassificationModel(learn.data, trace=FALSE,...)
  }
  else if(is.numeric(y)){
    model <- snn.createRegressionModel(learn.data, trace=FALSE, ...)
  }
  return(model)
}

# Step 2 of method 1:
# Function that given a model/w vector, optimize the p value
optimize_p_given_model <- function(simils, t, model, pInitial = 0.1){
  
  if(is.factor(t) || is.logical(t)){
    #Function to optimize
    func <- function(p) E.multinom(p, simils, t, model)
    #Gradient
    grad <- function(p) dE.multinom(p, simils, t, model)
  }
  else if(is.numeric(t)){
    #Function to optimize
    func <- function(p) E.func(p, simils, t, model)
    #Gradient
    grad <- function(p) dE.func(p, simils, t, model)
  }
  
  
  res <- optim(pInitial, func, grad, method = "BFGS")
}

# Try a range of p, and return the one with highest value
optimize_p_test_range_of_values <- function(simils, t, ps = NULL){
  if(is.null(ps)) ps = seq(0.01,1,0.01)
  
  E.res <- sapply(ps, function(p) {
    model <- optimize_p_create_model_given_p(simils,t,p)
    E.val <- E.func(p, simils,t, model)
    return(E.val)
  })
  z <- list()
  z$bestP <- ps[which.min(E.res)[1]]
  z$ps <- ps
  z$ps.E <- E.res
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


# E function (error) = t - snn(x)
E.class <- function(p, simils, t, model) {
  if(p<=0) return(NA)
  
  r = predict(model, simils)
  r==t
  res <-(sum((r==t))/length(t))
  #cat('Pval: ', p, ' - optVal= ', res, '\n')
  return(res)
}


# derivateE function (error) = t - snn(x)
E.class <- function(p, simils, t, model) {
  if(p<=0) return(NA)
  
  r = predict(model, simils)
  r==t
  res <-(sum((r==t))/length(t))
  #cat('Pval: ', p, ' - optVal= ', res, '\n')
  return(res)
}