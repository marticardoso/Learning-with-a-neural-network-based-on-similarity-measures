
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

E.func <- function(p, simils, t, model) {
  if(p<=0) return(NA)
  snn.res <- predict(model, data.frame(apply(simils, c(1,2), function(x) fp(x,p))))
  res <-(sum((t - snn.res)^2)/length(t))
  #cat('Pval: ', p, ' - optVal= ', res, '\n')
  return(res)
}

dE.func <- function(p, simils, t, model) {
  if(p<=0) return(NA)
  
  w <- coef(model)  #Extract w
  w0 <- w["(Intercept)"]                         #Intercept
  w <- w[-which(names(w) %in% c("(Intercept)"))] # Remove intercept
  names(w) <- gsub('X','',names(w))              # Remove X from names
  
  #snn.res <- predict(model, data.frame(apply(simils, c(1,2), function(x) fp(x,p))))
  #dsnn.res <- predict(model, data.frame(apply(simils, c(1,2), function(x) dfp(x,p))))
  
  snn.res <- apply(simils[,names(w)], c(1,2), function(x) fp(x,p))
  snn.res <- snn.res %*% w
  snn.res <- snn.res + w0
 
  dsnn.res <- apply(simils[,names(w)], c(1,2), function(x) dfp(x,p))
  dsnn.res <- dsnn.res %*% w # No intercept
  
  res <- -(2/length(t) * sum((t - snn.res)*dsnn.res))
  
  return(res)
}

optimize_p <- function(x, y, simil.types= list(), pInitial= 0.5,maxIter=100, toler=1e-5,...){
  set.seed(1234)
  x.daisy <- daisy(x, metric="gower", type = simil.types)
  x.simils <- 1-as.matrix(x.daisy)
  clusters.idxs <- snn.findclusters(data.frame(x.simils),...)
  x.simils <- x.simils[,clusters.idxs]
  
  cat('Optimization of p - pInitial = ', pInitial, '\n')
  bestP <- pInitial
  iter = 1
  while (iter < maxIter){
    model <- optimize_p_step1(x.simils, y, bestP,...)
    optRes <- optimize_p_step2(x.simils, y, model, bestP)
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

optimize_p_step1 <- function(simils, y, p, ...){
  learn.data <- data.frame(apply(simils, c(1,2), function(x) fp(x,p)))
  learn.data$Target <- y
  if(is.factor(y) || is.logical(y)){
    model <- snn.createClassificationModel(learn.data, trace=FALSE,...)
  }
  else if(is.numeric(y)){
    model <- snn.createRegressionModel(learn.data, ...)
  }
  return(model)
}

optimize_p_step2 <- function(simils, t, model, pInitial = 0.1){
  #Function to optimize
  func <- function(p) E.func(p, simils, t, model)
  
  #Gradient
  grad <- function(p) dE.func(p, simils, t, model)
  
  res <- optim(pInitial, func, grad, method = "BFGS")
}

optimize_p_cv <- function(simils, t, ps = NULL){
  if(is.null(ps)) ps = seq(0.01,1,0.01)
  
  E.res <- sapply(ps, function(p) {
    model <- optimize_p_step1(simils,t,p)
    E.val <- E.func(p, simils,t, model)
    return(E.val)
  })
  
  ps[which.min(E.res)[1]]
}

