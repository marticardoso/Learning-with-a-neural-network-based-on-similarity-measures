
a <- function(p) -1/4+sqrt((1/2)^4 + p)
fp <- function(x,p){
  if(x<=0.5) return(-p/((x-0.5)-a(p)) -a(p))
  else return(-p/((x-0.5)+a(p)) +a(p)+1)
}

#Derivates
da <- function(p) 1/(2*sqrt((1/2)^4 + p))
dfp <- function(x,p){
  if(x<=0.5) return(- 1 - (p/(x-0.5-a(p))+1)*da(p))
  else return(-1 + (p/(x-0.5+a(p))+1)*da(p))
}


plot.fp <- function(values,p){
  r <- lapply(values, function(v) fp(v,p))
  plot(values,r, type="l")
}
plot.fp(seq(0,1,0.01),0.001)
plot.fp(seq(0,1,0.01),0.01)
plot.fp(seq(0,1,0.01),0.1)
plot.fp(seq(0,1,0.01),1)


optimize_p <- function(x, y, simil.types= list(), pInitial= 0.1,maxIter=100, toler=1e-10,clust.method=clust.method,...){
  set.seed(1234)
  x.daisy <- daisy(x, metric="gower", type = simil.types)
  x.simils <- 1-as.matrix(x.daisy)
  clusters.idxs <- snn.findclusters(data.frame(x.simils),clust.method=clust.method,...)
  x.simils <- x.simils[,clusters.idxs]
  
  bestP <- pInitial
  lastOptValue <- Inf
  iter = 1
  while (iter < maxIter){
    model <- optimize_p_step1(x.simils, y, bestP,...)
    optRes <- optimize_p_step2(x.simils, y, model, bestP)
    newP <- optRes$par
    cat('Iter ', iter, ' - p = ', newP, '(opt value:',optRes$value,')\n')
    if(abs(lastOptValue-optRes$value) < toler){
      break;
    }
    bestP <- newP
    lastOptValue <- optRes$value
    iter = iter + 1
  }
  z <- list()
  z$bestP <- bestP
  z$simils <- x.simils
  z$y <- y
  z
  
}

optimize_p_step1 <- function(simils, y, p, ...){
  learn.data <- data.frame(apply(simils, c(1,2), function(x) f_p(x,p)))
  learn.data$Target <- y
  if(is.factor(y) || is.logical(y)){
    model <- snn.createClassificationModel(learn.data, ...)
  }
  else if(is.numeric(y)){
    model <- snn.createRegressionModel(learn.data, ...)
  }
  return(model)
}

optimize_p_step2 <- function(simils, t, model, pInitial = 0.1){
  #Function to optimize
  func <- function(p) {
    if(p<0) return(NA)
    snn.res <- predict(model, data.frame(apply(simils, c(1,2), function(x) f_p(x,p))))
    return(sum((t - snn.res)^2)/length(t))
  }
  
  #Gradient
  grad <- function(p) {
    if(p<0) return(NA)
    snn.res <- predict(model, data.frame(apply(simils, c(1,2), function(x) f_p(x,p))))
    dsnn.res <- predict(model, data.frame(apply(simils, c(1,2), function(x) dfp(x,p))))
    return(2*sum((t - snn.res)*dsnn.res))
  }
  
  res <- optim(0.1, func, grad, method = "BFGS")
}
