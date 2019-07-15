library(cluster)
library(glmnet)
library(nnet)
 
snn <- function (formula, data, subset=NULL, weights, na.action, x = FALSE, y = FALSE, ...)
{
  ret.x <- x
  ret.y <- y
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  ## need stats:: for non-standard evaluation
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  
  if (is.empty.model(mf)) {
    stop("Empty model not supported")
  }
  y <- model.response(mf)
  # x <- model.frame(mt, mf)
  x <- data.frame(mf[,-1])
  
  z <-  snn.fit(x, y, ...)
  
  # predict test data
  
  class(z) <- c("snn")
  z$na.action <- attr(mf, "na.action")
  z$call <- match.call()
  z$mf <- mf
  if (ret.x) z$x <- x
  if (ret.y) z$y <- y
  z$formula <- formula
  
  #Predict test data
  if(!is.null(subset) && length(subset)<nrow(data)){
    cat('Predicting test data\n')
    
    test.y <- model.response(model.frame(formula,data=data[-subset,]))
    
    if(is.logical(y)|| is.factor(y)){
      pred <- predict (z, newdata=data[-subset,], type=c("response","prob"))
      z$testResponse <- pred$response
      z$testProb <- pred$prob
      
      tab <- table(Truth=test.y,Pred=pred$response)
      z$testError <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
      z$testAccuracy <- 100*(1- z$testError)
      z$testContingencyTable <- tab
    }
    else if(is.numeric(y)){
      z$testResponse <- predict (z, newdata=data[-subset,], type="response")
      z$testReal <- test.y
      z$mse <- sum((z$testResponse - test.y)^2)/length(test.y)
      z$nmse <- z$mse/((length(test.y)-1)*var(test.y))
    }
    else {
      stop('Output type not supported')
    }
    
  }
  z
}


snn.fit <- function (x, y, method="glm", classical=FALSE, simil.types=list(),p=1,...)
{
  if (is.null(n <- nrow(x))) stop("'x' must be a dataframe")
  if (n == 0L) stop("0 (non-NA) cases")
  p <- ncol(x)
  if (p == 0L) stop("Null model")
  ny <- NCOL(y)
  
  chkDots(...)
  
  if (classical)  
    learn.data <- x
  else { 
    x.daisy <- daisy(x, metric="gower", type = simil.types)
    x.simils <- 1-as.matrix(x.daisy)
    x.simils <- apply(x.simils, c(1,2), function(x) f_p(x,p))
    learn.data <- data.frame(x.simils)
  }
  
  clusters.idxs <- snn.findclusters(learn.data,...)
  
  prototypes <- x[clusters.idxs,]
  
  dataframe <- data.frame(learn.data[,clusters.idxs], Target=y)  
  
  if(is.factor(y) || is.logical(y)){
    model <- snn.createClassificationModel(dataframe, method=method)
  }
  else if(is.numeric(y)){
    model <- snn.createRegressionModel(dataframe, method=method)
  }
  else stop("Output type not supported")
  
  z <- list() # Output
  z$model <- model
  z$prototypes <- prototypes
  z$classical <- classical
  z$simil.types <- simil.types
  z$p <- p
  if (!classical)
    z$sim <- learn.data
  z$outputType <- class(y)
  if(class(y)=="factor")
    z$outputLevels <- levels(y)
  z$dataframe <- dataframe
  z$method <- method
  z
}

snn.createClassificationModel <- function(dataframe,method="glm"){
  y <- model.response(model.frame(Target~.,dataframe))
  if(is.logical(y) || (is.factor(y) && nlevels(y)==2))
    family.type <- "binomial"
  else if(is.factor(y) && nlevels(y)>2)
    family.type <- "multinomial"
  print(family.type)
  if(method=="glm" && family.type == "binomial"){
    cat("[Classification] Creating glm model...\n")
    model <- glm (Target~., data=dataframe, family="binomial")
    model <- step(model, trace=0)
  }
  else if(method=="multinom" && family.type == "multinomial"){
    cat("[Classification] Creating glm model...\n")
    model <- multinom (Target~., data=dataframe)

  }
  else if(method=="ridge" || method=="lasso"){
    cat("[Classification] Creating multinom model...\n")
    x <- as.matrix(dataframe[,-which(names(dataframe) %in% c("Target"))])
    y <- dataframe$Target
    alpha <- ifelse(method=="lasso", 1, 0)
    cv.result <- cv.glmnet(x,y,nfolds = 10,family = family.type, alpha=alpha)
    model <- glmnet(x,y,family=family.type, lambda=cv.result$lambda.1se[1], alpha=alpha)
  }
  else
    stop(gettextf("Classification method '%s' is not supported. Choose: glm, multinom, ridge, lasso", method))
  print('out')
  model
  
}

snn.createRegressionModel <- function(dataframe,method="lm"){
  
  if(method=="lm"){
    cat("[Regression] Creating lm model...\n")
    model <- lm (Target~., data=dataframe)
    model <- step(model, trace=0)
  }
  else if(method=="ridge"){
    cat("[Regression] Creating ridge regression model...\n")
    stop("ToDO")
  }
  else
    stop(gettextf("Classification method '%s' is not supported. Choose: lm, ridge", method))
  model
  
}

#Function to find the clusters
snn.findclusters <- function(x,         #Dataset
                             clust.method = 'R', # Clustering method
                             clust.metric="euclidean", # (PAM)
                             clust.stand=FALSE,        # (PAM)
                             ...){
  N <- nrow(x)
  M <- snn.numberOfClusters(N, ...)
  
  if(clust.method=="PAM"){
    cat("[Clustering] PAM...\n")
    dataset.pam <- pam (x, k=M, metric = clust.metric, stand=clust.stand, keep.diss=FALSE, keep.data=FALSE)
    return(dataset.pam$id.med)  
  }
  else if(clust.method=="R" || clust.method=="Random"){
    cat("[Clustering] Random\n")
    return(sample(1:N,M))
  }
  else 
    stop(gettextf("Clustering method '%s' is not supported. Supported methods: PAM and Random.", clust.method))
}

# Method to find the number of clusters
# hp <- Estimation of the proportion of clusters.
# nclust.method <- method to decide the number of clusters
snn.numberOfClusters <- function(N, hp=0.1, nclust.method='C'){

  if(nclust.method=="U"|| nclust.method=="Uniform"){
    cat("[Num of clusters method]: Uniform")
    M <- round(runif(1,1,N*hp))
  }
  else if(nclust.method=="B" || nclust.method=="Binomial"){
    cat("[Num of clusters method]: Binomial")
    M <- rbinom(1,N,hp)
  }
  else if(nclust.method=="P" || nclust.method=="Poisson"){
    cat("[Num of clusters method]: Poisson")
    M <- min(rpois(1, N*hp),N)
  }
  else if(nclust.method=="C" || nclust.method=="Constant"){
    cat("[Num of clusters method]: Constant")
    M <- floor(hp*N)
  }
  else 
    stop(gettextf("Number of clusters method '%s' is not supported. Methods supported: Uniform (U), Binomial (B), Poisson (P) and Constant(C).", nclust.method))
  
  M <- max(M,1)
  cat(" - ", M,"\n")
  M
}

print.snn <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
  stop("ToDo implementation")
}

summary.snn <- function (object, correlation = FALSE, symbolic.cor = FALSE, ...)
{
  stop("ToDo implementation")
}



predict.snn = function(object, newdata,type=c("response","prob")){

  mf <- model.frame(object$formula,newdata)
  x <- mf[,-1]
  y <- model.response(mf)
  
  if (object$classical)  
    test.x <- x
  else { 
    compute.daisy <- function(newX, prototypes){
      tmp <- rbind(newX, prototypes)
      daisy.dist <- daisy(tmp, metric="gower", type = object$simil.types)
      as.matrix(daisy.dist)[1,-1]
    }
    
    x.daisy <- t(apply(x, 1, function(row) compute.daisy(row, object$prototypes)))
    x.simils <- 1-as.matrix(x.daisy)
    x.simils <- apply(x.simils, c(1,2), function(x) f_p(x,object$p))
    test.x <- data.frame(x.simils)
    colnames(test.x) = paste('X', row.names(object$prototypes), sep="")
  }
  
  if(object$method=="ridge" || object$method=="lasso") # Glmnet does not support data frame
    test.x  <- as.matrix(test.x)
  
  #Predict by type
  if(object$outputType == "logical"){
    test.prob <- predict (object$model, test.x, type="response")
    response <- test.prob>=0.5
  }
  else if(object$outputType == "factor"){
    if(any(class(object$model) == "multinom")) 
      prob <-  predict (object$model, test.x, type="probs")
    else 
      prob <-  predict (object$model, test.x, type="response")
    
    response <- apply(prob,1,function(p) object$outputLevels[which.max(p)[1]])
    test.prob <- apply(prob,1,function(p) max(p))
  }
  else if(object$outputType=="numeric"){
    response <- predict (object$model, test.x, type="response")
  }
  else
    stop("[Predicting] Output type not supported")
  z <-list()
  
  if("response" %in% type)
    z$response <- response
  
  if("prob" %in% type && (object$outputType == "logical" || object$outputType == "factor"))
    z$prob <- test.prob
  
  if(length(z)==1)
    return(z[[1]])
  
  z
}


f_p <- function(x,p){
  a <- function(p) (-0.5+sqrt(0.5^2+4*p))/2
  if(x<=0.5){
    value <- -p/((x-0.5)-a(p)) -a(p)
  }
  else{
    value <- -p/((x-0.5)+a(p)) +a(p)+1
  }
  value
}

plot.fp <- function(values,p){
  r <- lapply(values, function(v) f_p(v,p))
  plot(values,r, type="l")
}
plot.fp(seq(0,1,0.01),0.001)
plot.fp(seq(0,1,0.01),0.01)
plot.fp(seq(0,1,0.01),0.1)
plot.fp(seq(0,1,0.01),1)
