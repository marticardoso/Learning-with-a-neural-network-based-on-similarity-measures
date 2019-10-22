library(cluster)
library(glmnet)
library(StatMatch)

snn <- function (formula, data, subset=NULL, weights, na.action,
                method = "glm", clust.method = "PAM", x = FALSE, y = FALSE,
                contrasts = NULL, ...)
{
  ret.x <- x
  ret.y <- y
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  ## need stats:: for non-standard evaluation
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms") # allow model.frame to update it

  
  if (is.empty.model(mt)) {
    stop("Empty model not supported")
  }
  y <- model.response(mf)
  x <- model.matrix(mt, mf, contrasts)
  print(nrow(x))
  z <-  snn.fit(x, y, method=method, clust.method=clust.method, ...)
  
  # predict test data
  
  class(z) <- c("snn")
  z$na.action <- attr(mf, "na.action")
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
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
      
      tab <- table(test.y,pred$response)
      z$testError <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
      z$testAccuracy <- 100*(1- z$testError)
      z$testTab <- tab
    }
    else if(is.numeric(y)){
      z$testResponse <- predict (z, newdata=data[-subset,], type="response")
      z$testReal <- test.y
      z$mse <- sum((z$testResponse - test.y)^2)
      z$nmse <- z$mse/((length(test.y)-1)*var(test.y))
    }
    else {
      stop('Output type not supported')
    }
    
  }
  z
}


snn.fit <- function (x, y, clust.method = "PAM", method="glm", classical=FALSE, simil.types=list(),...)
{
  if (is.null(n <- nrow(x))) stop("'x' must be a matrix")
  if(n == 0L) stop("0 (non-NA) cases")
  p <- ncol(x)
  if (p == 0L) stop("Null model")
  ny <- NCOL(y)
  ## treat one-col matrix as vector
  if(is.matrix(y) && ny == 1)
    y <- drop(y)
  if (NROW(y) != n)
    stop("incompatible dimensions")
  
  chkDots(...)
  
  if (classical)  
    learn.data <- x
  else { 
    x.daisy <- daisy(x, metric="gower", type = simil.types)
    x.simils <- data.frame(1-as.matrix(x.daisy))
    learn.data <- x.simils
  }
  
  clusters.idxs <- snn.findclusters(learn.data,method=clust.method,...)
  
  medoids <- x[clusters.idxs,]
  
  dataframe <- data.frame(learn.data[,clusters.idxs], Target=y)  
  
  if(is.factor(y) && nlevels(y)>2){
    model <- list()
    for(i in 1:nlevels(y)){ 
      dataframe$Target = as.factor(y==levels(y)[i])
      model[[i]] <- snn.createClassificationModel(dataframe, method=method)
    }
  }
  else if(is.logical(y) || (is.factor(y) && nlevels(y)==2)){
    print('IN')
    model <- snn.createClassificationModel(dataframe, method=method)
  }
  else if(is.numeric(y)){
    model <- snn.createRegressionModel(dataframe, method=method)
  }
  else {
    stop("Output type not supported")
  }
  
  z <- list()
  z$model <- model
  z$medoids <- medoids
  z$nMedoids <- length(clusters.idxs)
  z$classical <- classical
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
  if(method=="glm"){
    cat("[Classification] Creating glm model...\n")
    model <- glm (Target~., data=dataframe, family=binomial())
    model <- step(model, trace=0)
  }
  else if(method=="glmnet"){
    cat("[Classification] Creating glmnet model...\n")
    x <- as.matrix(dataframe[,-which(names(dataframe) %in% c("Target"))])
    y <- dataframe$Target
    cv.result <- cv.glmnet(x,y,nfolds = 10,family = "binomial")
    model <- glmnet(x,y,family="binomial", lambda=cv.result$lambda.1se[1])
  }
  else
    stop(gettextf("Classification method '%s' is not supported. Choose: glm, glmnet", method))
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

snn.findclusters <- function(x,       #Dataset
                             M= NULL, #Number of clusters
                             method,  #Clustering method
                             clust.metric="euclidean", # (PAM)
                             clust.stand=FALSE,        # (PAM)
                             p = NULL,                 # (Binomial)
                             lambda = NULL,            # (Poison)
                             ...){
  n <- nrow(x)
  if(is.null(M)){
    M = max(0.1*n,1)
  }
  
  if(method=="PAM"){
    cat("[Clustering] Running PAM...\n")
    dataset.pam <- pam (x, k=M, metric = clust.metric, stand=clust.stand, keep.diss=FALSE, keep.data=FALSE)
    return(dataset.pam$id.med)  
  }
  else if(method=="Uniform")
    return(sample(1:n,M))
  else if(method=="Binomial"){
    if(is.null(p)) 
      p = M/n
    r <- rbinom(n, 1, p)
    return(which(r>0))
  }
  else if(method=="Poisson"){
    if(is.null(lambda))
      lambda = M/n
    r <- rpois(n, lambda)
    return(which(r>0))
  }
  else 
    stop(gettextf("Clustering method '%s' is not supported.", method))
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

  x <- model.matrix(object$formula,newdata)
  dataset.gower = gower.dist(x, data.y=object$medoids)
  dataset.sim <- data.frame(1-as.matrix(dataset.gower))
  colnames(dataset.sim) = paste('X', row.names(object$medoids), sep="")
  
  if(object$method=="glmnet") # Glmnet does not support data frame
    dataset.sim  <- as.matrix(dataset.sim)
  
  
  #Predict by type
  if(object$outputType == "logical"){
    test.prob <- predict (object$model, dataset.sim, type="response")
    response <-NULL
    response[test.prob<0.5]<-FALSE
    response[test.prob>=0.5]<-TRUE
  }
  else if(object$outputType == "factor"){
    nLevels <- length(object$outputLevels)
    prob = matrix(0,3,nrow(dataset.sim))
    for(i in 1:nLevels){ 
      prob[i,] <-  predict (object$model[[i]], dataset.sim, type="response")
    }
    response <- apply(prob,2,function(p) object$outputLevels[which.max(p)[1]])
    test.prob <- apply(prob,2,function(p) max(p))
  }
  else if(object$outputType=="numeric"){
    response <- predict (object$model, dataset.sim, type="response")
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

#############################
## Tests ####################
#############################

library(rattle.data)
library( faraway)
r1 <- snn(Type~.,wine,subset=sample(nrow(wine),100),method="glm")
r1$testTab
r2 <- snn(Type~.,wine,subset=sample(nrow(wine),100),method="glmnet")
r2$testTab

response = predict(r1,wine,c("response","prob"))
response$response
table(response$response, wine$Type)

(pred <- predict (r1, wine[1:10,], type="response"))

#Test with logical / factor of two modalities
wine2 <- wine
wine2$Type <- NULL
wine2$Type1 <- wine$Type==1

r3 <- snn(Type1~.,wine2,subset=sample(nrow(wine),100), method="glm")
r3$testTab
pred <- predict(r3,wine2)

r4 <- snn(Type1~.,wine2,subset=sample(nrow(wine),100), method="glmnet")
r4$testTab


# SNN.FIT
r4 <- snn.fit(wine[,-1],wine$Type)


# Snn.createmodel
df <-r1$dataframe
t <-snn.createClassificationModel(df, method="glmnet")



# Numeric output
s <- sample(nrow(prostate),60)
r5 <- snn(lpsa~.,prostate,subset=s, method="lm")
r5$testReal
r5$testResponse-r5$testReal
r5$mse


