library(cluster)
library(glmnet)
library(nnet)
 
source("fp_utils.R")

debug=TRUE

snn <- function (formula, data, subset=NULL, x = TRUE, y = TRUE, ..., trace=TRUE )
{
  ret.x <- x
  ret.y <- y
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  ## need stats:: for non-standard evaluation
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  
  if (is.empty.model(mf)) {
    stop("Empty model not supported")
  }
  y <- model.response(mf)
  x <- data.frame(mf[,-1])
  
  z <-  snn.fit(x, y, ...)
  
  #chkDots(...)
  
  class(z) <- c("snn")
  z$call <- match.call()
  z$mf <- mf
  if (ret.x) z$x <- x
  if (ret.y) z$y <- y
  z$formula <- formula
  
  #Predict test data
  if(!is.null(subset) && length(subset) < nrow(data)){
    if(trace) cat('Predicting test data\n')
    
    test.y <- model.response(model.frame(formula,data=data[-subset,]))
    
    if(is.logical(y)|| is.factor(y)){
      pred <- predict (z, newdata=data[-subset,], type=c("response","prob"))
      z$testResponse <- pred$response
      z$testProb <- pred$prob
      pred <<- pred
      tab <- table(Truth=test.y,Pred=pred$response)
      z$testError <- 1-sum(diag(tab))/sum(tab)
      z$testAccuracy <- 100*(1- z$testError)
      z$testContingencyTable <- tab
    }
    else if(is.numeric(y)){
      z$testResponse <- predict (z, newdata=data[-subset,], type="response")
      z$testReal <- test.y
      z$mse <- mean((z$testResponse - test.y)^2)
      z$nrmse <- sum((z$testResponse - test.y)^2) / ((length(test.y)-1)*var(test.y))
    }
    else stop('Output type not supported')
  }
  z
}


snn.fit <- function (x, y, method="glm", classical=FALSE, simil.types=list(),p=0.1,hp=0.1,..., trace=TRUE)
{
  if (is.null(n <- nrow(x))) stop("'x' must be a dataframe")
  p <- ncol(x)
  if (p == 0L) stop("Null model")
  ny <- NCOL(y)
  
  
  if (classical) clust.data <- x
  else { 
    x.daisy <- daisy(x, metric="gower", type = simil.types)
    x.simils <- 1 - as.matrix(x.daisy)
    clust.data <- data.frame(x.simils)
  }
  clusters.idxs <- snn.findclusters(clust.data,hp=hp,...)
  
  prototypes <- x[clusters.idxs,]
  
  if(classical) learn.data <- x[,clusters.idxs]
  else learn.data <- data.frame(apply(x.simils[,clusters.idxs], c(1,2), function(x) fp(x,p)))
  
  learn.data$Target <- y
  
  if(is.factor(y) || is.logical(y))
    model <- snn.createClassificationModel(learn.data, method=method,...)
  else if(is.numeric(y))
    model <- snn.createRegressionModel(learn.data, method=method,...)
  else stop("Output type not supported")
  
  z <- list() # Output
  z$model <- model
  z$prototypes <- prototypes
  z$classical <- classical
  z$simil.types <- simil.types
  z$p <- p
  if (!classical)
    z$sim <- clust.data
  z$outputType <- class(y)
  if(class(y)=="factor")
    z$outputLevels <- levels(y)
  
  z$method <- method
  #Debug info
  if(debug){
    z$x.simils <- x.simils
    z$clust.data <- clust.data
    z$learn.data <- learn.data
  }
  z
}

snn.createClassificationModel <- function(dataframe,method="glm",..., trace=TRUE){
  y <- model.response(model.frame(Target~.,dataframe))
  if(is.logical(y) || (is.factor(y) && nlevels(y)==2))
    family.type <- "binomial"
  else if(is.factor(y) && nlevels(y)>2)
    family.type <- "multinomial"

  if(method=="glm" && family.type == "binomial"){
    if(trace) cat("[Classification] Creating glm model...\n")
    model <- glm (Target~., data=dataframe, family="binomial",...)
    model <- step(model, trace=0)
  }
  else if(method=="multinom" && family.type == "multinomial"){
    if(trace) cat("[Classification] Creating multinom model...\n")
    model <- multinom (Target~., data=dataframe,trace=FALSE,...)
  }
  else if(method=="ridge" || method=="lasso"){
    if(trace) cat("[Classification] Creating ridge/lasso model...\n")
    x <- as.matrix(dataframe[,-which(names(dataframe) %in% c("Target"))])
    y <- dataframe$Target
    alpha <- ifelse(method=="lasso", 1, 0)
    cv.result <- cv.glmnet(x, y, nfolds = 10, family = family.type, alpha=alpha)
    model <- glmnet(x,y,family=family.type, lambda=cv.result$lambda.1se[1], alpha=alpha)
  }
  else
    stop(gettextf("Classification method '%s' is not supported. Choose: glm, multinom, ridge, lasso", method))

  model
}

snn.createRegressionModel <- function(dataframe,method="lm",..., trace=TRUE){
  
  if(method=="lm"){
    if(trace) cat("[Regression] Creating lm model...\n")
    model <- lm (Target~., data=dataframe)
    model <- step(model, trace=0)
  }
  else if(method=="ridge" || method=="lasso"){
    if(trace) cat("[Regression] Creating ridge/lasso regression model...\n")
    x <- as.matrix(dataframe[,-which(names(dataframe) %in% c("Target"))])
    y <- dataframe$Target
    alpha <- ifelse(method=="lasso", 1, 0)
    cv.result <- cv.glmnet(x, y, nfolds = 10, family = "gaussian", alpha=alpha)
    model <- glmnet(x,y,family="gaussian", lambda=cv.result$lambda.1se[1], alpha=alpha)
  }
  else
    stop(gettextf("Regression method '%s' is not supported. Choose: lm, ridge or lasso", method))
  model
}

#Function to find the clusters
snn.findclusters <- function(clust.data,         #Dataset
                             clust.method = 'PAM', # Clustering method
                             clust.metric="euclidean", # (PAM)
                             clust.stand=FALSE,        # (PAM)
                             ..., trace=TRUE){
  N <- nrow(clust.data)
  M <- snn.numberOfClusters(N, ...)
  
  if(clust.method=="PAM"){
    if(trace) cat("[Clustering] PAM...\n")
    dataset.pam <- pam (clust.data, k=M, metric = clust.metric, stand=clust.stand, keep.diss=FALSE, keep.data=FALSE)
    return(dataset.pam$id.med)  
  }
  else if(clust.method=="R" || clust.method=="Random"){
    if(trace) cat("[Clustering] Random\n")
    return(sample(1:N,M))
  }
  else 
    stop(gettextf("Clustering method '%s' is not supported. Supported methods: PAM and Random.", clust.method))
}

# Method to find the number of clusters
# hp <- Estimation of the proportion of clusters.
# nclust.method <- method to decide the number of clusters
snn.numberOfClusters <- function(N, hp=0.1, nclust.method='C', trace=TRUE){

  if(nclust.method=="U"|| nclust.method=="Uniform"){
    if(trace) cat("[Num of clusters method]: Uniform")
    M <- round(runif(1,1,N*hp))
  }
  else if(nclust.method=="B" || nclust.method=="Binomial"){
    if(trace) cat("[Num of clusters method]: Binomial")
    M <- rbinom(1,N,hp)
  }
  else if(nclust.method=="P" || nclust.method=="Poisson"){
    if(trace) cat("[Num of clusters method]: Poisson")
    M <- min(rpois(1, N*hp),N)
  }
  else if(nclust.method=="C" || nclust.method=="Constant"){
    if(trace) cat("[Num of clusters method]: Constant")
    M <- floor(hp*N)
  }
  else 
    stop(gettextf("Number of clusters method '%s' is not supported. Methods supported: Uniform (U), Binomial (B), Poisson (P) and Constant(C).", nclust.method))
  
  M <- max(M,1)
  if(trace) cat(" - ", M,"\n")
  M
}

print.snn <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
  stop("ToDo implementation")
}

summary.snn <- function (object)
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
    #compute.daisy <- function(newX, prototypes){
    #  tmp <- rbind(newX, prototypes)
    #  daisy.dist <- daisy(tmp, metric="gower", type = object$simil.types)
    #  r <<-as.matrix(daisy.dist)[1,-1]
      
    #  r
    #}
    #x.daisy <- sapply(1:nrow(x), function(row) compute.daisy(x[row,],object$prototypes))
    #x.daisy <- data.frame(x.daisy)
    
    x.daisy <- daisy(rbind(x,object$prototypes), metric="gower", type = object$simil.types)
    x.simils <- 1-as.matrix(x.daisy)
    x.simils <- x.simils[1:nrow(x), (nrow(x)+1):nrow(x.simils)]
    test.x <- data.frame(apply(x.simils, c(1,2), function(x) fp(x,object$p)))
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
    if(length(object$outputLevels)==2){
      response <- rep(object$outputLevels[1], length(y))
      response[prob>=0.5] <- object$outputLevels[2]
      test.prob <- prob
    }
    else{
      response <- apply(prob,1,function(p) object$outputLevels[which.max(p)[1]])
      test.prob <- apply(prob,1,function(p) max(p))
    }
    
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








