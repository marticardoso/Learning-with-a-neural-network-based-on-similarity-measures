library(MASS)
library(cluster)
library(TunePareto)
library(nnet)
library(kernlab)
library(xtable)
library(mice)
library(missForest)
library(StatMatch)

# define convenience functions
asNumeric <- function(x) as.numeric(unclass(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], asNumeric))


# main learning function for the SNN

SNN <- function (data, 
                 Targets,
                 simil.types,
                 clust.method="PAM",
                 clust.metric="euclidean", 
                 clust.stand=FALSE, 
                 shuffle=FALSE, 
                 scale=FALSE,
                 verbose=TRUE,
                 classical=FALSE)
  
  
  # clust.metric <- "euclidean"/"manhattan"
  # clust.stand <- FALSE/TRUE  should data be standardized?
  
  # shuffle <- FALSE/TRUE # should the dataset be shuffled?
  # scale <- FALSE/TRUE  # should the dataset be scaled (predictors only)?
  
  # classical <- TRUE means that the SNN clusters the original variables
  #              FALSE means that the SNN clusters the similarities
  
{
  # 2/3 lear
  N <- nrow(data)
  
  best.centers <- floor(0.05*N) # nnet() uses it
  
  dataset.daisy <- daisy(data, metric="gower", type = simil.types)
  dataset.simils <- data.frame(1-as.matrix(dataset.daisy))
  
  if (classical) { 
    LEARN.data <- Predictors 
  } else { 
    LEARN.data <- dataset.simils
  }
  
  # Select clusters
  
  if(clust.method=="PAM"){
    dataset.pam <- pam (LEARN.data, k=best.centers, metric = clust.metric, stand=clust.stand, keep.diss=FALSE, keep.data=FALSE)
    clusters.idx <- dataset.pam$id.med  
  }
  else if(clust.method=="Uniform"){
    clusters.idx <-sample(1:N,best.centers)
  }
  else if(clust.method=="Binomial"){
    if(is.null(p)){ p = best.centers/N}
    r <- rbinom(n, 1, p)
    clusters.idx <- which(r>0)
  }
  else if(clust.method=="Poisson"){
    if(is.null(lambda)){ lambda = best.centers/N }
    r <- rpois(N, lambda)
    clusters.idx <- which(r>0)
  }
  else{
    stop("Enter a valid method ('PAM', 'Uniform', 'Binomial', 'Poisson')")
  }
  
  medoids <- data[clusters.idx,]
  
  data.sim <- data.frame(dataset.simils[,clusters.idx])  
  colnames(data.sim) = paste('Medoid', 1:(nrow(medoids)), sep="")
  data.sim$Target = Targets
  
  dataset.model <- glm (Target ~ ., data=data.sim, family=binomial())
  dataset.final <- step(dataset.model, trace=0)
  
  # predict test data
  
  model = list(error=error, medoids=medoids, model=dataset.final)
  return(list(error=error, medoids=medoids, model=dataset.final))
}

SNN.predict = function(model, newData, Targets){
  
  medoids = model$medoids
  dataset.daisy = gower.dist(newData, data.y=medoids)
  dataset.simils <- data.frame(1-as.matrix(dataset.daisy))
  #Transform to data frame
  data.sim <- data.frame(dataset.simils)  
  colnames(data.sim) = paste('Medoid', 1:(nrow(medoids)), sep="")
  data.sim$Target = Targets
  
  #Predict
  glft <- predict (model$model, newdata=data.sim, type="response")
  
  pred.te=NULL
  pred.te[glft<0.5]=0
  pred.te[glft>=0.5]=1
  
  
  
  if(is.null(Targets)==FALSE){
    tab <- table(Targets,pred.te)
    error <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
  }
  
  return(list(prediction=pred.te, error=error))
}

SNN.model = SNN(wine[,-1],wine$Type,list())

pred.result = SNN.predict(SNN.model, wine[,-1],wine$Type)


table(Pred=pred.result$prediction, Truth=wine$Type)
