library(MASS)
library(cluster)
library(TunePareto)
library(nnet)
library(kernlab)
library(xtable)
library(mice)
library(missForest)

# define convenience functions
asNumeric <- function(x) as.numeric(unclass(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], asNumeric))


# main learning function for the SNN

SNN <- function (data, 
                     Targets,
                     newData,
                     simil.types,
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
  Nlearn <- round(2/3*N)
  Ntest <- N - Nlearn
  learn <- sample(N,Nlearn)
  
  best.centers <- floor(0.05*Nlearn) # nnet() uses it
  
  dataset.daisy <- daisy(data, metric="gower", type = simil.types)
  dataset.simils <- data.frame(1-as.matrix(dataset.daisy))
    
  if (classical) { 
    LEARN.data <- Predictors[learn,] 
  } else { 
    LEARN.data <- dataset.simils[learn,learn] 
  }
  
  # Select clusters
  
  if(method=="PAM"){
    dataset.pam <- pam (LEARN.data, k=M, metric = clust.metric, stand=clust.stand, keep.diss=FALSE, keep.data=FALSE)
    clusters.idx <- dataset.pam$id.med  
  }
  else if(method=="Uniform"){
    clusters.idx <-sample(1:n,M)
  }
  else if(method=="Binomial"){
    if(is.null(p)){ p = M/n}
    r <- rbinom(n, 1, p)
    clusters.idx <- which(r>0)
  }
  else if(method=="Poisson"){
    if(is.null(lambda)){ lambda = M/n }
    r <- rpois(n, lambda)
    clusters.idx <- which(r>0)
  }
  else{
    stop("Enter a valid method ('PAM', 'Uniform', 'Binomial', 'Poisson')")
  }
  
  medoids <- data[learn[clusters.idx],]
  
  datas <- data.frame(dataset.simils[,clusters.idx], Target=Targets)  

  dataset.model <- glm (Target ~ ., data=datas[learn,], family=binomial())
  dataset.final <- step(dataset.model, trace=0)
  
  # predict test data
  glft <- predict (dataset.final, newdata=datas[-learn,], type="response")
  
  pred.te=NULL
  pred.te[glft<0.5]=0
  pred.te[glft>=0.5]=1
  
  
  tab <- table(Targets[-learn],pred.te)
  error <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
  error
}

