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

# set base data dir accordingly
base.data.dir <- "~/UPC/DOCENCIA/Assignatures/ATCI-MAI/LABO/CodiR"
#setwd("~/UPC/DOCENCIA/Assignatures/ATCI-MAI/LABO/CodiR")

# main learning function for the SNN

proceed <- function (preproc, 
                     resampling="straight", 
                     kCV=3,
                     dataset.name, 
                     method, 
                     clust.metric, 
                     clust.stand=FALSE, 
                     shuffle=FALSE, 
                     scale=FALSE,
                     verbose=TRUE,
                     classical=FALSE)
  
# preproc <- "raw"            # means NAs are 0's and CATEGs as is (original coding)
# preproc <- "std"            # means NAs are imputed by mice() and CATEGs are factors
# preproc <- "sim"            # means similarity layer (Gower-like)

# resampling <- "CV"          # kCV in the learn part(s)
# kCV <- 5                    # value of k for kCV (default 3)
# resampling <- "straight"    # no kCV in the learn part(s), just fit the models

# the dataset name should match its directory name
# e.g. dataset.name <- 'Pima' matches /Pima/Pima.r

# method <- "LogReg"          # the learning method used

# clust.metric <- "euclidean"/"manhattan"
# clust.stand <- FALSE/TRUE  should data be standardized?

# shuffle <- FALSE/TRUE # should the dataset be shuffled?
# scale <- FALSE/TRUE  # should the dataset be scaled (predictors only)?

# classical <- TRUE means that the SNN clusters the original variables
#              FALSE means that the SNN clusters the similarities

{
  data.dir <- paste (base.data.dir, dataset.name, sep="/")
  setwd (data.dir)
  
  if (verbose) 
  { 
    cat ("\n\n\n######################################################")
    print(sprintf("Processing %s with %s", dataset.name, method)) 
  }
  
  source (paste (dataset.name, ".r", sep=""), local=TRUE)  # it inherits the parameters
  
  best.centers <- floor(0.05*Nlearn) # nnet() uses it
  
  if (preproc == "sim")
  {
    dataset.daisy <- daisy(Predictors, metric="gower", type = simil.types)
    dataset.simils <- data.frame(1-as.matrix(dataset.daisy))
    
    if (classical) { LEARN.data <- Predictors[learn,] } else { LEARN.data <- dataset.simils[learn,learn] }
    
    # PAM clustering
    dataset.pam <- pam (LEARN.data, k=best.centers, metric = clust.metric, stand=clust.stand, keep.diss=FALSE, keep.data=FALSE)
    datas <- data.frame(dataset.simils[,dataset.pam$id.med], Target=Targets)  
  } else 
  { 
    LEARN.data <- Predictors[learn,] 
    datas <- data.frame(Predictors, Target=Targets)
  }
  
  
  LEARN.Targets <- Targets[learn]
  
  LEARN.priors <- as.numeric(table(LEARN.Targets)/Nlearn)
  
  if (verbose) { print(sprintf("Processing %d centers ...", best.centers)) }
  
  if (resampling == "CV")
  {
    CV.folds <- generateCVRuns (labels = Targets[learn], ntimes = 1, nfold = kCV, stratified=TRUE)
    Initial.centers <- 50
    Final.centers <- 5
    step.by <- 5
    
    NVA <-  length(CV.folds[[1]][[1]]) # this is constant
    Centers.seq <- seq(Initial.centers,Final.centers,by=-step.by)
    
    MAXITERS <- length(Centers.seq)
    
    # prepare the structure to store the partial results
    
    cv.results <- matrix (rep(0,4*MAXITERS),nrow=MAXITERS)
    colnames (cv.results) <- c("Ncenters","Silhouette","Silh/Ncenters","CV error")
    cv.results[,"Ncenters"] <- Centers.seq
    cv.results[,"CV error"] <- 0
    cv.results[,"Silhouette"] <- 0
    
    for (j in 1:kCV)
    {
      print(sprintf("** Processing learning fold %d", j))
      
      # get VA data
      va <- unlist(CV.folds[[1]][[j]])
      
      # prepare for clustering on TR-VA data

      if (classical) 
      { 
        LEARN.data <- Predictors[learn,]
        pam.data <- LEARN.data[-va,]
      } else 
      { 
        LEARN.data <- dataset.simils[learn,learn]
        pam.data <- LEARN.data[-va,-va]
      }
      
      i <- 1
      for (Ncenters in Centers.seq)
      {  
        print(sprintf("Reducing columns from %d to %d ...", Nlearn-NVA, Ncenters))
        
        dataset.pam <- pam (pam.data, k=Ncenters, metric = clust.metric, stand=clust.stand, keep.diss=FALSE, keep.data=FALSE)
        
        ### Modelling ###
        print(sprintf("Evaluating reduction ..."))
        
        if (classical)
        {
          datas.LEARN <- data.frame(dataset.simils[learn,dataset.pam$id.med], Target=LEARN.Targets)
        } else 
        { 
          datas.LEARN <- data.frame(LEARN.data[,dataset.pam$id.med], Target=LEARN.Targets) 
        }
        
        if (method == "LogReg") 
        {  # LogReg
          dataset.model <- glm (Target ~ ., data=datas.LEARN[-va,], family=binomial())
          dataset.final <- step(dataset.model, trace=0)
          
          glft <- predict (dataset.final, newdata=datas.LEARN[va,], type="response")
          
          pred.va=NULL
          pred.va[glft<0.5]=0
          pred.va[glft>=0.5]=1
        }
        
        if (method == "LDA") 
        {  # LDA
          dataset.model <- lda (Target ~ ., data=datas.LEARN[-va,], prior=LEARN.priors, CV=FALSE)
          
          pred.va <- predict (dataset.model, newdata=datas.LEARN[va,])$class
        }
        
        if (method == "Multinom") 
        {  # Multinom
          dataset.model <- multinom (Target ~ ., data=datas.LEARN[-va,], maxit = 250)
          
          pred.va <- predict (dataset.model, newdata=datas.LEARN[va,], type="class")
        }
        
        if (method == "MLP") 
        {
          dataset.model <- nnet (Target ~ ., data=datas.LEARN[-va,], size=Ncenters %/% 2, maxit = 250)
          
          pred.va <- predict (dataset.model, newdata=datas.LEARN[va,], type="class")
        }
        
        if (method == "SVM")
        {
          dataset.model <- ksvm (Target ~ ., data=datas.LEARN[-va,], C=SVM.C, cross=0)
          
          pred.va <- predict (dataset.model, newdata=datas.LEARN[va,], type="response")
        }
        
        tab <- table(Targets[va], pred.va)
        error <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
        
        # Gather results
        
        cv.results[i,"Silhouette"] <- cv.results[i,"Silhouette"] + mean(silhouette(dataset.pam))
        cv.results[i,"CV error"] <- cv.results[i,"CV error"] + error
        
        i <- i + 1
      }
    }
    
    # get best result and refit on learn data
    cv.results[,"Silhouette"] <- cv.results[,"Silhouette"] / kCV
    cv.results[,"CV error"] <- cv.results[,"CV error"] / kCV
    cv.results[,"Silh/Ncenters"] <- cv.results[,"Silhouette"] / cv.results[,"Ncenters"]
    
    best.centers <- cv.results[which.min(cv.results[,"CV error"]),"Ncenters"]
    
    print(sprintf("(%s,%s) best.centers %d", dataset.name, method, best.centers)) 
    
    dataset.pam <- pam (LEARN.data, k=best.centers, metric = clust.metric, stand=clust.stand, keep.diss=FALSE, keep.data=FALSE)
    datas <- data.frame(dataset.simils[,dataset.pam$id.med], Target=Targets)
  }
  
  
  if (method == "LogReg") 
  {  # LogReg
    dataset.model <- glm (Target ~ ., data=datas[learn,], family=binomial())
    dataset.final <- step(dataset.model, trace=0)
    
    # predict test data
    glft <- predict (dataset.final, newdata=datas[-learn,], type="response")
    
    pred.te=NULL
    pred.te[glft<0.5]=0
    pred.te[glft>=0.5]=1
  }
  
  if (method == "LDA") 
  {  # LDA
    dataset.model <- lda (Target ~ ., data=datas[learn,], prior=LEARN.priors, CV=FALSE)
    
    pred.te <- predict (dataset.model, newdata=datas[-learn,])$class
  }
  
  if (method == "QDA") 
  {  # QDA
    dataset.model <- qda (Target ~ ., data=datas[learn,], prior=LEARN.priors, CV=FALSE)
    
    pred.te <- predict (dataset.model, newdata=datas[-learn,])$class
  }
  
  if (method == "Multinom") 
  { 
    dataset.model <- multinom (Target ~ ., data=datas[learn,], maxit = 250)
    
    pred.te <- predict (dataset.model, newdata=datas[-learn,], type="class")
  }
  
  if (method == "MLP")
  {
    dataset.model <- nnet (Target ~ ., data=datas[learn,], size=best.centers %/% 2, maxit = 250)
    
    pred.te <- predict (dataset.model, newdata=datas[-learn,], type="class")
  }
  
  if (method == "SVM")
  {
    dataset.model <- ksvm (Target ~ ., data=datas[learn,], C=SVM.C)
    
    pred.te <- predict (dataset.model, newdata=datas[-learn,], type="response")
  }
  
  tab <- table(Targets[-learn],pred.te)
  error <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
  error
}

