# Source of Experiment 2 (Tests the Ensemble of SNNs for several parameters).
source("test/testUtils.R")
source("SNNBagging.R")
library(randomForest)

runEnsSNNTests <- function(datasets, nRuns = 10, classification = FALSE, onlyRandomForest = FALSE) {
  
  if (classification) {
    ensMethods = c('A', 'A2', 'B','B2','C','E') # E is C2
  } else {
    ensMethods = c('A', 'B','B2','C','E') 
  }

  clust.methods <- c('PAM')
  
  fullResults <- data.frame()
  shortResult <- data.frame() 
  for (ds in datasets) {
    cat('Dataset: ', ds$name, '\n')
    set.seed(1998)
    nRunsDs <- ifelse(nrow(ds$dataset) < 4000, nRuns, min(nRuns, 20))
    sampleByRun <- sapply(1:nRunsDs, function(i) sampleTwoThirds(ds$dataset))
    seeds <- sapply(1:nRunsDs, function(i) round(runif(1) * 10000000))

    if (!onlyRandomForest) {
      for (ensMethod in ensMethods) {
        for (clust.method in clust.methods) {
            if ( (TRUE  || nrow(ds$dataset) < 4000)) {
              cat('Executing method', ensMethod, ',', clust.method, ' \n')

              nrmseOrAcc <- numeric(nRunsDs)
              times <- numeric(nRunsDs)

              cat('  # Runs: ')
              for (i in 1:nRunsDs) {
                set.seed(seeds[i])
                cat(i, '')
                myTic()
                
                bagM <- ensMethod
                reg <- FALSE
                if(bagM == 'B2'){
                  reg <- TRUE
                  bagM <- 'B'
                }
                r1 <- snn.bagging(ds$formula, subset = sampleByRun[, i], ds$dataset, bagging.method = bagM, simil.types = ds$simil.types, trace = FALSE, regularization = reg) #, clust.control = list(clust.method = clust.method))
                nrmseOrAcc[i] <- ifelse(!is.null(r1$nrmse), r1$nrmse, r1$testAccuracy)
                times[i] <- myToc(print = FALSE)

                newRow <- data.frame(dataset = ds$name, ensMethod = ensMethod, clust.method = clust.method, run=i, saccOrNRMSE = nrmseOrAcc[i], time = times[i])
                fullResults <- rbind(fullResults, newRow)
                
                r1 <- NULL
                gc()
                save(shortResult, fullResults, file = "experiments/TMPData.Rdata")
              }
              cat('\n')

              newRow <- data.frame(dataset = ds$name, ensMethod = ensMethod, clust.method = clust.method, accMean = mean(nrmseOrAcc), accSd = sd(nrmseOrAcc), timeMean = mean(times), timeSd = sd(times))
              shortResult <- rbind(shortResult, newRow)

              save(shortResult, fullResults, file = "experiments/TMPData.Rdata")
            }
          }
      }
    }
    save(shortResult, fullResults, file = "experiments/TMPData.Rdata")

    #Random Forest
    cat('Executing method: Random Forest \n')
    y <- model.response(model.frame(ds$formula, data = ds$dataset, na.action = NULL, drop.unused.levels = TRUE))
    nrmseOrAcc <- numeric(nRunsDs)
    times <- numeric(nRunsDs)
    cat('  # Runs: ')
    ds4RF <- fixDatasetForRF(ds$dataset)
    for (i in 1:nRunsDs) {
      cat(i, '')
      iniTime <- myTic()
      model.tree <- randomForest(ds$formula, data = ds4RF[sampleByRun[, i],], na.action = na.roughfix)

      if (classification) {
        pred <- predict(model.tree, ds4RF[-sampleByRun[, i],], type = 'class')
        nrmseOrAcc[i] <- accuracy(pred, y[-sampleByRun[, i]])
      } else {
        pred <- predict(model.tree, ds4RF[-sampleByRun[, i],])
        nrmseOrAcc[i] <- nrmse(pred, y[-sampleByRun[, i]])
      }
      times[i] <- myToc(ini = iniTime, print = FALSE)

      newRow <- data.frame(dataset = ds$name, ensMethod = 'RandForest', clust.method = '', run = i, saccOrNRMSE = nrmseOrAcc[i], time = times[i])
      fullResults <- rbind(fullResults, newRow)
    }
    cat('\n')
    newRow <- data.frame(dataset = ds$name, ensMethod = 'RandForest', clust.method = '', accMean = mean(nrmseOrAcc), accSd = sd(nrmseOrAcc), timeMean = mean(times), timeSd = sd(times))
    shortResult <- rbind(shortResult, newRow)

    save(shortResult, fullResults, file = "experiments/TMPData.Rdata")
  }

  fullResults$fullMethod <- paste(paste('Ens:', fullResults$ensMethod), sep = '\n')
  fullResults[fullResults$ensMethod == 'RandForest',]$fullMethod <- 'RandForest'

  shortResult$fullMethod <- paste(paste('Ens:', shortResult$ensMethod), sep = '\n')
  shortResult[shortResult$ensMethod == 'RandForest',]$fullMethod <- 'RandForest'
  z <- list()
  z$fullResults <- fullResults
  z$shortResults <- shortResult

  
  z
}