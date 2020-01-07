source("testUtils.R")
source("SNNBagging.R")
source("benchmarkutils.R")
library(randomForest)

addMissingValues <- function(ds, targetFirstPosition = FALSE, missingRatio = 0.1) {
  totalData <- nrow(ds) * (ncol(ds)-1)
  nMissings <- round(missingRatio * totalData)
  m <- sample(1:totalData, nMissings)-1

  mCol <- mod(m, ncol(ds) - 1) + 1
  if (targetFirstPosition) mCol <- mCol + 1
  mRow <- floor(m / (ncol(ds)-1) ) + 1

  for (colId in 1:ncol(ds)) {    
    ds[mRow[mCol == colId], colId] <- NA
  }
  ds
}

#getPercentageOfNa
runExperiment5 <- function(datasets, nRuns = 1, classification = FALSE, targetFirstPosition = FALSE) {

  if (classification) {
    ensMethods = c('A')
  } else {
    ensMethods = c('A')
  }
  clust.methods <- c('PAM')

  fullResults <- data.frame()
  shortResult <- data.frame()
  for (ds in datasets) {

    cat('Dataset: ', ds$name, '\n')
    set.seed(1998)
    sampleByRun <- sapply(1:nRuns, function(i) sampleTwoThirds(ds$dataset))
    seeds <- sapply(1:nRuns, function(i) round(runif(1) * 10000000))
    for (propMissings in seq(0, 0.999999, 0.1)) {
      datasetWithMissings <- addMissingValues(ds$dataset, missingRatio = propMissings, targetFirstPosition = targetFirstPosition)
      gDatasetMissing <<- datasetWithMissings
      for (ensMethod in ensMethods) {
        for (clust.method in clust.methods) {
          cat('Executing missings', propMissings, ' \n')

          nrmseOrAcc <- numeric(nRuns)
          times <- numeric(nRuns)

          cat('  # Runs: ')
          for (i in 1:nRuns) {
            set.seed(seeds[i])
            cat(i, '')
            myTic()
            r1 <- snn.bagging(ds$formula, subset = sampleByRun[, i], datasetWithMissings, bagging.method = ensMethod, simil.types = ds$simil.types, trace = FALSE, regularization = FALSE) #, clust.control = list(clust.method = clust.method))
            nrmseOrAcc[i] <- ifelse(!is.null(r1$nrmse), r1$nrmse, r1$testAccuracy)
            times[i] <- myToc(print = FALSE)

            newRow <- data.frame(dataset = ds$name, ensMethod = ensMethod, clust.method = clust.method, propMissings = propMissings, run = i, saccOrNRMSE = nrmseOrAcc[i], time = times[i])
            fullResults <- rbind(fullResults, newRow)
          }
          cat('\n')

          newRow <- data.frame(dataset = ds$name, ensMethod = ensMethod, clust.method = clust.method, propMissings = propMissings, accMean = mean(nrmseOrAcc), accSd = sd(nrmseOrAcc), timeMean = mean(times), timeSd = sd(times))
          shortResult <- rbind(shortResult, newRow)

          save(shortResult, fullResults, file = "tests/exp5TMPData.Rdata")
        }
      }
      save(shortResult, fullResults, file = "tests/exp5TMPData.Rdata")
    }
  }

  fullResults$fullMethod <- paste(fullResults$clust.method, paste('Ens:', fullResults$ensMethod), fullResults$propMissings, sep = '\n')
  z <- list()
  z$fullResults <- fullResults
  z$shortResults <- shortResult


  z
}