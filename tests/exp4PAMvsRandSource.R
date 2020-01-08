source("testUtils.R")
source("SNNBagging.R")
source("benchmarkutils.R")
library(randomForest)

#getPercentageOfNa
runExperiment4 <- function(datasets, nRuns = 1, classification = FALSE) {
  clust.methods <- c('PAM', 'Random')
  fullResults <- data.frame()
  shortResult <- data.frame()
  for (ds in datasets) {

    cat('Dataset: ', ds$name, '\n')
    set.seed(1998)
    sampleByRun <- sapply(1:nRuns, function(i) sampleTwoThirds(ds$dataset))
    seeds <- sapply(1:nRuns, function(i) round(runif(1) * 10000000))
    for (clust.method in clust.methods) {
      cat('Executing prop proptotypes ', clust.method, ' \n')

      nrmseOrAcc <- numeric(nRuns)
      times <- numeric(nRuns)

      cat('  # Runs: ')
      for (i in 1:nRuns) {
        set.seed(seeds[i])
        cat(i, '')
        myTic()
        clust.control <- list(clust.method = clust.method)
        r1 <- snn.bagging(ds$formula, subset = sampleByRun[, i], ds$dataset, bagging.method = 'A', simil.types = ds$simil.types, trace = FALSE, regularization = FALSE, clust.control = clust.control) 
        nrmseOrAcc[i] <- ifelse(!is.null(r1$nrmse), r1$nrmse, r1$testAccuracy)
        times[i] <- myToc(print = FALSE)

        newRow <- data.frame(dataset = ds$name, clust.method = clust.method, run = i, saccOrNRMSE = nrmseOrAcc[i], time = times[i])
        fullResults <- rbind(fullResults, newRow)
      }
      cat('\n')

      newRow <- data.frame(dataset = ds$name, clust.method = clust.method, accMean = mean(nrmseOrAcc), accSd = sd(nrmseOrAcc), timeMean = mean(times), timeSd = sd(times))
      shortResult <- rbind(shortResult, newRow)

      save(shortResult, fullResults, file = "tests/exp7TMPData.Rdata")
    }
    save(shortResult, fullResults, file = "tests/exp7TMPData.Rdata")
  }

  fullResults$fullMethod <- paste(fullResults$clust.method, sep = '\n')
  z <- list()
  z$fullResults <- fullResults
  z$shortResults <- shortResult

  z
}