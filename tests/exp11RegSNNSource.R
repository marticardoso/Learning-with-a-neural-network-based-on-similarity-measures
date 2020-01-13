source("testUtils.R")
source("SNNBagging.R")
source("benchmarkutils.R")

runExperiment11 <- function(datasets, nRuns = 1, classification = FALSE) {

  fullResults <- data.frame()
  shortResult <- data.frame()
  for (ds in datasets) {

    cat('Dataset: ', ds$name, '\n')
    set.seed(11911)
    sampleByRun <- sapply(1:nRuns, function(i) sampleTwoThirds(ds$dataset))
    seeds <- sapply(1:nRuns, function(i) round(runif(1) * 10000000))
    for (reg in c(FALSE,TRUE)) {
      cat('Executing ', reg, ' \n')

      nrmseOrAcc <- numeric(nRuns)
      times <- numeric(nRuns)

      cat('  # Runs: ')
      for (i in 1:nRuns) {
        set.seed(seeds[i])
        cat(i, '')
        myTic()
        r1 <- snn.bagging(ds$formula, subset = sampleByRun[, i], ds$dataset, bagging.method = 'A',
                          simil.types = ds$simil.types, trace = FALSE, regularization = FALSE, snn.reg = reg)
        nrmseOrAcc[i] <- ifelse(!is.null(r1$nrmse), r1$nrmse, r1$testAccuracy)
        times[i] <- myToc(print = FALSE)

        newRow <- data.frame(dataset = ds$name, reg = reg, run = i, saccOrNRMSE = nrmseOrAcc[i], time = times[i])
        fullResults <- rbind(fullResults, newRow)
      }
      cat('\n')

      newRow <- data.frame(dataset = ds$name, reg = reg, accMean = mean(nrmseOrAcc), accSd = sd(nrmseOrAcc), timeMean = mean(times), timeSd = sd(times))
      shortResult <- rbind(shortResult, newRow)

      save(shortResult, fullResults, file = "tests/Exp11/Exp11TMPData.Rdata")
    }
    save(shortResult, fullResults, file = "tests/Exp11/Exp11TMPData.Rdata")
  }

  fullResults$fullMethod <- paste(fullResults$reg, sep = '\n')
  fullResults$isRegularization <- ifelse(fullResults$reg, 'Yes', 'No')
  shortResult$isRegularization <- ifelse(shortResult$reg, 'Yes', 'No')
  z <- list()
  z$fullResults <- fullResults
  z$shortResults <- shortResult

  z
}