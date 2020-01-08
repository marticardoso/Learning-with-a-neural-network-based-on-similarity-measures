source("testUtils.R")
source("SNNBagging.R")
source("benchmarkutils.R")

runExperiment10 <- function(datasets, nRuns = 1, classification = FALSE) {

  fullResults <- data.frame()
  shortResult <- data.frame()
  for (ds in datasets) {

    cat('Dataset: ', ds$name, '\n')
    set.seed(1998)
    sampleByRun <- sapply(1:nRuns, function(i) sampleTwoThirds(ds$dataset))
    seeds <- sapply(1:nRuns, function(i) round(runif(1) * 10000000))
    for (nrow.prop in seq(0, 1, 0.1)) {
      cat('Executing ', nrow.prop, ' \n')

      nrmseOrAcc <- numeric(nRuns)
      times <- numeric(nRuns)

      cat('  # Runs: ')
      for (i in 1:nRuns) {
        set.seed(seeds[i])
        cat(i, '')
        myTic()
        r1 <- snn.bagging(ds$formula, subset = sampleByRun[, i], ds$dataset, bagging.method = 'A', snnbag.rowMethod = 'C', snnbag.expectedRows = nrow.prop,
                          simil.types = ds$simil.types, trace = FALSE, regularization = FALSE)
        nrmseOrAcc[i] <- ifelse(!is.null(r1$nrmse), r1$nrmse, r1$testAccuracy)
        times[i] <- myToc(print = FALSE)

        newRow <- data.frame(dataset = ds$name, nrow.prop = nrow.prop, run = i, saccOrNRMSE = nrmseOrAcc[i], time = times[i])
        fullResults <- rbind(fullResults, newRow)
      }
      cat('\n')

      newRow <- data.frame(dataset = ds$name, nrow.prop = nrow.prop, accMean = mean(nrmseOrAcc), accSd = sd(nrmseOrAcc), timeMean = mean(times), timeSd = sd(times))
      shortResult <- rbind(shortResult, newRow)

      save(shortResult, fullResults, file = "tests/Exp10/Exp10TMPData.Rdata")
    }
    save(shortResult, fullResults, file = "tests/Exp10/Exp10TMPData.Rdata")
  }

  fullResults$fullMethod <- paste(fullResults$nrow.prop, sep = '\n')
  z <- list()
  z$fullResults <- fullResults
  z$shortResults <- shortResult

  z
}