# Source of Experiment 8 (Tests the number of prototypes distribution used for each SNN of an Ensemble).
source("test/testUtils.R")
source("SNNBagging.R")
source("test/benchmarkutils.R")

runExperiment8 <- function(datasets, nRuns = 1, classification = FALSE) {
  nclust.methods <- c('Uniform', 'Binomial','Poisson','Constant')
  fullResults <- data.frame()
  shortResult <- data.frame()
  for (ds in datasets) {

    cat('Dataset: ', ds$name, '\n')
    set.seed(1998)
    sampleByRun <- sapply(1:nRuns, function(i) sampleTwoThirds(ds$dataset))
    seeds <- sapply(1:nRuns, function(i) round(runif(1) * 10000000))
    for (nclust.method in nclust.methods) {
      cat('Executing prop proptotypes ', nclust.method, ' \n')

      nrmseOrAcc <- numeric(nRuns)
      times <- numeric(nRuns)

      cat('  # Runs: ')
      for (i in 1:nRuns) {
        set.seed(seeds[i])
        cat(i, '')
        myTic()
        clust.control <- list(clust.method = 'R', nclust.method = nclust.method, hp = 0.1)
        r1 <- snn.bagging(ds$formula, subset = sampleByRun[, i], ds$dataset, snnbag.rowMethod = 'C',bagging.method = 'A', p.control = list(method = 'Opt'), simil.types = ds$simil.types, trace = FALSE, regularization = FALSE, clust.control = clust.control)
        nrmseOrAcc[i] <- ifelse(!is.null(r1$nrmse), r1$nrmse, r1$testAccuracy)
        times[i] <- myToc(print = FALSE)

        newRow <- data.frame(dataset = ds$name, nclust.method = nclust.method, run = i, saccOrNRMSE = nrmseOrAcc[i], time = times[i])
        fullResults <- rbind(fullResults, newRow)
      }
      cat('\n')

      newRow <- data.frame(dataset = ds$name, nclust.method = nclust.method, accMean = mean(nrmseOrAcc), accSd = sd(nrmseOrAcc), timeMean = mean(times), timeSd = sd(times))
      shortResult <- rbind(shortResult, newRow)

      save(shortResult, fullResults, file = "experiments/exp7TMPData.Rdata")
    }
    save(shortResult, fullResults, file = "experiments/exp7TMPData.Rdata")
  }

  fullResults$fullMethod <- paste(fullResults$nclust.method, sep = '\n')
  z <- list()
  z$fullResults <- fullResults
  z$shortResults <- shortResult

  z
}