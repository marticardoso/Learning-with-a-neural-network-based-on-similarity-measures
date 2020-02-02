# Source of Experiment 7 (Tests the number of prototypes used in an SNN).
source("test/testUtils.R")
source("SNNBagging.R")
source("test/benchmarkutils.R")

runExperiment7 <- function(datasets, nRuns = 1, classification = FALSE) {
    clust.methods <- c('PAM')

  fullResults <- data.frame()
  shortResult <- data.frame()
  for (ds in datasets) {

    cat('Dataset: ', ds$name, '\n')
    set.seed(1998)
    sampleByRun <- sapply(1:nRuns, function(i) sampleTwoThirds(ds$dataset))
    seeds <- sapply(1:nRuns, function(i) round(runif(1) * 10000000))
    for (propPrototypes in c(0, 0.05, seq(0.1, 1, 0.1))) {
        for (clust.method in clust.methods) {
          cat('Executing prop proptotypes ', propPrototypes, ' \n')

          nrmseOrAcc <- numeric(nRuns)
          times <- numeric(nRuns)

          cat('  # Runs: ')
          for (i in 1:nRuns) {
            set.seed(seeds[i])
            cat(i, '')
            myTic()
            clust.control <- list(clust.method = clust.method, nclust.method = 'C', hp = propPrototypes)
            r1 <- snn(ds$formula, subset = sampleByRun[, i], ds$dataset, simil.types = ds$simil.types, p.control = list(method = 'Opt'), trace = FALSE, regularization = FALSE, clust.control = clust.control)
            nrmseOrAcc[i] <- ifelse(!is.null(r1$nrmse), r1$nrmse, r1$testAccuracy)
            times[i] <- myToc(print = FALSE)

            newRow <- data.frame(dataset = ds$name, clust.method = clust.method, propPrototypes = propPrototypes, run = i, saccOrNRMSE = nrmseOrAcc[i], time = times[i])
            fullResults <- rbind(fullResults, newRow)
          }
          cat('\n')

          newRow <- data.frame(dataset = ds$name,  clust.method = clust.method, propPrototypes = propPrototypes, accMean = mean(nrmseOrAcc), accSd = sd(nrmseOrAcc), timeMean = mean(times), timeSd = sd(times))
          shortResult <- rbind(shortResult, newRow)

          save(shortResult, fullResults, file = "experiments/exp7TMPData.Rdata")
        }
      }
      save(shortResult, fullResults, file = "experiments/exp7TMPData.Rdata")
  }

  fullResults$fullMethod <- paste(fullResults$clust.method, fullResults$propPrototypes, sep = '\n')
  z <- list()
  z$fullResults <- fullResults
  z$shortResults <- shortResult

  z
}