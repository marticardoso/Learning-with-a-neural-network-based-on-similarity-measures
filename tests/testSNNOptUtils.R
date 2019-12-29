source("testUtils.R")
source("SNN.R")
runRegressionSNNOptTests <- function(formula, ds, nRuns = 10) {
  set.seed(1234)
  pNames <- c('Const', 'Opt', 'GCV', 'CV')
  pOptMethods <- list(NULL, list(method = 'Opt'), list(method = 'GCV'), list(method = 'CV'))
  nrmseOrAcc <- matrix(0, nRuns, length(pOptMethods), dimnames = list(1:nRuns, pNames))
  times <- matrix(0, nRuns, length(pOptMethods), dimnames = list(1:nRuns, pNames))
  sampleByRun <- sapply(1:nRuns, function(i) sampleTwoThirds(ds))

  col <- 1
  myTic()
  for (j in 1:length(pOptMethods)) {
    cat('Executing method', pNames[j], '\n')
    cat('  # Runs: ')
    for (i in 1:nRuns) {
      cat(i, '')
      r1 <- snn(formula, subset = sampleByRun[, i], ds, p.control = pOptMethods[[j]], trace = FALSE)
      nrmseOrAcc[i, col] <- ifelse(!is.null(r1$nrmse), r1$nrmse, r1$testAccuracy)
      times[i, col] <- myToc(print = FALSE)
    }
    col <- col + 1
    cat('\n')
  }

  z <- list()
  z$nrmseOrAcc <- nrmseOrAcc
  z$times <- times
  z
}


runSNNOptTests <- function(datasets, nRuns = 10, classification = FALSE) {
  set.seed(1234)
  if (classification) {
    pNames <- c('Const', 'Opt', 'CV')
    pOptMethods <- list(NULL, list(method = 'Opt'), list(method = 'CV'))
    allowRegularization <- c(TRUE, FALSE, TRUE)
  }
  else {
    pNames <- c('Const', 'Opt', 'GCV', 'CV')
    pOptMethods <- list(NULL, list(method = 'Opt'), list(method = 'GCV'), list(method = 'CV'))
    allowRegularization <- c(TRUE, FALSE, TRUE, TRUE)
  }
 
  
  fullResults <- data.frame()
  shortResult <- data.frame() 
  for (ds in datasets) {
    cat('Dataset: ', ds$name, '\n')
    #nrmseOrAcc <- matrix(0, nRuns, length(pOptMethods), dimnames = list(1:nRuns, pNames))
    #times <- matrix(0, nRuns, length(pOptMethods), dimnames = list(1:nRuns, pNames))
    sampleByRun <- sapply(1:nRuns, function(i) sampleTwoThirds(ds$dataset))
    seeds <- sapply(1:nRuns, function(i) round(runif(1) * 10000000))

    for (j in 1:length(pOptMethods)) {
      for (clust.method in c('PAM', 'R')) {
        for (reg in c(TRUE, FALSE)) {
          if (!reg || allowRegularization[j]) {
            if (!reg) cat('Executing method', pNames[j], '\n')
            else cat('Executing method', pNames[j], '*(Reg) \n')

            nrmseOrAcc <- numeric(nRuns)
            times <- numeric(nRuns)

            cat('  # Runs: ')
            for (i in 1:nRuns) {
              set.seed(seeds[i])
              cat(i, '')
              myTic()
              r1 <- snn(ds$formula, subset = sampleByRun[, i], ds$dataset, p.control = pOptMethods[[j]], trace = FALSE, regularization = reg, clust.control = list(clust.method = clust.method))
              nrmseOrAcc[i] <- ifelse(!is.null(r1$nrmse), r1$nrmse, r1$testAccuracy)
              times[i] <- myToc(print = FALSE)

              newRow <- data.frame(dataset = ds$name, method = pNames[j], reg = reg, clust.method = clust.method, saccOrNRMSE = nrmseOrAcc[i], time = times[i], p = r1$p, nProt = nrow(r1$prototypes))
              fullResults <- rbind(fullResults, newRow)
            }
            cat('\n')

            newRow <- data.frame(dataset = ds$name, method = pNames[j], reg = reg, clust.method = clust.method, accMean = mean(nrmseOrAcc), accSd = sd(nrmseOrAcc), timeMean = mean(times), timeSd = sd(times))
            shortResult <- rbind(shortResult, newRow)
          }
        }
      }
    }
    save(shortResult, fullResults, file = "tests/TMPData.Rdata")
  }
  
  z <- list()
  z$fullResults <- fullResults
  z$shortResults <- shortResult
  z
}