source("testUtils.R")

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
  }
  else {
    pNames <- c('Const', 'Opt', 'GCV', 'CV')
    pOptMethods <- list(NULL, list(method = 'Opt'), list(method = 'GCV'), list(method = 'CV'))
  }
 
  

  fdResult <- data.frame() 
  for (ds in datasets) {
    cat('Dataset: ', ds$name, '\n')
    nrmseOrAcc <- matrix(0, nRuns, length(pOptMethods), dimnames = list(1:nRuns, pNames))
    times <- matrix(0, nRuns, length(pOptMethods), dimnames = list(1:nRuns, pNames))
    sampleByRun <- sapply(1:nRuns, function(i) sampleTwoThirds(ds$dataset))
    col <- 1

    seeds <- sapply(1:nRuns, function(i) round(runif(1)*10000000))
    
    for (j in 1:length(pOptMethods)) {
      cat('Executing method', pNames[j], '\n')
      cat('  # Runs: ')
      for (i in 1:nRuns) {
        set.seed(seeds[i])
        cat(i, '')
        myTic()
        r1 <- snn(ds$formula, subset = sampleByRun[, i], ds$dataset, p.control = pOptMethods[[j]], trace = FALSE)
        nrmseOrAcc[i, col] <- ifelse(!is.null(r1$nrmse), r1$nrmse, r1$testAccuracy)
        times[i, col] <- myToc(print = FALSE)
      }
      col <- col + 1
      cat('\n')
    }

    dsR <- data.frame(dataset=ds$name, method = pNames, mean = colMeans(nrmseOrAcc), sd = colSd(nrmseOrAcc), timeMean = colMeans(times), timeSd = colSd(times))
    fdResult <- rbind(fdResult, dsR)
  }
  

  fdResult
}