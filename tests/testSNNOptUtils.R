source("testUtils.R")
source("SNN.R")
fixDatasetForTree <- function(ds) {

  # Fix to many NA
  ds[, 0.95 < (colSums(is.na(ds)) / nrow(ds))] <- NULL

  #Fix more than 32 levels
  for (colId in 1:ncol(ds)) {
    if (is.factor(ds[, colId]) && nlevels(ds[, colId]) > 32) {
      lev <- rownames(sort(table(ds[, colId]), decreasing = TRUE))
      newLev <- c(lev[1:31], 'Other')
      f <- as.vector(ds[, colId])
      for (j in 32:length(lev))
        f[f == lev[j]] <- 'Other'
      ds[, colId] <- factor(f, levels = newLev)
    }
  }
  ds
}


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


runSNNOptTests <- function(datasets, nRuns = 10, classification = FALSE, onlyTree = FALSE) {
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

  clust.methods <- c('PAM', 'R')
  regOptions <- c(TRUE, FALSE)
  
  fullResults <- data.frame()
  shortResult <- data.frame() 
  for (ds in datasets) {
    cat('Dataset: ', ds$name, '\n')
    #nrmseOrAcc <- matrix(0, nRuns, length(pOptMethods), dimnames = list(1:nRuns, pNames))
    #times <- matrix(0, nRuns, length(pOptMethods), dimnames = list(1:nRuns, pNames))
    sampleByRun <- sapply(1:nRuns, function(i) sampleTwoThirds(ds$dataset))
    seeds <- sapply(1:nRuns, function(i) round(runif(1) * 10000000))

    if (!onlyTree) {
      for (j in 1:length(pOptMethods)) {
        for (clust.method in clust.methods) {
          for (reg in regOptions) {
            if ((nrow(ds$dataset) < 4000 || pNames[j] != 'CV') && (clust.method == 'R' || nrow(ds$dataset) < 4000) && (!reg || allowRegularization[j])) {
              if (!reg) cat('Executing method', pNames[j], ',', clust.method, '\n')
              else cat('Executing method', pNames[j], ',', clust.method, '*(Reg) \n')

              nrmseOrAcc <- numeric(nRuns)
              times <- numeric(nRuns)

              cat('  # Runs: ')
              for (i in 1:nRuns) {
                set.seed(seeds[i])
                cat(i, '')
                myTic()
                r1 <- snn(ds$formula, subset = sampleByRun[, i], ds$dataset, simil.types = ds$simil.types, p.control = pOptMethods[[j]], trace = FALSE, regularization = reg, clust.control = list(clust.method = clust.method))
                nrmseOrAcc[i] <- ifelse(!is.null(r1$nrmse), r1$nrmse, r1$testAccuracy)
                times[i] <- myToc(print = FALSE)

                newRow <- data.frame(dataset = ds$name, method = pNames[j], reg = reg, clust.method = clust.method, saccOrNRMSE = nrmseOrAcc[i], time = times[i], p = r1$p, nProt = nrow(r1$prototypes))
                fullResults <- rbind(fullResults, newRow)
              }
              cat('\n')

              newRow <- data.frame(dataset = ds$name, method = pNames[j], reg = reg, clust.method = clust.method, accMean = mean(nrmseOrAcc), accSd = sd(nrmseOrAcc), timeMean = mean(times), timeSd = sd(times))
              shortResult <- rbind(shortResult, newRow)

              save(shortResult, fullResults, file = "tests/TMPData.Rdata")
            }
          }
        }
      }
    }
    save(shortResult, fullResults, file = "tests/TMPData.Rdata")

    #Decision Tree
    cat('Executing method: decision tree \n')
    y <- model.response(model.frame(ds$formula, data = ds$dataset, na.action = NULL, drop.unused.levels = TRUE))
    nrmseOrAcc <- numeric(nRuns)
    times <- numeric(nRuns)
    cat('  # Runs: ')
    ds4Tree <- fixDatasetForTree(ds$dataset)
    for (i in 1:nRuns) {
      cat(i, '')
      iniTime <- myTic()
      model.tree <- tree(ds$formula, data = ds4Tree[sampleByRun[, i],])

      if (classification) {
        pred <- predict(model.tree, ds4Tree[-sampleByRun[, i],], type = 'class')
        nrmseOrAcc[i] <- accuracy(pred, y[-sampleByRun[, i]])
      } else {
        pred <- predict(model.tree, ds4Tree[-sampleByRun[, i],])
        nrmseOrAcc[i] <- nrmse(pred, y[-sampleByRun[, i]])
      }
      times[i] <- myToc(ini = iniTime, print = FALSE)

      newRow <- data.frame(dataset = ds$name, method = 'tree', reg = FALSE, clust.method = '', saccOrNRMSE = nrmseOrAcc[i], time = times[i], p = -1, nProt = 0)
      fullResults <- rbind(fullResults, newRow)
    }
    cat('\n')
    newRow <- data.frame(dataset = ds$name, method = 'tree', reg = FALSE, clust.method = '', accMean = mean(nrmseOrAcc), accSd = sd(nrmseOrAcc), timeMean = mean(times), timeSd = sd(times))
    shortResult <- rbind(shortResult, newRow)

    save(shortResult, fullResults, file = "tests/TMPData.Rdata")
  }

  fullResults$fullMethod <- paste(fullResults$clust.method, paste('OptP:', fullResults$method), fullResults$reg, sep = '\n')
  fullResults[fullResults$method=='tree',]$fullMethod <- 'Tree'
  z <- list()
  z$fullResults <- fullResults
  z$shortResults <- shortResult

  
  z
}