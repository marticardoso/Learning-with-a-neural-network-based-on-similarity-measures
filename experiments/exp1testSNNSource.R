# Source of Experiment 1 (Tests the SNN for several parameters).
source("test/testUtils.R")
source("SNN.R")

runSNNOptTests <- function(datasets, nRuns = 10, classification = FALSE, onlyTree = FALSE) {
  
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
    set.seed(1998)
    nRunsDs <- ifelse(nrow(ds$dataset) < 4000, nRuns, min(nRuns, 20))
    sampleByRun <- sapply(1:nRunsDs, function(i) sampleTwoThirds(ds$dataset))
    seeds <- sapply(1:nRunsDs, function(i) round(runif(1) * 10000000))

    if (!onlyTree) {
      for (j in 1:length(pOptMethods)) {
        for (clust.method in clust.methods) {
          for (reg in regOptions) {
            if ((nrow(ds$dataset) < 4000 || (pNames[j] != 'CV' && pNames[j]!='GCV')) && (clust.method == 'R' || nrow(ds$dataset) < 4000) && (!reg || allowRegularization[j])) {
              if (!reg) cat('Executing method', pNames[j], ',', clust.method, '\n')
              else cat('Executing method', pNames[j], ',', clust.method, '*(Reg) \n')

              
              nrmseOrAcc <- numeric(nRunsDs)
              times <- numeric(nRunsDs)

              cat('  # Runs: ')
              for (i in 1:nRunsDs) {
                set.seed(seeds[i])
                cat(i, '')
                myTic()
                r1 <- snn(ds$formula, subset = sampleByRun[, i], ds$dataset, simil.types = ds$simil.types, p.control = pOptMethods[[j]], trace = FALSE, regularization = reg, clust.control = list(clust.method = clust.method))
                nrmseOrAcc[i] <- ifelse(!is.null(r1$nrmse), r1$nrmse, r1$testAccuracy)
                times[i] <- myToc(print = FALSE)

                newRow <- data.frame(dataset = ds$name, method = pNames[j], reg = reg, clust.method = clust.method, run = i, saccOrNRMSE = nrmseOrAcc[i], time = times[i], p = r1$p, nProt = nrow(r1$prototypes))
                fullResults <- rbind(fullResults, newRow)

                r1 <- NULL
                gc()
                save(shortResult, fullResults, file = "experiments/TMPData2.Rdata")
              }
              cat('\n')

              newRow <- data.frame(dataset = ds$name, method = pNames[j], reg = reg, clust.method = clust.method, accMean = mean(nrmseOrAcc), accSd = sd(nrmseOrAcc), timeMean = mean(times), timeSd = sd(times))
              shortResult <- rbind(shortResult, newRow)

              save(shortResult, fullResults, file = "experiments/TMPData2.Rdata")
            }
          }
        }
      }
    }
    save(shortResult, fullResults, file = "experiments/TMPData2.Rdata")

    #Decision Tree
    cat('Executing method: decision tree \n')
    y <- model.response(model.frame(ds$formula, data = ds$dataset, na.action = NULL, drop.unused.levels = TRUE))
    nrmseOrAcc <- numeric(nRunsDs)
    times <- numeric(nRunsDs)
    cat('  # Runs: ')
    
    ds4Tree <- fixDatasetForTree(ds$dataset, perc = ifelse(ds$name != 'Annealing', 0.95, 0.84))
    for (i in 1:nRunsDs) {
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
      newRow <- data.frame(dataset = ds$name, method = 'tree', reg = FALSE, clust.method = '', run = i, saccOrNRMSE = nrmseOrAcc[i], time = times[i], p = -1, nProt = 0)
      fullResults <- rbind(fullResults, newRow)
    }
    cat('\n')
    newRow <- data.frame(dataset = ds$name, method = 'tree', reg = FALSE, clust.method = '', accMean = mean(nrmseOrAcc), accSd = sd(nrmseOrAcc), timeMean = mean(times), timeSd = sd(times))
    shortResult <- rbind(shortResult, newRow)

    save(shortResult, fullResults, file = "experiments/TMPData2.Rdata")
  }

  fullResults$fullMethod <- paste(fullResults$clust.method, paste('OptP:', fullResults$method), fullResults$reg, sep = '\n')
  fullResults[fullResults$method == 'tree',]$fullMethod <- 'Tree'

  shortResult$fullMethod <- paste(shortResult$clust.method, paste('OptP:', shortResult$method), shortResult$reg, sep = '\n')
  shortResult[shortResult$method == 'tree',]$fullMethod <- 'Tree'
  z <- list()
  z$fullResults <- fullResults
  z$shortResults <- shortResult

  
  z
}