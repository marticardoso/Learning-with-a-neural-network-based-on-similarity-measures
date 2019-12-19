
sampleTwoThirds <- function(ds) {
  sample(nrow(ds), floor(2 / 3 * nrow(ds)))
}

accuracy <- function(response, real) {
  tab <- table(Truth = real, Pred = response)
  sum(diag(tab)) / sum(tab) * 100
}
nrmse <- function(response, real) {
  sum((response - real) ^ 2) / ((length(real) - 1) * var(real))
}


runClassificationEnsembleTests <- function(formula, ds, nRuns = 10) {
  set.seed(1234)
  
  methods <- c("A", "A2", "B", "BReg", "C","D","E")
  accs <- matrix(0, nRuns, length(methods), dimnames = list(1:nRuns, methods))
  times <- matrix(0, nRuns, length(methods), dimnames = list(1:nRuns, methods))
  sampleByRun <- sapply(1:nRuns, function(i) sampleTwoThirds(ds))

  col <- 1
  myTic()
  for (method in methods) {
    cat('Executing method', method, '\n')
    cat('  # Runs: ')
    reg <- endsWith(method, "Reg")
    if (reg) method <- substr(method, 1, nchar(method) - 3)
    for (i in 1:nRuns) {
      cat(i, '')
      r1 <- snn.bagging(formula, subset = sampleByRun[, i], ds, bagging.method = method, trace = FALSE, regularization = reg)
      accs[i, col] <- r1$testAccuracy
      times[i, col] <- myToc(print=FALSE)
    }
    col <- col + 1
    cat('\n')
  }
  
  z <- list()
  z$accuracies <- accs
  z$times <- times
  z
}

runRegressionEnsembleTests <- function(formula, ds, nRuns = 10) {
  set.seed(1234)

  methods <- c("A", "B", "BReg", "C", "D", "E")
  nrmses <- matrix(0, nRuns, length(methods), dimnames = list(1:nRuns, methods))
  times <- matrix(0, nRuns, length(methods), dimnames = list(1:nRuns, methods))
  sampleByRun <- sapply(1:nRuns, function(i) sampleTwoThirds(ds))

  col <- 1
  myTic()
  for (method in methods) {
    cat('Executing method', method, '\n')
    cat('  # Runs: ')
    reg <- endsWith(method, "Reg")
    if (reg) method <- substr(method, 1, nchar(method) - 3)
    for (i in 1:nRuns) {
      cat(i, '')
      r1 <- snn.bagging(formula, subset = sampleByRun[, i], ds, bagging.method = method, trace = FALSE, regularization = reg)
      nrmses[i, col] <- r1$nrmse
      times[i, col] <- myToc(print = FALSE)
    }
    col <- col + 1
    cat('\n')
  }

  z <- list()
  z$nrmse <- nrmses
  z$times <- times
  z
}