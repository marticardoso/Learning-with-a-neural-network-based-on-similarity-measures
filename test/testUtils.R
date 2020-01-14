source('test/benchmarkutils.R')

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

colSd <- function(df) {
  apply(df, 2, sd)
}

fixDatasetForTree <- function(ds, perc = 0.95) {

  # Fix to many NA
  ds[, perc < (colSums(is.na(ds)) / nrow(ds))] <- NULL

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



fixDatasetForRF <- function(ds) {

  for (colId in 1:ncol(ds)) {
    if (is.logical(ds[, colId])) {
      ds[, colId] <- factor(ds[, colId])
    }
  }
  
  for (colId in 1:ncol(ds)) {
    if (is.factor(ds[, colId]) && nlevels(ds[, colId]) > 52) {
      lev <- rownames(sort(table(ds[, colId]), decreasing = TRUE))
      newLev <- c(lev[1:52], 'Other')
      f <- as.vector(ds[, colId])
      for (j in 53:length(lev))
        f[f == lev[j]] <- 'Other'
      ds[, colId] <- factor(f, levels = newLev)
    }
  }

  ds <- na.roughfix(ds)
  ds
}