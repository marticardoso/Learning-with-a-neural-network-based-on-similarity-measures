
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