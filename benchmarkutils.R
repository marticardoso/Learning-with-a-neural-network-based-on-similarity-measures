#library(microbenchmark)

#Get current milisecond
milisec <- function(ini=0) {
  return(as.numeric(Sys.time()) * 1000 - ini)
}

resetTimers <- function() {
  E.s1 <<- 0
  E.s2 <<- 0
  E.s3 <<- 0
  E.s4 <<- 0
  E.s5 <<- 0
  E.s6 <<- 0
  E.s7 <<- 0
  E.s8 <<- 0
  E.s9 <<- 0
  dE.s1 <<- 0
  dE.s2 <<- 0
  dE.s3 <<- 0
  dE.s4 <<- 0
  dE.s5 <<- 0
  dE.s6 <<- 0
  dE.s7 <<- 0
  dE.s8 <<- 0
  dE.s9 <<- 0
  dE.s10 <<- 0
  dE.s11 <<- 0
}

plotBenchMark <- function(doPlot = TRUE) {
  ds <- c(E.s1, E.s2, E.s3, E.s4, E.s5, E.s6, E.s7, E.s8, E.s9)
  names(ds) <- c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9")
  ds <- ds[ds>0]
  if (doPlot) barplot(ds)
  ds
}

plotBenchMark2 <- function(doPlot = TRUE) {
  ds <- c(dE.s1, dE.s2, dE.s3, dE.s4, dE.s5, dE.s6, dE.s7, dE.s8, dE.s9, dE.s10, dE.s11)
  names(ds) <- c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11")
  ds <- ds[ds > 0]
  if (doPlot) barplot(ds)
  ds
}