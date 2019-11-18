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
