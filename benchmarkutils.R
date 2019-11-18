#library(microbenchmark)

#Get current milisecond
milisec <- function(ini=0) {
  return(as.numeric(Sys.time()) * 1000 - ini)
}
