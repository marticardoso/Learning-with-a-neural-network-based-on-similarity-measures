
source("daisy/daisy2.R")
source("daisy/daisy2Predict.R")
source("daisy/daisy2_noComputation.R")

library(rattle.data)


tmp <- daisy2(wine[20:30,], metric = "gower", fixUnbalancedDiss = TRUE)
tmpV <- 1 - as.matrix(tmp)

predTmp <- daisy2.newObservations(wine[20:30,], attr(tmp, 'daisyObj'))
tmpV2 <- 1 - as.matrix(predTmp)

sum(tmpV - tmpV2) # Should be 0

dObj <- daisy2_noComputation(wine[20:30,], metric = "gower", fixUnbalancedDiss = FALSE)

predTmp2 <- daisy2.newObservations(wine[20:30,], dObj)
tmpV3 <- 1 - as.matrix(sqrt(predTmp2))

sum(tmpV3-tmpV) # Should be 0