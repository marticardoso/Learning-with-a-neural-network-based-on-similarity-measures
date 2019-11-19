
library(ggplot2)
library(scales)

data(BostonHousing)
dim(BostonHousing)

set.seed(1234)
s <- sample(nrow(BostonHousing), 400)
reg.lm <- snn(medv ~ ., BostonHousing, subset = s, regularization = FALSE,
              clust.control = list(clust.method = "PAM", nclust.method = "C"),
              p.control = list(method = 'CV'))
gSimils <- reg.lm$simil.matrix.prot
gY <- reg.lm$y

##############################
## Test with regularization ##
##############################
r <- optimize_p_kFoldCV(gSimils, gY)
plot(r$ps, r$E, type = 'l')

#################################
## Test without regularization ##
#################################

r <- optimize_p_kFoldCV(gSimils, gY, regularization = FALSE)
plot(r$ps, r$E, type = 'l')

