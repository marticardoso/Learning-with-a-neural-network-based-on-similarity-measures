
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
r <- optimize_p_GCV(gSimils, gY, regularization = TRUE)

ds <- expand.grid(l = r$lambdas, p = r$ps)
ds$value <- as.vector(t(r$grid.search))
# Heatmap 
ds <- ds[ds$l<=5,]
ggplot(ds, aes(l, p, fill = log(value))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") + scale_x_continuous(trans = 'log10') +
  xlab('lambda')


plot.byLambda <- function(r, lId) {
  df <- expand.grid(lambda = r$lambdas, p = r$ps)
  df$value <- as.vector(t(r$grid.search))
  df <- df[df$lambda == r$lambdas[lId],]
  ggplot(data = df, aes(x = p, y = value)) +
    geom_line() + geom_point() +
    xlab('ps') + ylab(paste('E (lambda =', r$lambdas[lId], ')'))
}
plot.byLambda(r, 36)
plot.byLambda(r, 10)


plot.byP <- function(r, pId) {
  df <- expand.grid(lambda = r$lambdas, p = r$ps)
  df$value <- as.vector(t(r$grid.search))
  df <- df[df$p==r$ps[pId],]
  ggplot(data = df, aes(x = lambda, y = value)) +
    geom_line() + geom_point() +
    scale_x_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10 ^ x), labels = trans_format("log10", math_format(10 ^ .x))) +
    xlab('lambdas') + ylab(paste('E (p =',r$ps[pId],')'))
  
}
plot.byP(r, 2)
plot.byP(r, 20)
r2 <- optimize_p_kFoldCV(gSimils, gY)

#################################
## Test without regularization ##
#################################

r <- optimize_p_GCV(gSimils, gY, regularization = FALSE)
plot.byLambda(r, 1)

