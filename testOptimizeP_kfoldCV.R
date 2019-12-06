

source('SNN.R')
library(rattle.data)
library(faraway)
library(mlbench)
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

#################################
## Test without regularization ##
#################################

r <- optimize_p_kFoldCV(gSimils, gY, regularization = FALSE)
plot(r$ps, r$E, type = 'l')

ggplot(data = data.frame(E = r$E, p = r$ps), aes(x = p, y = E)) +
    geom_line() + geom_point() +
    xlab('ps') + ylab(paste('E'))
 
##############################
## Test with regularization ##
##############################
r <- optimize_p_kFoldCV(gSimils, gY, regularization = TRUE)

ds <- expand.grid(l = r$lambdas, p = r$ps)
ds$value <- as.vector(t(r$E))
# Heatmap 
ggplot(ds, aes(l, p, fill = log(value))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") + scale_x_continuous(trans = 'log10') +
  xlab('lambda')

plot.byLambda <- function(r, lId) {
  df <- expand.grid(lambda = r$lambdas, p = r$ps)
  df$value <- as.vector(t(r$E))
  df <- df[df$lambda == r$lambdas[lId],]
  ggplot(data = df, aes(x = p, y = value)) +
    geom_line() + geom_point() +
    xlab('ps') + ylab(paste('E (lambda =', r$lambdas[lId], ')'))
}
plot.byLambda(r, 36)
plot.byLambda(r, 10)

plot.byP <- function(r, pId) {
  df <- expand.grid(lambda = r$lambdas, p = r$ps)
  df$value <- as.vector(t(r$E))
  df <- df[df$p == r$ps[pId],]
  ggplot(data = df, aes(x = lambda, y = value)) +
    geom_line() + geom_point() +
    scale_x_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10 ^ x), labels = trans_format("log10", math_format(10 ^ .x))) +
    xlab('lambdas') + ylab(paste('E (p =', r$ps[pId], ')'))

}
plot.byP(r, 2)
plot.byP(r, 20)



# Classification problem
reg.lm <- snn(Type ~ ., wine, regularization = FALSE,
              clust.control = list(clust.method = "PAM", nclust.method = "C"))
gSimils <- reg.lm$simil.matrix.prot
gY <- reg.lm$y

r <- optimize_p_kFoldCV(gSimils, gY, regularization = FALSE)
plot(r$ps, r$E, type = 'l')

ggplot(data = data.frame(E = r$E, p = r$ps), aes(x = p, y = E)) +
    geom_line() + geom_point() +
    xlab('ps') + ylab(paste('E'))

#Regularization
r <- optimize_p_kFoldCV(gSimils, gY, regularization = TRUE)
ds <- expand.grid(l = r$lambdas, p = r$ps)
ds$value <- as.vector(t(r$E))
# Heatmap 
ggplot(ds, aes(l, p, fill = log(value))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") + scale_x_continuous(trans = 'log10') +
  xlab('lambda')


# Binomial case
wine2 <- wine
wine2$Type <- NULL
wine2$Type1 <- wine$Type == 1

reg.lm <- snn(Type1 ~ ., wine2, regularization = FALSE,
              clust.control = list(clust.method = "PAM", nclust.method = "C"))
gSimils <- reg.lm$simil.matrix.prot
gY <- reg.lm$y

r <- optimize_p_kFoldCV(gSimils, gY, regularization = FALSE)
ggplot(data = data.frame(E = r$E, p = r$ps), aes(x = p, y = E)) +
    geom_line() + geom_point() +
    xlab('ps') + ylab(paste('E'))

#Regularization
r <- optimize_p_kFoldCV(gSimils, gY, regularization = TRUE)
ds <- expand.grid(l = r$lambdas, p = r$ps)
ds$value <- as.vector(t(r$E))
ggplot(ds, aes(l, p, fill = log(value))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") + scale_x_continuous(trans = 'log10') +
  xlab('lambda')