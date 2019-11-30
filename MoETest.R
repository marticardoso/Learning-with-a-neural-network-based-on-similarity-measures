rm(list = ls())
set.seed(104)

# Set environment
setwd(".")

source('MoF.R')
n <- 10
p <- 3
m <- 5
x <- matrix(1, n, p)
x[, 1] <- 10

b <- matrix(20, p + 1, m)

t <- matrix(10, 1, n)
snnX <- matrix(2, n, m)
snnX[,1] <- 10
(o <- MoE.E.regression(x, snnX, t, b))
(o2 <- MoE.dE.regression_for(x, snnX, t, b))
(o3 <- MoE.dE.regression_semifor(x, snnX, t, b))
(o4 <- MoE.dE.regression(x, snnX, t, b))
sum(abs(o2 - o3))
sum(abs(o2 - o4))
matrix(as.vector(o), p + 1, m)


func <- function(args) {
  b2 <- matrix(args, p + 1, m)
  MoE.E.regression(x = x, snnX = snnX, t = t, b = b2)
}

grad <- function(args) {
  b2 <- matrix(args, p + 1, m)
  -as.vector(MoE.dE.regression(x = x, snnX = snnX, t = t, b = b2))
}

btmp <- as.vector(b)
res <- optim(as.vector(b), func, grad, method = "BFGS")

func(res$par) # Should be 0
(newB <- matrix(res$par, p + 1, m))

(o <- MoE.E.regression(x, snnX, t, b))
(o <- MoE.E.regression(x, snnX, t, newB))


# Test optimization

MoE.optimize(x, snnX, t)


