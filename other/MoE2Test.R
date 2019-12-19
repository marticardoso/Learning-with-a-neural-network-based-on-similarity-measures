rm(list = ls())
set.seed(104)

# Set environment
setwd(".")

source('MoE2.R')
n <- 10
p <- 3
m <- 5
x <- matrix(1, n, p)
x[, 1] <- 10

b <- matrix(0.1, p + 1, m)

t <- matrix(10, 1, n)
snnX <- matrix(2, n, m)
snnX[,1] <- 10
(o <- MoE2.E.regression(x, snnX, t, b))
(o4 <- MoE2.dE.regression(x, snnX, t, b))
matrix(as.vector(o), p + 1, m)


func <- function(args) {
  b2 <- matrix(args, p + 1, m)
  MoE2.E.regression(x = x, snnX = snnX, t = t, b = b2)
}

grad <- function(args) {
  b2 <- matrix(args, p + 1, m)
  -as.vector(MoE2.dE.regression(x = x, snnX = snnX, t = t, b = b2))
}

btmp <- as.vector(b)
res <- optim(as.vector(b), func, grad, method = "BFGS")

func(res$par) # Should be 0
(newB <- matrix(res$par, p + 1, m))

(o <- MoE2.E.regression(x, snnX, t, b))
(o <- MoE2.E.regression(x, snnX, t, newB))


# Test optimization
predByM <- list(1:2, 1, 3, 1:3, 2:3)
MoE2.optimize(x, snnX, t, 'numeric', predByM = predByM)


##################################
# Binomail

n <- 10
p <- 3
m <- 5
x <- matrix(1, n, p)
x[, 1] <- 10

b <- matrix(-1, p + 1, m)

t <- matrix(10, 1, n) > 5
snnX <- matrix(0, n, m)
snnX[, 1] <- 1
snnX[, 2] <- 1
(o <- MoE2.E.binomial(x, snnX, t, b))
(o2 <- MoE2.dE.binomial(x, snnX, t, b))


func <- function(args) {
  b2 <- matrix(args, p + 1, m)
  MoE2.E.binomial(x = x, snnX = snnX, t = t, b = b2)
}

grad <- function(args) {
  b2 <- matrix(args, p + 1, m)
  -as.vector(MoE2.dE.binomial(x = x, snnX = snnX, t = t, b = b2))
}

btmp <- as.vector(b)
res <- optim(as.vector(b), func, grad, method = "BFGS")

func(res$par) # Should be 0
grad(res$par)
(newB <- matrix(res$par, p + 1, m))

(o <- MoE2.E.binomial(x, snnX, t, b))
(o <- MoE2.E.binomial(x, snnX, t, newB))
r <- MoE2.optimize(x, snnX, t, 'binomial')
MoE2.E.binomial(x, snnX, t, r$b)


t <- factor(t,levels = c('TRUE','FALSE'))
(o <- MoE2.E.binomial(x, snnX, t, b))
(o2 <- MoE2.dE.binomial(x, snnX, t, b))
r <- MoE2.optimize(x, snnX, t, 'binomial', predByM = predByM)
MoE2.E.binomial(x, snnX, t, r$b)

##################################
# Multinomial

n <- 10
p <- 3
m <- 5
x <- matrix(1, n, p)
x[, 1] <- 10

b <- matrix(20, p + 1, m)

t <- factor(rep("c2",n), levels = c("c1","c2","c3"))
snnX <- matrix(0, n, m*3)
snnX[, 2] <- 1
snnX[, 1 * 3 + 1] <- 1
snnX[, 2 * 3 + 1] <- 1
snnX[, 3 * 3 + 1] <- 1
snnX[, 4 * 3 + 1] <- 1
(o <- MoE2.E.multinomial(x, snnX, t, b))
(o2 <- MoE2.dE.multinomial(x, snnX, t, b))

func <- function(args) {
  b2 <- matrix(args, p + 1, m)
  MoE2.E.multinomial(x = x, snnX = snnX, t = t, b = b2)
}

grad <- function(args) {
  b2 <- matrix(args, p + 1, m)
  - as.vector(MoE2.dE.multinomial(x = x, snnX = snnX, t = t, b = b2))
}

btmp <- as.vector(b)
res <- optim(as.vector(b), func, grad, method = "BFGS")

func(res$par)
(newB <- matrix(res$par, p + 1, m))

(o <- MoE2.E.multinomial(x, snnX, t, b))
(o <- MoE2.E.multinomial(x, snnX, t, newB))


# Test optimization

r <- MoE2.optimize(x, snnX, t, 'multinomial', predByM = predByM)
MoE2.E.multinomial(x, snnX, t, r$b)
r2 <- MoE2.optimize(x, snnX, t, 'multinomial', bIni = as.vector(r$b))
MoE2.E.multinomial(x, snnX, t, r2$b)
