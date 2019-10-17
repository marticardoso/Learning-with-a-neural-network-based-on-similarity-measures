
rm(list = ls())
set.seed (104)

# Set environment
setwd(".")


library(rattle.data)
library(faraway)
library(ggplot2)

source('SNN.R')
source('fp_utils.R')


# Plot fp
plot.fp <- function(values,p){
  r <- sapply(values, function(v) fp(v,p))
  plot(values,r, type="l")
}

par(mfrow=c(2, 2))
plot.fp(seq(0,1,0.01),0.001)
plot.fp(seq(0,1,0.01),0.01)
plot.fp(seq(0,1,0.01),0.1)
plot.fp(seq(0,1,0.01),1)

# Plot dfp
plot.dfp <- function(values,p){
  r <- sapply(values, function(v) dfp(v,p))
  plot(values,r, type="l", main='dfp')
}

plot.dfp(seq(0,1,0.01),0.001)
plot.dfp(seq(0,1,0.01),0.01)
plot.dfp(seq(0,1,0.01),0.1)
plot.dfp(seq(0,1,0.01),1)

# Plot fp and dfp
plot.dfAnddfp <- function(x,values){
  res.fp <- sapply(values, function(v) fp(x,v))
  res.dfp <- sapply(values, function(v) dfp(x,v))
  df <- rbind(data.frame(res.fp=res.fp, p=values, type='fp'),
              data.frame(res.fp=res.dfp, p=values, type='dfp'))
  par(mfrow=c(1, 2))
  plot(values,res.fp, type="l", main='fp')
  plot(values,res.dfp, type="l", main='dfp')
  list(fp=res.fp, dfp=res.dfp)
}

r1<-plot.dfAnddfp(0.6, seq(0,9,0.01))
r2<-plot.dfAnddfp(0.01, seq(0,2,0.01))

plot.a <- function(values){
  r <- sapply(values, function(p) a(p))
  plot(values,r, type="l")
}

plot.a(seq(0,10,0.01))

plot.aAndDa <- function(values){
  res.fp <- sapply(values, function(v) a(v))
  res.dfp <- sapply(values, function(v) da(v))
  df <- rbind(data.frame(res.fp=res.fp, values=values, type='a'),
              data.frame(res.fp=res.dfp, values=values, type='da'))
  ggplot(data=df, aes(x=values, y=res.fp, group=type)) +
    geom_line(aes(color=type))
}
plot.aAndDa(seq(0,10,0.01))



# Numeric output
set.seed(1234)
s <- sample(nrow(prostate),60)
r5 <- snn(lpsa~.,prostate,subset=s, method="lm", x=TRUE, y=TRUE)
r5$testReal
r5$testResponse-r5$testReal
r5$mse


# Optimization of p
set.seed(1234)
res <- optimize_p(r5$x, r5$y, clust.method= "PAM")

r5 <- snn(lpsa~.,prostate,subset=s, method="lm", clust.method="PAM", p= res$bestP, x=TRUE, y=TRUE)
r5$mse

(res <- optimize_p_step2(x.simils, t, model, pInitial=0.1))

func(0.1,res$simils, res$model, res$y)

tmp.E <- function(p) {
  if(p<=0) return(NA)
  snn.res <- predict(res$model, data.frame(apply(res$simils, c(1,2), function(x) fp(x,p))))
  return(sum((res$t - snn.res)^2)/length(res$t))
}

tmp.dE <- function(p) {
  if(p<=0) return(NA)
  snn.res <- predict(res$model, data.frame(apply(res$simils, c(1,2), function(x) fp(x,p))))
  dsnn.res <- predict(res$model, data.frame(apply(res$simils, c(1,2), function(x) dfp(x,p))))
  return(2*sum((res$t - snn.res)*dsnn.res))
}

ps <- seq(0,10,0.01)
E.ps <- sapply(ps, function(p) E.func(p,res$simils, res$y, res$model) )
dE.ps <- sapply(ps, function(p) dE.func(p,res$simils, res$y, res$model) )
plot(ps,E.ps, type='l')
plot(ps,dE.ps, type='l')
which.min(E.ps)




(res2 <- optimize_p_step2(res$simils, res$y, res$model, pInitial=0.1))

# Test p=0.2
m1.res <- optimize_p(r5$x, r5$y, clust.method= "PAM", pInitial=0.2)
ps <- seq(0.01,3,0.01)
E.ps <- sapply(ps, function(p) E.func(p,m1.res$simils, m1.res$y, m1.res$model) )
plot(ps,E.ps, type='l')

dE.ps <- sapply(ps, function(p) dE.func(p,m1.res$simils, m1.res$y, m1.res$model) )
plot(ps[10:1000],dE.ps[10:1000], type='l')
abline(h=0, col="red")
abline(v=0.2, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue")





###########################

tmp.E <- function(p, obj) {
  if(p<=0) return(NA)
  snn.res <- apply(obj$simils, c(1,2), function(x) fp(x,p))
  snn.res <- snn.res %*% c(1,1)
  snn.res <- snn.res
  return(sum((obj$y - snn.res)^2)/length(obj$y))
}

tmp.dE <- function(p,obj) {
  if(p<=0) return(NA)
  
  snn.res <- apply(obj$simils, c(1,2), function(x) fp(x,p))
  snn.res <- snn.res %*% c(1,1)
  snn.res <- snn.res +1
  
  dsnn.res <- apply(obj$simils, c(1,2), function(x) dfp(x,p))
  dsnn.res <- dsnn.res %*% c(1,1)
  dsnn.res <- dsnn.res+1
  res <- -(2/length(obj$y) * sum((obj$y - snn.res)*dsnn.res))
  
  #cat('## Derivative Pval: ', p, ' - doptVal= ', res, '\n')
  return(res)
}

id.min <- 13
id.max <- 200
ps <- seq(0.01,2,0.01)
E.ps <- sapply(ps, function(p) tmp.E(p,failing.obj) )
plot(ps[id.min:id.max],E.ps[id.min:id.max], type='l')

dE.ps <- sapply(ps, function(p) tmp.dE(p,failing.obj) )
plot(ps[id.min:id.max],dE.ps[id.min:id.max], type='l')
abline(h=0, col="red")
abline(v=0.2, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue")
lines(ps[id.min:id.max],E.ps[id.min:id.max], col='green')


failing.obj.v1 <- failing.obj
w <- coef(failing.obj$model)
failing.obj$w <- w
failing.obj$simils <- failing.obj$simils[,c("48","96")]
###########################


# Test p=0.1
m2.res <- optimize_p(r5$x, r5$y, clust.method= "PAM", pInitial=0.1)
E.ps <- sapply(ps, function(p) E.func(p,m2.res$simils, m2.res$y, m2.res$model) )
plot(ps,E.ps, type='l')

dE.ps <- sapply(ps, function(p) dE.func(p,m2.res$simils, m2.res$y, m2.res$model) )
plot(ps[1:1000],dE.ps[1:1000], type='l')
abline(h=0, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue")

m2.res <- optimize_p(r5$x, r5$y, clust.method= "PAM", pInitial=0.1, maxIter=2)
E.ps <- sapply(ps, function(p) E.func(p,m2.res$simils, m2.res$y, m2.res$model) )
plot(ps[0:100],E.ps[0:100], type='l')


w <- c(2,1,0.1,1,0.5,2)
# TMP
E.tmp <- function(p, simils, t) {
  if(p<=0) return(NA)
  x.fp <- apply(simils, c(1,2), function(x) fp(x,p))
  x.fp <- x.fp %*% w
  res <-(sum((t - x.fp)^2)/length(t))
  return(res)
}

dE.tmp <- function(p, simils, t) {
  if(p<=0) return(NA)
  x.fp <- apply(simils, c(1,2), function(x) fp(x,p))
  x.dfp <- apply(simils, c(1,2), function(x) dfp(x,p))
  
  x.fp <- x.fp %*% w
  x.dfp <- x.dfp %*% w
  res <- -(2/length(t) * sum((t - x.fp)*x.dfp))
  return(res)
}

x <- m2.res$simils 
y <- apply(x, c(1,2), function(x) fp(x + runif(1,-1, 1)/6,0.2))
y <- y %*% w #+runif(1,-1,1)

ps <- seq(0,1,0.001)
E.ps <- sapply(ps, function(p) E.tmp(p,x,y) )
plot(ps,E.ps, type='l', main="E")

dE.ps <- sapply(ps, function(p) dE.tmp(p,x,y) )
plot(ps[50:700],dE.ps[50:700], type='l', main="dE")
abline(h=0, col="red")
abline(v=0.2, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue")




m2.res <- optimize_p(r5$x, r5$y, clust.method= "PAM", pInitial=0.1, maxIter=2)
E.ps <- sapply(ps, function(p) E.func(p,m2.res$simils, m2.res$y, m2.res$model) )
plot(ps[0:100],E.ps[0:100], type='l')


# Test several values
(cv.bestP <- optimize_p_cv(res$simils,res$y))
