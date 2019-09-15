
rm(list = ls())
set.seed (104)

# Set environment
setwd(".")


library(rattle.data)
library(faraway)
library(ggplot2)
library(mlbench)

source('SNN.R')
source('fp_utils.R')

#####
# Plots related to fp and a
#####

# Plot fp
plot.fp <- function(values,p){
  r <- sapply(values, function(v) fp(v,p))
  plot(values,r, type="l", main= paste('fp (p=',p,')'))
}

par(mfrow=c(2, 3))
xs <- seq(0,1,0.01)
plot.fp(xs,0.001)
plot.fp(xs,0.01)
plot.fp(xs,0.1)
plot.fp(xs,1)
plot.fp(xs,10)
plot.fp(xs,100)

par(mfrow=c(1, 1))
plot(xs,sapply(xs, function(v) fp(v,0.001)), type="l")
lines(xs,sapply(xs, function(v) fp(v,0.01)), type="l", col='red')
lines(xs,sapply(xs, function(v) fp(v,0.1)), type="l", col='blue')
lines(xs,sapply(xs, function(v) fp(v,1)), type="l", col='orange')
lines(xs,sapply(xs, function(v) fp(v,10)), type="l", col='cyan')

# Plot dfp
plot.dfp <- function(values,p){
  r <- sapply(values, function(v) dfp(v,p))
  plot(values,r, type="l", main='dfp')
}
par(mfrow=c(2, 2))
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


plot.aAndDa <- function(values){
  res.fp <- sapply(values, function(v) a(v))
  res.dfp <- sapply(values, function(v) da(v))
  df <- rbind(data.frame(res.fp=res.fp, values=values, type='a'),
              data.frame(res.fp=res.dfp, values=values, type='da'))
  ggplot(data=df, aes(x=values, y=res.fp, group=type)) +
    geom_line(aes(color=type))
}
plot.aAndDa(seq(0,10,0.01))

graphics.off()

#####
# Test optimization for regression
#####
# It is tested the optimization of p for numeric output

# The following lines are used to obtain the similarity matrix that will be used in the optimization
set.seed(1234)
s <- sample(nrow(prostate),60)
snn.res <- snn(lpsa~.,prostate,subset=s, method="lm", x=TRUE, y=TRUE)
snn.res$mse
snn.res$nrmse

# Run optimization of p (given the snn result)
res <- optimize_p(snn.res$simil.matrix.prot, snn.res$y)

# Create the new snn model with the best P
snn.res <- snn(lpsa~.,prostate,subset=s, method="lm", clust.method="PAM", p= res$bestP, x=TRUE, y=TRUE)
snn.res$nrmse

# Some plots (E and dE)
ps <- seq(0,10,0.01)
E.ps <- sapply(ps, function(p) E.func(p,res$simils, res$y, res$model) )
dE.ps <- sapply(ps, function(p) dE.func(p,res$simils, res$y, res$model) )
plot(ps,E.ps, type='l')
plot(ps,dE.ps, type='l')
which.min(E.ps)

(res2 <- optimize_p_given_model(res$simils, res$y, res$model, pInitial=0.1))

# Test initial p = 0.2
m1.res <- optimize_p(snn.res$simil.matrix.prot, snn.res$y, pInitial=0.2)
ps <- seq(0.01,3,0.01)
E.ps <- sapply(ps, function(p) E.func(p,m1.res$simils, m1.res$y, m1.res$model) )
plot(ps,E.ps, type='l')

dE.ps <- sapply(ps, function(p) dE.func(p,m1.res$simils, m1.res$y, m1.res$model) )
plot(ps[50:200],dE.ps[50:200], type='l')
abline(h=0, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue")


# Test initial p = 0.01
m2.res <- optimize_p(snn.res$simil.matrix.prot, snn.res$y, pInitial=0.01)
E.ps <- sapply(ps, function(p) E.func(p,m2.res$simils, m2.res$y, m2.res$model) )
plot(ps,E.ps, type='l')

dE.ps <- sapply(ps, function(p) dE.func(p,m2.res$simils, m2.res$y, m2.res$model) )
plot(ps[1:200],dE.ps[1:200], type='l')
abline(h=0, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue")


# Test Ridge
m3.res <- optimize_p(snn.res$simil.matrix.prot, snn.res$y, method="ridge", pInitial=0.5)
E.ps <- sapply(ps, function(p) E.func(p,m3.res$simils, m3.res$y, m3.res$model) )
plot(ps,E.ps, type='l')

dE.ps <- sapply(ps, function(p) dE.func(p,m3.res$simils, m3.res$y, m3.res$model) )
plot(ps[10:200],dE.ps[10:200], type='l')
abline(h=0, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue")


# BostonHousing example

set.seed(1234)
data(BostonHousing)
s <- sample(nrow(BostonHousing),400)
boss.res <- snn(medv~.,BostonHousing,subset=s, method="lm", x=TRUE, y=TRUE)
boss.res$mse
boss.res$nrmse

#Run optimization
boss.opt <- optimize_p(boss.res$simil.matrix.prot, boss.res$y)
ps <- seq(0.01,3,0.01)
E.ps <- sapply(ps, function(p) E.func(p,boss.opt$simils, boss.opt$y, boss.opt$model) )
plot(ps[1:300],E.ps[1:300], type='l')
dE.ps <- sapply(ps, function(p) dE.func(p,boss.opt$simils, boss.opt$y, boss.opt$model) )
plot(ps[1:300],dE.ps[1:300], type='l')
abline(h=0, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue")

# Test several values
results <- optimize_p_test_range_of_values(snn.res$simil.matrix.prot,res$y,seq(0.01,3,0.01))
plot(results$ps,results$ps.E, type='l')
abline(v=results$ps[which.min(results$ps.E)], col="blue")


# Greedy approaches
v3.results <- optimize_p_method3(snn.res$dissim.matrix,snn.res$findclusters.res)
barplot(v3.results$results)
abline(h=v3.results$avg, col="blue")
v3.results$avg




## Multinomial case ##

# The following lines are used to obtain the similarity matrix that will be used in the optimization
set.seed(1234)
s <- sample(nrow(wine),60)
snn.res <- snn(Type~.,wine,subset=s, method="multinom", x=TRUE, y=TRUE)
snn.res$testContingencyTable


# Run optimization of p (given the snn result)
res <- optimize_p(snn.res$simil.matrix.prot, snn.res$y, method="multinom", pInitial=0.4)
par(mfrow=c(2, 1))

ps <- seq(0.39,0.42,0.01)
ini <- 1
end <- length(ps)
E.ps <- sapply(ps, function(p) E.multinom(p,gSimils, gT, gModel) )
plot(ps[ini:end],E.ps[ini:end], type='l')
dE.ps <- sapply(ps, function(p) dE.multinom(p,gSimils, gT, gModel) )
plot(ps[ini:end],dE.ps[ini:end], type='l')
abline(h=0, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue")



#DELETE

E.multinom <- function(p, simils, t, model){
  if(p<=0) return(NA)
  w <- coef(model)  #Extract w
  colnames(w) <- gsub('X','',colnames(w)) # Remove X from names
  
  prototypes <- colnames(w)[-which(colnames(w) %in% c("(Intercept)"))]
  
  X <- apply(simils[,prototypes], c(1,2), function(x) fp(x,p))
  X <- cbind(1, X)
  snn.res <- X %*% t(w)
  snn.res <- cbind(0,snn.res)
  nnetRes <- snn.res 
  nnetRes <- t(apply(snn.res, 1, function(r) r/sum(r)))
  
  real <- class.ind(t)
  
  res <- sum((nnetRes -real)^2)/(length(t))
  return(res)
}

dE.multinom <- function(p, simils, t, model){
  if(p<=0) return(NA)
  w <- coef(model)  #Extract w
  colnames(w) <- gsub('X','',colnames(w)) # Remove X from names
  prototypes <- colnames(w)[-which(colnames(w) %in% c("(Intercept)"))]
  
  X <- apply(simils[,prototypes], c(1,2), function(x) fp(x,p))
  X <- cbind(1, X) # Add intercept column
  nnetRes <- X %*% t(w) # w has intercept
  nnetRes <- cbind(0,nnetRes) # Added base class
  
  snnRes <- t(apply(nnetRes, 1, function(r) exp(r)))#/sum(exp(r))))
  
  
  w0 <- w[,"(Intercept)"]                         #Intercept
  w <- w[,-which(colnames(w) %in% c("(Intercept)"))] # Remove intercept
  
  X <- apply(simils[,prototypes], c(1,2), function(x) dfp(x,p))
  dnnet <- X %*% t(w) # No intercept
  dnnet <- cbind(0,dnnet) # First class has 0 derivative
  
  dnnetRes <- dnnet
  for(i in 1:nrow(dnnetRes)){
    rowExpSum <- sum(snnRes[i,])
    dRowExpSum <- sum(dnnet[i,])
    for(j in 1:ncol(dnnetRes)){
      dnnetRes[i,j] <- dnnet[i,j]*rowExpSum - snnRes[i,j]*dRowExpSum
      dnnetRes[i,j] <- dnnetRes[i,j]/rowExpSum^2
    }
  }
  
  real <- class.ind(t)
  
  res <- -2/(length(t)) * sum((real-snnRes) *dnnetRes)
  return(res)
}

gId <- 1:60
pCoef <- 0
cols <- c(1,2)
E.multinom <- function(p, simils, t, model){
  if(p<=0) return(NA)
  w <- coef(model)  #Extract w
  colnames(w) <- gsub('X','',colnames(w)) # Remove X from names
  
  prototypes <- colnames(w)[-which(colnames(w) %in% c("(Intercept)"))]
  
  
  X <- apply(simils[,prototypes], c(1,2), function(x) fp(x,p))
  X <- cbind(1, X)
  X <- X %*% t(w)
  X <- cbind(0,X)
  #nnetRes <- snn.res 
  nnetRes <- t(apply(X, 1, function(r) exp(r)/sum(exp(r))))
  real <- class.ind(t)
  return(sum((real-nnetRes)^2) +pCoef*p)
}


dE.multinom <- function(p, simils, t, model){
  if(p<=0) return(NA)
  
  w <- coef(model)  #Extract w
  colnames(w) <- gsub('X','',colnames(w)) # Remove X from names
  prototypes <- colnames(w)[-which(colnames(w) %in% c("(Intercept)"))]
  
  
  X <- apply(simils[,prototypes], c(1,2), function(x) fp(x,p))
  X <- cbind(1, X)
  X <- X %*% t(w)
  X <- cbind(0,X)
  
  dX <- apply(simils[,prototypes], c(1,2), function(x) dfp(x,p))
  wNoInter <- w[,-which(colnames(w) %in% c("(Intercept)"))] # Remove intercept
  dX <- dX %*% t(wNoInter)
  dX <- cbind(0,dX)
 
  nnetRes <- t(apply(X, 1, function(r) exp(r)/sum(exp(r))))

  dnnetRes <- matrix(0,dim(nnetRes)[1], dim(nnetRes)[2])
  for(i in 1:nrow(dnnetRes)){
    dnnetRes[i,] <- (sum(exp(X[i,]))*exp(X[i,])*dX[i,]-exp(X[i,])*sum(exp(X[i,])*dX[i,]))/(sum(exp(X[i,])))^2
  }
  
 
  real <- class.ind(t)
  return(-2*sum((real-nnetRes)*dnnetRes) + pCoef)
}


