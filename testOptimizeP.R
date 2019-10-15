
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
plot(xs,sapply(xs, function(v) fp(v,0.001)), type="l", xlab= 'x', ylab='fp(x)')
lines(xs,sapply(xs, function(v) fp(v,0.01)), type="l", col='red')
lines(xs,sapply(xs, function(v) fp(v,0.1)), type="l", col='blue')
lines(xs,sapply(xs, function(v) fp(v,1)), type="l", col='orange')
lines(xs,sapply(xs, function(v) fp(v,10)), type="l", col='cyan')
legend(0, 1, legend=c("p=0.001", "p=0.01", "p=0.1", "p=1", "p=10"),
       col=c("black", "red","blue",'orange','cyan'), lty=1:1, cex=0.8)

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
set.seed(123)
s <- sample(nrow(prostate),60)
snn.res <- snn(lpsa~.,prostate,subset=s, method="lm", x=TRUE, y=TRUE)
snn.res$mse
snn.res$nrmse

# Run optimization of p (given the snn result)
res <- optimize_p(snn.res$simil.matrix.prot, snn.res$y, method="lm", validation=TRUE, maxIter = 100)

plot(res$ps.evol, res$E.learn.evol, xlab='p', ylab="E(p)")
plot(res$ps.evol, res$E.val.evol,xlab='p', ylab="E(p)")

# Create the new snn model with the best P
snn.res <- snn(lpsa~.,prostate,subset=s, method="lm", clust.method="PAM", p= res$bestP, x=TRUE, y=TRUE)
snn.res$nrmse

# Some plots (E and dE)
par(mfrow=c(2, 1))
ps <- seq(0.5,1.5,0.01)
E.ps <- sapply(ps, function(p) E.func.from_model(p,res$simils, res$y, res$model) )
dE.ps <- sapply(ps, function(p) dE.func(p,res$simils, res$y, extractCoefficients(res$model)) )
plot(ps,E.ps, type='l')
plot(ps,dE.ps, type='l')
abline(h=0, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue")
which.min(E.ps)

(res2 <- optimize_p_given_model(res$simils, res$y, res$model, pInitial=0.1))

# Test initial p = 0.2
m1.res <- optimize_p(snn.res$simil.matrix.prot, snn.res$y, pInitial=0.2)
ps <- seq(0.5,2,0.01)
E.ps <- sapply(ps, function(p) E.func.from_model(p,m1.res$simils, m1.res$y, m1.res$model))
plot(ps,E.ps, type='l')

dE.ps <- sapply(ps, function(p) dE.func(p,m1.res$simils, m1.res$y, extractCoefficients(m1.res$model)) )
plot(ps,dE.ps, type='l')
abline(h=0, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue")


# Test initial p = 0.01
m2.res <- optimize_p(snn.res$simil.matrix.prot, snn.res$y, pInitial=0.01)

plot(m2.res$Efunc.evolution, type='l')
plot(m2.res$ps.evolution, type='l')

ps <- seq(0.01,0.4,0.001)
E.ps <- sapply(ps, function(p) E.func.from_model(p,m2.res$simils, m2.res$y, m2.res$model))
plot(ps,E.ps, type='l')

dE.ps <- sapply(ps, function(p) dE.func(p,m2.res$simils, m2.res$y, extractCoefficients(m2.res$model) ))
plot(ps[1:200],dE.ps[1:200], type='l')
abline(h=0, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue")


# Test Ridge
m3.res <- optimize_p(snn.res$simil.matrix.prot, snn.res$y, method="ridge", pInitial=0.9)
ps <- seq(7,8,0.01)
E.ps <- sapply(ps, function(p) E.func.from_model(p,m3.res$simils, m3.res$y, m3.res$model) )
plot(ps,E.ps, type='l')

dE.ps <- sapply(ps, function(p) dE.func(p,m3.res$simils, m3.res$y, extractCoefficients(m3.res$model) ))
plot(ps,dE.ps, type='l')
abline(h=0, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue")


# BostonHousing example

set.seed(1234)
data(BostonHousing)
s <- sample(nrow(BostonHousing),400)
boss.res <- snn(medv~.,BostonHousing,subset=s, method="lm", hp=0.05, x=TRUE, y=TRUE)
boss.res$mse
boss.res$nrmse

#Run optimization
boss.opt <- optimize_p(boss.res$simil.matrix.prot, boss.res$y, hp=0.04, maxIter = 10)
ps <- seq(0.5,2,0.01)
E.ps <- sapply(ps, function(p) E.func.from_model(p,boss.opt$simils, boss.opt$y, boss.opt$model))
plot(ps[1:300],E.ps[1:300], type='l')
dE.ps <- sapply(ps, function(p) dE.func(p,boss.opt$simils, boss.opt$y, extractCoefficients(boss.opt$model) ))
plot(ps[1:300],dE.ps[1:300], type='l')
abline(h=0, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue")

# Test several values
results <- optimize_p_test_range_of_values(snn.res$simil.matrix.prot,snn.res$y,seq(0.01,3,0.01))
plot(results$ps,results$ps.E, type='l')
abline(v=results$ps[which.min(results$ps.E)], col="blue")


#K fold cross valiation
results <- optimize_p_kFoldCV(snn.res$simil.matrix,colnames(snn.res$simil.matrix.prot), snn.res$y,seq(0.01,2,0.01))
plot(results$ps,results$ps.E, type='l')
abline(v=results$ps[which.min(results$ps.E)], col="blue")

results$ps[which(results$ps.E <= min(results$ps.E))]

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
res <- optimize_p(snn.res$simil.matrix.prot, snn.res$y, method="multinom", pInitial=0.1)
par(mfrow=c(2, 1))

ps <- seq(0.07,0.2,0.0001)
ini <- 1
end <- length(ps)
E.ps <- sapply(ps, function(p) E.func.from_model(p,res$simils, res$y, res$model) )
plot(ps[ini:end],E.ps[ini:end], type='l')
dE.ps <- sapply(ps, function(p) dE.func(p,res$simils, res$y, extractCoefficients(res$model) ))
plot(ps[ini:end],dE.ps[ini:end], type='l')
abline(h=0, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue",lwd=2)
opt <- optimize_p_given_model(res$simils, res$y, res$model, res$bestP)
abline(v=opt$par, col="red")


## Binomial case ##

# The following lines are used to obtain the similarity matrix that will be used in the optimization
set.seed(1234)
s <- sample(nrow(wine),60)
wine2 <- wine
wine2$Type <- wine2$Type == 1
snn.res <- snn(Type~.,wine2,subset=s, method="glm", x=TRUE, y=TRUE)
snn.res$testContingencyTable


res <- optimize_p(snn.res$simil.matrix.prot, snn.res$y, method="glm")

par(mfrow=c(2, 1))

ps <- seq(0.19,0.21,0.0001)
ini <- 1
end <- length(ps)
E.ps <- sapply(ps, function(p) E.func.from_model(p,res$simils, res$y, res$model) )
plot(ps[ini:end],E.ps[ini:end], type='l')
dE.ps <- sapply(ps, function(p) dE.func(p,res$simils, res$y, extractCoefficients(res$model) ))
plot(ps[ini:end],dE.ps[ini:end], type='l')
abline(h=0, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue",lwd=2)
opt <- optimize_p_given_model(res$simils, res$y, res$model, res$bestP)
abline(v=opt$par, col="red")

# BreastCancer #

data(BreastCancer)
summary(BreastCancer)
BreastCancer$Id <- NULL
set.seed(1)
s <- sample(nrow(BreastCancer),500)
bc.snn <- snn(Class~.,BreastCancer,subset=s, method="glm", p = 0.1, hp=0.05)
bc.snn$testContingencyTable

res <- optimize_p(bc.snn$simil.matrix.prot, bc.snn$y, method="glm", maxIter = 100)

ps <- seq(0.1,0.21,0.001)
ini <- 1
end <- length(ps)
E.ps <- sapply(ps, function(p) E.func.from_model(p,res$simils, res$y, res$model) )
plot(ps[ini:end],E.ps[ini:end], type='l')
dE.ps <- sapply(ps, function(p) dE.func(p,res$simils, res$y, extractCoefficients(res$model) ))
plot(ps[ini:end],dE.ps[ini:end], type='l')
abline(h=0, col="red")
abline(v=ps[which.min(E.ps)[1]], col="blue",lwd=2)
opt <- optimize_p_given_model(res$simils, res$y, res$model, res$bestP)
abline(v=opt$par, col="red")


bc.snn <- snn(Class~.,BreastCancer,subset=s, method="ridge", p = 0.1, hp=0.05)
bc.snn$testContingencyTable

res <- optimize_p(bc.snn$simil.matrix.prot, bc.snn$y, method="ridge", pInitial=0.35)


data(Sonar)
summary(Sonar)
set.seed(1)
s <- sample(nrow(Sonar),100)
bc.snn <- snn(Class~.,Sonar,subset=s, method="glm", p = 0.1, hp=0.1)
bc.snn$testContingencyTable

res <- optimize_p(bc.snn$simil.matrix.prot, bc.snn$y, method="glm", pInitial=15, maxIter = 100)

