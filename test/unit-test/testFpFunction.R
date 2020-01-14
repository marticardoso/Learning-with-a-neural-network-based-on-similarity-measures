# In this file, there are some unit tests that check the right execution of the fp functions (and derivatives)
# (this is not an experiment)

rm(list = ls())
set.seed (104)


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
