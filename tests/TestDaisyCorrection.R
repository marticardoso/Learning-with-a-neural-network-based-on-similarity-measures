
load("WineCorrectedDissimilaritiesSubset.Rda")

r2 <- snn(Type~.,wine,subset=errorSubset,regularization=TRUE)

hist(gDisv, main='Dissimilarity histogram', xlab='Dissimilarity')
hist(sqrt(gDisv), main='Corrected dissimilarity histogram', xlab='Sqrt(dissimilarity)')

save(errorSubset,file="WineCorrectedDissimilaritiesSubset.Rda")

df.m <- data.frame()
for(i in 1:10){
  r2 <- snn(Type~.,wine)

  iDf <- data.frame(value=gDisv)
  iDf$method <- paste("F",i, sep="")
  iDf$correction <- FALSE
  
  ciDf <- data.frame(value=gCDisv)
  ciDf$method <- paste("F",i, sep="")
  ciDf$correction <- TRUE
  df.m <- rbind(df.m,ciDf, iDf)
}

require(ggplot2)
ggplot(data = df.m, aes(x=method, y=value)) + geom_boxplot(aes(fill=correction)) + ylim(0, 1)


# Regression dataset
df.m2 <- data.frame()
for(i in 1:10){
  p <- snn(lpsa~.,prostate)
  iDf <- data.frame(value=gDisv)
  iDf$method <- paste("F",i, sep="")
  iDf$correction <- FALSE
  
  ciDf <- data.frame(value=gCDisv)
  ciDf$method <- paste("F",i, sep="")
  ciDf$correction <- TRUE
  df.m2 <- rbind(df.m2,ciDf, iDf)
}

ggplot(data = df.m2, aes(x=method, y=value)) + geom_boxplot(aes(fill=correction)) + ylim(0, 1)


# Boston
df.m3 <- data.frame()
for(i in 1:10){
  s <- sample(nrow(BostonHousing),400)
  reg.lm <- snn(medv~.,BostonHousing,subset=s, regularization=FALSE)
  iDf <- data.frame(value=gDisv)
  iDf$method <- paste("F",i, sep="")
  iDf$correction <- FALSE
  
  ciDf <- data.frame(value=gCDisv)
  ciDf$method <- paste("F",i, sep="")
  ciDf$correction <- TRUE
  df.m3 <- rbind(df.m3,ciDf, iDf)
}

ggplot(data = df.m3, aes(x=method, y=value)) + geom_boxplot(aes(fill=correction)) + ylim(0, 1)

