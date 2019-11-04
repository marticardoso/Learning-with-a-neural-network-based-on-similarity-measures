
r1 <- snn(Type~.,wine,subset=sample(nrow(wine),100),method="multinom", x=TRUE, p=0.2, hp=0.02)
model <- r1$model
simils <- r1$simil.matrix
predSimils <- simils
simils

colnames(predSimils) = paste('X', row.names(simils), sep="")
predict(model,predSimils)
model2 <- model
class(model2) <- "nnet"
predict(model2,predSimils)
tmpPred <-predict(model2,predSimils)
predFirsts <- tmpPred[1:2,]

model3 <- model2
model3$softmax <- FALSE
predict(model3,predSimils)
predNnet <-predict(model3,predSimils)
predNnetFirsts <- predNnet[1:2,]



rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

tmpPred2 <-predict(model2,cbind(1,simils))
if(p<=0) return(NA)

w <- coef(model)  #Extract w
#if(any(class(model)=="glmnet")) w <- w[,1]
#w0 <- w[,"(Intercept)"]                         #Intercept
#w <- w[,-which(colnames(w) %in% c("(Intercept)"))] # Remove intercept
colnames(w) <- gsub('X','',colnames(w))              # Remove X from names

snn.res <- simils[1:5,colnames(w)[2:ncol(w)]] #apply(simils[,colnames(w)], c(1,2), function(x) fp(x,p))
snn.res <- cbind(1, snn.res)
snn.res <- snn.res %*% t(w)
snn.res <- cbind(0,snn.res)

#softmax
r <- t(apply(snn.res, 1, function(r) exp(r)/sum(exp(r))))

snn.res2 <- simils[1:5,colnames(w)]

function(model, simils, p){
  
  w <- coef(model)  #Extract w
  colnames(w) <- gsub('X','',colnames(w)) # Remove X from names
  prototypes <- colnames(w)[-which(colnames(w) %in% c("(Intercept)"))]
  
  snn.res <- apply(simils[,prototypes], c(1,2), function(x) fp(x,p))
  snn.res <- cbind(1, snn.res)
  snn.res <- snn.res %*% t(w)
  snn.res <- cbind(0,snn.res)
  
  r <- t(apply(snn.res, 1, function(r) exp(r)/sum(exp(r))))
}

# MODEL

similsX <- simils[1:5, colnames(w)]
colnames(similsX) = paste('X', colnames(similsX), sep="")
similsX <- cbind(1,similsX)

predict(model, similsX)
modelT <- model
class(modelT) <- "nnet"
predict(modelT, similsX)

modelT$softmax <- FALSE
predict(modelT, similsX)

#################### w0 do not sum properly

snn.res <- cbind(0,snn.res)
snn.res2 <- snn.res
for(i in 1:nrow(snn.res)){
  snn.res2[i,] <- exp(snn.res[i,])/sum(exp(snn.res[i,]))

}
apply(snn.res,1, function(l) )
Y <- class.ind(t)

res <-(sum((t - snn.res)^2)/length(t))
#cat('Pval: ', p, ' - optVal= ', res, '\n')


similsX <- simils
colnames(similsX) = paste('X', row.names(similsX), sep="")

newdata <- as.data.frame(similsX[1:3,colnames(w)])
rn <- row.names(newdata)
Terms <- delete.response(model$terms)
m <- model.frame(Terms, newdata,xlev = model$xlevels)


keep <- match(row.names(m), rn)
X <- model.matrix(Terms, m, contrasts = model$contrasts)
Y1 <- predict.nnet(model, X)
Y <- matrix(NA, nrow(newdata), ncol(Y1),
            dimnames = list(rn, colnames(Y1)))
Y[keep, ] <- Y1