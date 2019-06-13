library(cluster)

snn <- function (formula, data, subset, weights, na.action,
                method = "qr", x = FALSE, y = FALSE,
                contrasts = NULL, ...)
{
  ret.x <- x
  ret.y <- y
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  ## need stats:: for non-standard evaluation
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms") # allow model.frame to update it

  
  if (is.empty.model(mt)) {
    stop("Empty model not supported")
  }
  y <- model.response(mf)
  x <- model.matrix(mt, mf, contrasts)
  print(nrow(x))
  z <-  snn.fit(x, y, ...)
  
  # predict test data
  
  class(z) <- c("snn")
  z$na.action <- attr(mf, "na.action")
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  if (ret.x) z$x <- x
  if (ret.y) z$y <- y
  z$formula <- formula
  
  #Predict test data
  if(length(subset)<nrow(data)){
    
    pred <- predict (z, newdata=data[-subset,], type=c("response","prob"))
    
    pred.test <- pred$prediction
    test.y <- model.response(model.frame(formula,data=data[-subset,]))
    tab <- table(test.y,pred.test)
    error <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    z$testError <- error
    z$testAccuracy <- 100*(1- error)
    z$testProb <- pred$prob
    z$testPred <- pred.test
    z$testTab <- tab
  }
  z
}

r1 <- snn(Type~.,wine,subset=sample(nrow(wine),100))


snn.fit <- function (x, y, clust.method = "PAM", classical=FALSE, ...)
{
  if (is.null(n <- nrow(x))) stop("'x' must be a matrix")
  if(n == 0L) stop("0 (non-NA) cases")
  p <- ncol(x)
  if (p == 0L) stop("Null model")
  ny <- NCOL(y)
  ## treat one-col matrix as vector
  if(is.matrix(y) && ny == 1)
    y <- drop(y)
  if (NROW(y) != n)
    stop("incompatible dimensions")
  
  chkDots(...)
  
  if (classical) { 
    learn.data <- x
  } else { 
    x.daisy <- daisy(x, metric="gower", type = simil.types)
    x.simils <- data.frame(1-as.matrix(x.daisy))
    learn.data <- x.simils
  }
  
  clusters.idxs <- snn.findclusters(learn.data,method=clust.method,...)
  
  medoids <- x[clusters.idxs,]
  
  dataframe <- data.frame(x.simils[,clusters.idxs], Target=y)  
  
  if(class(y)=="factor"){
    model <- list()
    for(i in 1:nlevels(y)){ 
      dataframe$Target = (y==levels(y)[i])
      model[[i]] <- glm (Target ~ ., data=dataframe, family=binomial())
      model[[i]] <- step(model[[i]], trace=0)
    }
  }
  if(class(y)=="logical"){
    model <- glm (Target ~ ., data=dataframe, family=binomial())
    
    model <- step(model, trace=0)
  }
  
  z <- list()
  z$model <- model
  z$medoids <- medoids
  z$nMedoids <- length(clusters.idxs)
  z$classical <- classical
  z$sim <- x.simils
  z$outputType <- class(y)
  if(class(y)=="factor")
    z$outputLevels <- levels(y)
  z
}

r2 <- snn.fit(wine[,-1],wine$Type, subset=sample(nrow(wine),100))

snn.findclusters <- function(x,       #Dataset
                             M= NULL, #Number of clusters
                             method,  #Clustering method
                             clust.metric="euclidean", # (PAM)
                             clust.stand=FALSE,        # (PAM)
                             p = NULL,                 # (Binomial)
                             lambda = NULL,            # (Poison)
                             ...){
  n <- nrow(x)
  if(is.null(M)){
    M = max(0.1*n,1)
  }
  
  if(method=="PAM"){
    dataset.pam <- pam (x, k=M, metric = clust.metric, stand=clust.stand, keep.diss=FALSE, keep.data=FALSE)
    return(dataset.pam$id.med)  
  }
  else if(method=="Uniform")
    return(sample(1:n,M))
  else if(method=="Binomial"){
    if(is.null(p)) 
      p = M/n
    r <- rbinom(n, 1, p)
    return(which(r>0))
  }
  else if(method=="Poisson"){
    if(is.null(lambda))
      lambda = M/n
    r <- rpois(n, lambda)
    return(which(r>0))
  }
  else 
    stop(gettextf("Clustering method '%s' is not supported.", method))
}

print.lm <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  if(length(coef(x))) {
    cat("Coefficients:\n")
    print.default(format(coef(x), digits = digits),
                  print.gap = 2L, quote = FALSE)
  } else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}

summary.lm <- function (object, correlation = FALSE, symbolic.cor = FALSE, ...)
{
  z <- object
  p <- z$rank
  rdf <- z$df.residual
  if (p == 0) {
    r <- z$residuals
    n <- length(r)
    w <- z$weights
    if (is.null(w)) {
      rss <- sum(r^2)
    } else {
      rss <- sum(w * r^2)
      r <- sqrt(w) * r
    }
    resvar <- rss/rdf
    ans <- z[c("call", "terms", if(!is.null(z$weights)) "weights")]
    class(ans) <- "summary.lm"
    ans$aliased <- is.na(coef(object))  # used in print method
    ans$residuals <- r
    ans$df <- c(0L, n, length(ans$aliased))
    ans$coefficients <- matrix(NA_real_, 0L, 4L, dimnames =
                                 list(NULL, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
    ans$sigma <- sqrt(resvar)
    ans$r.squared <- ans$adj.r.squared <- 0
    ans$cov.unscaled <- matrix(NA_real_, 0L, 0L)
    if (correlation) ans$correlation <- ans$cov.unscaled
    return(ans)
  }
  if (is.null(z$terms))
    stop("invalid 'lm' object:  no 'terms' component")
  if(!inherits(object, "lm"))
    warning("calling summary.lm() ...")
  Qr <- qr.lm(object)
  n <- NROW(Qr$qr)
  if(is.na(z$df.residual) || n - p != z$df.residual)
    warning("residual degrees of freedom in object suggest this is not an \"lm\" fit")
  ## do not want missing values substituted here
  r <- z$residuals
  f <- z$fitted.values
  w <- z$weights
  if (is.null(w)) {
    mss <- if (attr(z$terms, "intercept"))
      sum((f - mean(f))^2) else sum(f^2)
    rss <- sum(r^2)
  } else {
    mss <- if (attr(z$terms, "intercept")) {
      m <- sum(w * f /sum(w))
      sum(w * (f - m)^2)
    } else sum(w * f^2)
    rss <- sum(w * r^2)
    r <- sqrt(w) * r
  }
  resvar <- rss/rdf
  ## see thread at https://stat.ethz.ch/pipermail/r-help/2014-March/367585.html
  if (is.finite(resvar) &&
      resvar < (mean(f)^2 + var(c(f))) * 1e-30)  # a few times .Machine$double.eps^2
    warning("essentially perfect fit: summary may be unreliable")
  p1 <- 1L:p
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  se <- sqrt(diag(R) * resvar)
  est <- z$coefficients[Qr$pivot[p1]]
  tval <- est/se
  ans <- z[c("call", "terms", if(!is.null(z$weights)) "weights")]
  ans$residuals <- r
  ans$coefficients <-
    cbind(Estimate = est, "Std. Error" = se, "t value" = tval,
          "Pr(>|t|)" = 2*pt(abs(tval), rdf, lower.tail = FALSE))
  ans$aliased <- is.na(z$coefficients)  # used in print method
  ans$sigma <- sqrt(resvar)
  ans$df <- c(p, rdf, NCOL(Qr$qr))
  if (p != attr(z$terms, "intercept")) {
    df.int <- if (attr(z$terms, "intercept")) 1L else 0L
    ans$r.squared <- mss/(mss + rss)
    ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n - df.int)/rdf)
    ans$fstatistic <- c(value = (mss/(p - df.int))/resvar,
                        numdf = p - df.int, dendf = rdf)
  } else ans$r.squared <- ans$adj.r.squared <- 0
  ans$cov.unscaled <- R
  dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1,1)]
  if (correlation) {
    ans$correlation <- (R * resvar)/outer(se, se)
    dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
    ans$symbolic.cor <- symbolic.cor
  }
  if(!is.null(z$na.action)) ans$na.action <- z$na.action
  class(ans) <- "summary.lm"
  ans
}

print.summary.lm <-
  function (x, digits = max(3L, getOption("digits") - 3L),
            symbolic.cor = x$symbolic.cor,
            signif.stars = getOption("show.signif.stars"),	...)
  {
    cat("\nCall:\n", # S has ' ' instead of '\n'
        paste(deparse(x$call), sep="\n", collapse = "\n"), "\n\n", sep = "")
    resid <- x$residuals
    df <- x$df
    rdf <- df[2L]
    cat(if(!is.null(x$weights) && diff(range(x$weights))) "Weighted ",
        "Residuals:\n", sep = "")
    if (rdf > 5L) {
      nam <- c("Min", "1Q", "Median", "3Q", "Max")
      rq <- if (length(dim(resid)) == 2L)
        structure(apply(t(resid), 1L, quantile),
                  dimnames = list(nam, dimnames(resid)[[2L]]))
      else  {
        zz <- zapsmall(quantile(resid), digits + 1L)
        structure(zz, names = nam)
      }
      print(rq, digits = digits, ...)
    }
    else if (rdf > 0L) {
      print(resid, digits = digits, ...)
    } else { # rdf == 0 : perfect fit!
      cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
      cat("\n")
    }
    if (length(x$aliased) == 0L) {
      cat("\nNo Coefficients\n")
    } else {
      if (nsingular <- df[3L] - df[1L])
        cat("\nCoefficients: (", nsingular,
            " not defined because of singularities)\n", sep = "")
      else cat("\nCoefficients:\n")
      coefs <- x$coefficients
      if(any(aliased <- x$aliased)) {
        cn <- names(aliased)
        coefs <- matrix(NA, length(aliased), 4, dimnames=list(cn, colnames(coefs)))
        coefs[!aliased, ] <- x$coefficients
      }
      
      printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
                   na.print = "NA", ...)
    }
    ##
    cat("\nResidual standard error:",
        format(signif(x$sigma, digits)), "on", rdf, "degrees of freedom")
    cat("\n")
    if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep = "")
    if (!is.null(x$fstatistic)) {
      cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
      cat(",\tAdjusted R-squared: ",formatC(x$adj.r.squared, digits = digits),
          "\nF-statistic:", formatC(x$fstatistic[1L], digits = digits),
          "on", x$fstatistic[2L], "and",
          x$fstatistic[3L], "DF,  p-value:",
          format.pval(pf(x$fstatistic[1L], x$fstatistic[2L],
                         x$fstatistic[3L], lower.tail = FALSE),
                      digits = digits))
      cat("\n")
    }
    correl <- x$correlation
    if (!is.null(correl)) {
      p <- NCOL(correl)
      if (p > 1L) {
        cat("\nCorrelation of Coefficients:\n")
        if(is.logical(symbolic.cor) && symbolic.cor) {# NULL < 1.7.0 objects
          print(symnum(correl, abbr.colnames = NULL))
        } else {
          correl <- format(round(correl, 2), nsmall = 2, digits = digits)
          correl[!lower.tri(correl)] <- ""
          print(correl[-1, -p, drop=FALSE], quote = FALSE)
        }
      }
    }
    cat("\n")#- not in S
    invisible(x)
  }


library(StatMatch)
predict.snn = function(object, newdata,type=c("response","prob")){
  
 
  x <- model.matrix(object$formula,newdata)
  dataset.daisy = gower.dist(x, data.y=object$medoids)
  dataset.simils <- data.frame(1-as.matrix(dataset.daisy))
  #Transform to data frame
  data.sim <- data.frame(dataset.simils)  
  colnames(data.sim) = paste('X', row.names(object$medoids), sep="")
  
  #Predict by type
  if(object$outputType == "logical"){
    test.prob <- predict (object$model, newdata=data.sim, type="response")
    test.pred=NULL
    test.pred[test.prob<0.5]=FALSE
    test.pred[test.prob>=0.5]=TRUE
  }
  if(object$outputType == "factor"){
    nLevels <- length(object$outputLevels)
    prob = matrix(0,3,nrow(data.sim))
    for(i in 1:nLevels){ 
      prob[i,] <-  predict (object$model[[i]], newdata=data.sim, type="response")
    }
    test.pred <- apply(prob,2,function(p) object$outputLevels[which.max(p)[1]])
    test.prob <- apply(prob,2,function(p) max(p))
  }
  z <-list()
  
  if("response" %in% type)
    z$prediction <- test.pred
  
  if("prob" %in% type)
    z$prob <- test.prob
  
  if(length(z)==1)
    return(z[[1]])
  
  z
}

r = predict.snn(r1,wine,c("response","prob"))
r$prediction


r1 <- snn(Type~.,wine,subset=sample(nrow(wine),100))

pred <- predict (r8, newdata=wine[1:10,], type="response")
