library(cluster)
library(glmnet)
library(nnet)
library(MASS)

source("fp_utils.R")
source("daisy/daisy2.R")
source("daisy/daisy2Predict.R")
source("daisy/daisy2_noComputation.R")
debug = TRUE

snn <- function(formula, data, subset = NULL, x = FALSE, y = FALSE, ..., trace = TRUE) {
  ret.x <- x
  ret.y <- y
  mf <- model.frame(formula = formula, data = data, na.action = NULL, drop.unused.levels = TRUE)
  if (!is.null(subset)) mf <- mf[subset,]

  if (is.empty.model(mf)) {
    stop("Empty model not supported")
  }
  y <- model.response(mf)
  x <- data.frame(mf[, -1])

  z <- snn.fit(x, y, ..., trace = trace)


  class(z) <- c("snn")
  z$call <- match.call()
  z$mf <- mf
  if (ret.x) z$x <- x
  if (ret.y) z$y <- y
  z$formula <- formula

  #Predict test data
  if (!is.null(subset) && length(subset) < nrow(data)) {
    if (trace) cat('Predicting test data\n')
    mf.test <- model.frame(formula, data = data[-subset,], na.action = NULL, drop.unused.levels = TRUE)
    test.y <- model.response(mf.test)

    if (is.logical(y) || is.factor(y)) {
      pred <- predict(z, newdata = data[-subset,], type = c("response", "prob"),trace =trace)
      z$testResponse <- pred$response
      z$testProb <- pred$prob
      z$testReal <- test.y
      tab <- table(Truth = test.y, Pred = pred$response)
      z$testError <- 1 - sum(diag(tab)) / sum(tab)
      z$testAccuracy <- 100 * (1 - z$testError)
      z$testContingencyTable <- tab
    }
    else if (is.numeric(y)) {
      z$testResponse <- predict(z, newdata = data[-subset,], type = "response", trace = trace)
      z$testReal <- test.y
      z$mse <- mean((z$testResponse - test.y) ^ 2)
      z$nrmse <- sum((z$testResponse - test.y) ^ 2) / ((length(test.y) - 1) * var(test.y))
    }
    else stop('Output type not supported')
    }
  z
}


snn.fit <- function(x, y, daisyObj = NULL,
                    regularization = FALSE,
                    standardizeSimils = TRUE,
                    simil.types = list(), clust.control = NULL, p = 0.1, p.control = NULL, ..., trace = TRUE) {
  if (is.null(n <- nrow(x))) stop("'x' must be a dataframe")
  if (ncol(x) == 0L) stop("Null model")
  ny <- NCOL(y)

  shouldComputeDaisyBeforeClustering <- snn.findclusters.needsDaisy(clust.control) || (!is.null(p.control) && p.control$method == 'G')
  if (shouldComputeDaisyBeforeClustering) {
    if (is.null(daisyObj)) {
        if (trace) cat('[Computing daisy dissimilarity]\n')
        daisyObj <- x.daisy <- daisy2(x, metric = "gower", type = simil.types)
    }
    else if (is.dissimilarity(daisyObj)) {
      if (trace) cat('[dissimilarity passed as parameter]\n')
      x.daisy <- as.matrix(daisyObj)[rownames(x), rownames(x)]
    }
    else {
      if (trace) cat('[Computing daisy dissimilarity (using global sx)]\n')
      x.daisy <- daisy2.newObservations(x, daisyObj)
    }
    clust.data <- x.daisy
  }
  else clust.data <- NULL

  findclusters.res <- snn.findclusters(nrow(x), clust.data, control = clust.control, ..., trace = trace)
  id.medoid <- findclusters.res$id.med

  prototypes <- x[id.medoid,]
  # Daisy only with prototypes
  if (!shouldComputeDaisyBeforeClustering) {
    if (is.null(daisyObj)) daisyObj <- daisy2_noComputation(x, metric = "gower", type = simil.types)

    if (is.dissimilarity(daisyObj)) {
      if (trace) cat('[Dissimilarity passed as parameter]')
      x.daisy <- as.matrix(daisyObj)[rownames(x), rownames(prototypes)]
    }
    else {
      if (trace) cat('[Computing daisy dissimilarity (using global sx and only prototypes)]')
      x.daisy <- daisy2.newObservations(prototypes, daisyObj, newdata = x)
      x.daisy <- as.matrix(x.daisy)
    }
    x.simils <- 1 - as.matrix(x.daisy)
  }
  else {
    x.simils <- 1 - as.matrix(x.daisy)[, id.medoid]
  }

  if (!is.null(p.control)) {
    if (trace) cat('[Optimization of p] Method: ', p.control$method, '\n')
    if (p.control$method == 'Opt') {
      optRes <- optimize_p_oneOpt(x.simils, y, pInitial = p)
      p <- optRes$newP
    }
    else if (p.control$method == 'CV') {
      optRes <- optimize_p_kFoldCV(x.simils, y, control = p.control, ..., trace = trace)
      p <- optRes$bestP
    }
    else if (p.control$method == 'GCV') {
      optRes <- optimize_p_GCV(x.simils, y, control = p.control, ..., trace = trace)
      p <- optRes$bestP
    }
    else if (p.control$method == 'G') {
      stop('ToDo: x.simils is not complete')
      optRes <- optimize_p_method3(as.matrix(x.daisy), findclusters.res, type = p.control$type)
      if (is.numeric(optRes)) p <- optRes
      else p <- optRes$avg
      }
    else if(!is.null(p.control$method)) stop('Invalid optimization of p method')
  }
  if (trace) cat('Using p=', p, '\n')
  learn.data <- data.frame(apply.fp(x.simils, p))

  if (standardizeSimils) {
    dataScaled <- scale(learn.data)
    scaled_center <- attr(dataScaled, "scaled:center")
    scaled_scale <- attr(dataScaled, "scaled:scale")
    learn.data <- data.frame(dataScaled)
  }
  learn.data$Target <- y

  if (is.factor(y) || is.logical(y))
    model <- snn.createClassificationModel(learn.data, regularization = regularization, ..., trace = trace)
  else if (is.numeric(y))
    model <- snn.createRegressionModel(learn.data, regularization = regularization, ..., trace = trace)
  else stop("Output type not supported")

  z <- list() # Output

  #Problem fields
  z$outputType <- class(y)
  if (class(y) == "factor") z$outputLevels <- levels(y)

  #Daisy object
  if (is.dissimilarity(daisyObj)) z$daisyObj <- attr(daisyObj, "daisyObj")
  else z$daisyObj <- daisyObj
  z$prototypes <- prototypes
  z$clust.method <- findclusters.res$clust.method
  z$nclust.method <- findclusters.res$nclust.method
  z$hp <- findclusters.res$hp
  # P field
  z$p <- p
  z$pmethod <- p.control$method

  # Standarization fields
  z$standardizeSimils <- standardizeSimils
  if (standardizeSimils) z$scaled <- list(center = scaled_center, scale = scaled_scale)

  #Model fields
  z$model <- model
  z$regularization <- regularization

  #Debug info
  if (debug) {
    z$clust.data <- clust.data
    z$learn.data <- learn.data
    z$dissim <- x.daisy
    z$dissim.matrix <- as.matrix(x.daisy)
    z$simil.matrix <- x.simils
    z$findclusters.res <- findclusters.res
  }
  z
}

snn.createClassificationModel <- function(dataframe, regularization = FALSE, lambdas = NULL, ..., trace = TRUE) {
  y <- model.response(model.frame(Target ~ ., dataframe))
  if (is.logical(y) || (is.factor(y) && nlevels(y) == 2))
    family.type <- "binomial"
  else if (is.factor(y) && nlevels(y) > 2)
    family.type <- "multinomial"
  else if (is.factor(y) && nlevels(y) == 1) {
    return(NULL) # Empty model, return always the only level
  }
  else stop(gettextf("Classification output '%s' is not supported.", class(y)))

  if (!regularization && family.type == "binomial") {
    if (trace) cat("[Classification] Creating glm model...\n")
    model <- glm(Target ~ ., data = dataframe, family = "binomial", control = list(maxit = 100), ..., trace = trace)
  }
  else if (!regularization && family.type == "multinomial") {
    if (trace) cat("[Classification] Creating multinom model...\n")
    model <- multinom(Target ~ ., data = dataframe, trace = FALSE, maxit = 500, abstol = 1e-6, MaxNWts = 10000, ...)
  }
  else {
    if (trace) cat("[Classification] Creating ridge model...\n")
    x <- as.matrix(dataframe[, - which(names(dataframe) %in% c("Target"))])
    y <- dataframe$Target
    if (is.null(lambdas)) lambdas <- 2 ^ seq(-10, 10, 0.25)
    else lambdas <- c(lambdas)
    if (length(lambdas) > 1) {      
      cv.result <- cv.glmnet(x, y, nfolds = 10, family = family.type, alpha = 0, standardize = FALSE)
      best.lambda <- cv.result$lambda.min[1]
    }
    else {
      best.lambda <- lambdas[1]
    }
    model <- glmnet(x, y, family = family.type, lambda = best.lambda, alpha = 0, standardize = FALSE)
  }
  model
}

snn.createRegressionModel <- function(dataframe, regularization = FALSE, lambdas = NULL, ..., trace = TRUE) {
  if (!regularization) {

    if (trace) cat("[Regression] Creating lm model...\n")
    model <- lm(Target ~ ., data = dataframe)
    if (is.na(sum(model$coefficients))) {
      model$coefficients[is.na(model$coefficients)] <- 0
    }
  }
  else {
    if (trace) cat("[Regression] Creating ridge regression model...\n")

    if (is.null(lambdas)) lambdas <- 2 ^ seq(-10, 10, 0.25)
    else lambdas <- c(lambdas)

    if (length(lambdas) > 1) {
      r <- lm.ridge(Target ~ ., dataframe, lambda = lambdas)
      lambda.id <- which.min(r$GCV)
      best.lambda <- lambdas[lambda.id]
    }
    else {
      best.lambda <- lambdas[1]
    }
    model <- lm.ridge(Target ~ ., dataframe, lambda = best.lambda)
  }
  model
}

snn.findclusters.needsDaisy <- function(control = NULL) {
  clust.method <- 'PAM'
  if (!is.null(control) && !is.null(control$clust.method)) clust.method <- control$clust.method
  return(clust.method == 'PAM')
}
#Function to find the clusters
# - control
#   - clust.method: method used to find clusters (PAM or Random)
#   - clust.metric: when PAM, metric used to find the clusters (default: Euclidean)
#   - clust.stand: when PAM, wheter the data is standarized or not (default false)
#   - nclust.method: method to find the number of clusters
#   - hp: hyper-parameter used to find the number of clusters (Estimation of the proportion of clusters)
snn.findclusters <- function(N, clust.data, #Dataset
                             control = NULL,
                             ..., trace = TRUE) {
  # Load info from control
  clust.method <- 'PAM' # Clustering method
  clust.metric <- "euclidean"
  clust.stand <- FALSE
  nclust.method <- 'U'
  hp <- 0.1
  if (!is.null(control)) {
    if (!is.null(control$clust.method)) clust.method <- control$clust.method
    if (!is.null(control$clust.metric)) clust.metric <- control$clust.metric
    if (!is.null(control$clust.stand)) clust.stand <- control$clust.stand
    if (!is.null(control$hp)) hp <- control$hp
    if (!is.null(control$nclust.method)) nclust.method <- control$nclust.method
    }

  M <- snn.numberOfClusters(N, hp = hp, nclust.method = nclust.method, trace = trace)

  if (clust.method == "PAM") {
    if (trace) cat("[Clustering] PAM...\n")
    z <- pam(clust.data, k = M, metric = clust.metric, stand = clust.stand, diss = TRUE, keep.diss = FALSE, keep.data = FALSE)
  }
  else if (clust.method == "R" || clust.method == "Random") {
    if (trace) cat("[Clustering] Random\n")
    z <- list(id.med = sample(1:N, M))
  }
  else
    stop(gettextf("Clustering method '%s' is not supported. Supported methods: PAM and Random.", clust.method))


  z$clust.method <- clust.method
  z$nclust.method <- nclust.method
  z$hp <- hp
  return(z)
}

# Method to find the number of clusters
# hp <- Estimation of the proportion of clusters.
# nclust.method <- method to decide the number of clusters
snn.numberOfClusters <- function(N, hp = 0.1, nclust.method = 'C', trace = TRUE) {

  if (nclust.method == "U" || nclust.method == "Uniform") {
    if (trace) cat("[Num of clusters method]: Uniform")
    M <- round(runif(1, 1, ceiling(N * hp)))
  }
  else if (nclust.method == "B" || nclust.method == "Binomial") {
    if (trace) cat("[Num of clusters method]: Binomial")
    M <- rbinom(1, N, hp)
  }
  else if (nclust.method == "P" || nclust.method == "Poisson") {
    if (trace) cat("[Num of clusters method]: Poisson")
    M <- min(rpois(1, N * hp), N)
  }
  else if (nclust.method == "C" || nclust.method == "Constant") {
    if (trace) cat("[Num of clusters method]: Constant")
    M <- floor(hp * N)
  }
  else
    stop(gettextf("Number of clusters method '%s' is not supported. Methods supported: Uniform (U), Binomial (B), Poisson (P) and Constant(C).", nclust.method))

  M <- min(max(M, 2), 4000)
  if (trace) cat(" - ", M, "\n")
  M
}

print.snn <- function(object, digits = max(3L, getOption("digits") - 3L), ...) {
  cat(c('A ', ncol(object$prototypes), '-', nrow(object$prototypes), '-'), sep = '')
  if (object$outputType == "factor" && length(object$outputLevels) > 2) cat(length(object$outputLevels))
  else cat('1')
  cat(' snn network\n')

  if (NCOL(object$prototypes) < 15) cat(c('Inputs: ', colnames(object$prototypes), '\n'))
  cat('formula: ')
  print(object$formula)

  cat(c('prototypes:', row.names(object$prototypes), '\n'))


  cat('options were: \n')
  ind <- '    '
  cat(ind, '#clusters: ')
  if (object$nclust.method == "U" || object$nclust.method == "Uniform") cat("Uniform")
  else if (object$nclust.method == "B" || object$nclust.method == "Binomial") cat("Binomial")
  else if (object$nclust.method == "P" || object$nclust.method == "Poisson") cat("Poisson")
  else if (object$nclust.method == "C" || object$nclust.method == "Constant") cat("Constant")
  cat(c(' (hp: ', object$hp, ', # ',nrow(object$prototypes), ')\n'), sep = '')
  cat(c(ind, 'clustering:', ifelse(object$clust.method == 'PAM', 'PAM', 'Random'), '\n'))
  cat(c(ind, 'p:', object$p))
  if (!is.null(object$pmethod)) cat(c(' (method: ', object$pmethod, ')'), sep = '')
  cat('\n')
  cat(c(ind, 'standarization:', ifelse(object$standardizeSimils, 'YES', 'NO'), '\n'))
  cat(c(ind, 'model:', class(object$model), '\n'))
  cat(c(ind, 'regularization:', ifelse(object$regularization, 'YES', 'NO'), '\n'))
}

summary.snn <- function(object) {
  ind <- '    '
  cat('--- SNN Summary ---\n')
  print(object)

  if (!is.null(object$testResponse)) {
    cat('\n--- TEST RESULTS ---\n')
    if (!is.null(object$testAccuracy)) {
      cat(c(ind, 'Accuracy:', round(object$testAccuracy, 3), '\n'))
      cat(c(ind, 'Contingency table:\n'))
      print(object$testContingencyTable)
      
    }
    else {
      cat(c(ind, 'NRMSE: ', object$nrmse, '\n'))
      cat(c(ind, 'MSE: ', object$mse, '\n'))
    }
  }
}

predict.snn = function(object, newdata, type = c("response", "prob", "simils"), daisyObj = NULL, ..., trace = TRUE) {
  if(trace) print('Predicting SNN')
  if (ncol(object$prototypes)+1 == ncol(newdata)) {
    mf <- model.frame(object$formula, newdata, na.action = NULL)
    x <- mf[, -1]
  }
  else if (ncol(object$prototypes) == ncol(newdata)) {
    x <- newdata
  }
  else stop('Invalid new data')
  #print('Computing daisy')
  if (is.null(daisyObj) || !is.dissimilarity(daisyObj)) {
    x.daisy <- daisy2.newObservations(object$prototypes, object$daisyObj, newdata = x)
    x.daisy <- as.matrix(x.daisy)
  }
  else {
    x.daisy <- as.matrix(daisyObj)[rownames(x), rownames(object$prototypes)]
  }
  
  x.simils <- 1 - x.daisy

  test.x <- data.frame(apply.fp(x.simils, object$p))
  colnames(test.x) <- paste('X', row.names(object$prototypes), sep = "")

  if (object$standardizeSimils) {
    dataScaled <- scale(test.x, center = object$scaled$center, scale = object$scaled$scale)
    test.x <- data.frame(dataScaled)
  }

  if (any(class(object$model) == "glmnet")) # Glmnet does not support data frame
    test.x <- as.matrix(test.x)

  #Predict by type
  if (object$outputType == "logical") {
    test.prob <- predict(object$model, test.x, type = "response")
    response <- test.prob >= 0.5
  }
  else if (object$outputType == "factor" && length(object$outputLevels) == 1) {
    response <- rep(object$outputLevels, nrow(test.x))
    test.prob <- rep(1, nrow(test.x))
    names(response) <- row.names(test.x)
    names(test.prob) <- row.names(test.x)
  }
  else if (object$outputType == "factor") {
    if (any(class(object$model) == "multinom"))
      prob <- predict(object$model, test.x, type = "probs")
    else
      prob <- predict(object$model, test.x, type = "response")
    if (length(object$outputLevels) == 2) {
      response <- rep(object$outputLevels[1], nrow(x))
      response[prob >= 0.5] <- object$outputLevels[2]
      test.prob <- prob
    }
    else {
      response <- apply(prob, 1, function(p) object$outputLevels[which.max(p)[1]])
      test.prob <- prob
    }

  }
  else if (object$outputType == "numeric" || object$outputType == "integer") {
    response <- cbind(1, as.matrix(test.x)) %*% coef(object$model)
    if (is.matrix(response)) response <- response[, 1]
    }
  else
    stop("[Predicting] Output type not supported")

  z <- list()

  if ("response" %in% type || object$outputType == "numeric")
    z$response <- response

  if ("prob" %in% type && (object$outputType == "logical" || object$outputType == "factor"))
    z$prob <- test.prob

  if ("simils" %in% type) {
    z$simils <- x.simils
  }
  if (length(z) == 1)
    return(z[[1]])

  z
}

