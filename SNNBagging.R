source('SNN.R')
source('benchmarkutils.R')

# Trace level


snn.bagging <- function(formula, data, subset = NULL, nSNN = 10,
  simil.types = list(), regularization = FALSE, snn.reg = FALSE,
  runDaisyOnce = TRUE, ..., trace = TRUE) {
  if (!is.null(subset) && length(subset) < nrow(data)) data.train <- data[subset,]
  else data.train <- data

  if(trace) cat('Computing dissimilarities \n') # Compute similarities before in order to speed up
  myTic()
  data.train.inputs <- model.frame(formula, data.train)[, -1]
  y <- model.response(model.frame(formula, data.train))
  if (runDaisyOnce) {
    if(trace) cat('Daisy is computed only once, at the begging!\n')
    daisyObject <- daisy2(data.train.inputs, metric = "gower", type = simil.types)
  }
  else {
    if(trace) cat('Daisy will be computed in each SNN!\n')
    daisyObject <- NULL
  }
  myToc(label = 'Daisy')

  if (trace) cat('Computing Models \n')
  howManyRows <- function() max(50, ceiling(nrow(data.train) * runif(1) / 2))

  snn.sets <- lapply(1:nSNN, function(i) {
    nrows <- howManyRows()
    if(trace) cat('Model ', i, 'of', nSNN, '(', nrows, ' observations) \n')

    bag.Ids <- sample(1:nrow(data.train))[1:nrows]
    snn(formula, data.train[bag.Ids,], daisyObj = daisyObject, regularization = snn.reg, simil.types = simil.types, ...,
        trace = FALSE, clust.control = list(clust.method = "R", nclust.method = "U"))
  })

  

  fit2layer <- snn.bagging.fit.second.layer(data.train, y, snn.sets, daisyObject = daisyObject, regularization = regularization, ..., trace = trace)

  z <- list()
  class(z) <- c("snn.bagging")
  z$nSNN <- nSNN
  z$snn.sets <- snn.sets
  z$fit2layer <- fit2layer
  z$formula <- formula
  z$problemType <- z$fit2layer$problemType
  z$responseLevels <- levels(y)

  # Use test set
  if (!is.null(subset) && length(subset) < nrow(data)) {
    if (trace) cat('Predicting test data\n')

    test.y <- model.response(model.frame(formula, data = data[-subset,]))

    if (is.logical(y) || is.factor(y)) {
      pred <- predict(z, newdata = data[-subset,], type = c("response", "prob"))
      if (is.list(pred)) {
        z$testResponse <- pred$response
        z$testProb <- pred$prob
      }
      else {
        z$testResponse <- pred
      }
      tab <- table(Truth = test.y, Pred = z$testResponse)
      z$testError <- 1 - sum(diag(tab)) / sum(tab)
      z$testAccuracy <- 100 * (1 - z$testError)
      z$testContingencyTable <- tab
    }
    else if (is.numeric(y)) {
      z$testResponse <- predict(z, newdata = data[-subset,], type = "response")
      z$testReal <- test.y
      z$mse <- sum((z$testResponse - test.y) ^ 2) / (length(test.y) - 1)
      z$nrmse <- sum((z$testResponse - test.y) ^ 2) / ((length(test.y) - 1) * var(test.y))
    }
    else stop('Output type not supported')
    }

  z
}

snn.bagging.fit.second.layer <- function(data.train, y, snn.sets, daisyObject, bagging.method = 'B', regularization=FALSE, ..., trace = TRUE) {

  if (trace) cat('Fitting second layer... ')

  if (trace) cat('Computing output for all models (Bagging) \n')
  
  if (bagging.method == 'B') {

    myTic()
    snn.sets.pred <- lapply(1:length(snn.sets), function(i) predict(snn.sets[[i]], data.train, type = "prob", daisyObj = daisyObject))



    bagging.ds <- data.frame(row.names = row.names(data.train))
    for (snn.i.pred in snn.sets.pred)
      bagging.ds <- cbind(bagging.ds, snn.i.pred)
    colnames(bagging.ds) <- 1:ncol(bagging.ds)
    bagging.ds$Target <- y
    myToc()
  }

  z <- list()
  z$method <- bagging.method
  z$problemType <- getTypeOfProblem(y)
  z$regularization <- regularization
  cat(c('Problem type: ', z$problemType, ' - Method', bagging.method,'\n'))
  if (bagging.method == 'A' || bagging.method == 'A2') {
    # Nothing to do
  }
  if (bagging.method == 'B') {
    if (!regularization && z$problemType == 'binomial') {
      if (trace) cat("[2nd layer] Fitting glm...\n")
      z$model <- glm(Target ~ ., data = bagging.ds, family = "binomial", ...)
    }
    else if (!regularization && z$problemType == "multinomial") {
      if (trace) cat("[2nd layer] Fitting mulitnomial...\n")
      z$model <- multinom(Target ~ ., data = bagging.ds, trace = FALSE, maxit = 500, abstol = 1e-6,...)
    }
    else if (regularization && (z$problemType == 'binomial' || z$problemType == 'multinomial')) {
      if (trace) cat("[2nd layer] Fitting glmnet...\n")
      x <- as.matrix(bagging.ds[, - which(names(bagging.ds) %in% c("Target"))])
      y <- bagging.ds$Target
      family.type <- ifelse(z$problemType == 'binomial', 'binomial', 'multinomial')
      cv.result <- cv.glmnet(x, y, nfolds = 10, family = family.type, alpha = 0, standardize = FALSE)
      z$model <- glmnet(x, y, family = family.type, lambda = cv.result$lambda.min[1], alpha = 0, standardize = FALSE)
    }
    else if (z$problemType == 'numeric') {
      if (!regularization) {
        if (trace) cat("[2nd layer] Fitting lm...\n")
        z$model <- lm(Target ~ ., data = bagging.ds)
      }
      else {
        if (trace) cat("[2nd layer] Fitting lm.ridge...\n")
        lambdas <- 2 ^ seq(-10, 10, 0.25)
        r <- lm.ridge(Target ~ ., data = bagging.ds, lambda = lambdas)
        lambda.id <- which.min(r$GCV)
        best.lambda <- lambdas[lambda.id]
        z$model <- lm.ridge(Target ~ ., bagging.ds, lambda = best.lambda)
      }

    }
  }
  z
}

# # # # # # # #
# Prediction! #
# # # # # # # #
predict.snn.bagging = function(object, newdata, type = c("response", "prob")) {
  mf <- model.frame(object$formula, newdata)
  x <- mf[, -1]

  nmodels <- length(object$snn.sets)
  snn.sets.pred <- lapply(1:nmodels, function(i) predict(object$snn.sets[[i]], newdata, type = "prob"))
  gsnn.sets.pred <<- snn.sets.pred
  #Transform to dataset
  bagging.ds <- data.frame(row.names = row.names(newdata))
  for (snn.i.pred in snn.sets.pred)
    bagging.ds <- cbind(bagging.ds, snn.i.pred)
  colnames(bagging.ds) <- 1:ncol(bagging.ds)
  


  # Max vote and mean
  if (object$fit2layer$method == 'A') {
    if (object$problemType == "binomial") {
      response <- rowMeans(bagging.ds >= 0.5) > 0.5
      if (!is.null(object$responseLevels)) {
        responseFactor <- rep(object$responseLevels[1], nrow(newdata))
        responseFactor[response] <- object$responseLevels[2]
        response <- responseFactor
      }
    }
    else if (object$problemType == "multinomial") {
      # Transform list to 3D array
      snns.preds <- matrix(0, nrow(newdata), length(snn.sets.pred))
      for (i in (1:length(snn.sets.pred))) {
        snns.preds[, i] <- apply(snn.sets.pred[[i]], 1, function(p) object$responseLevels[which.max(p)[1]])
      }
      response <- apply(snns.preds, 1, function(x) names(sort(table(x), decreasing = TRUE)[1]))
      response <- factor(response, levels = object$responseLevels)
    }
    else if (object$problemType == "numeric") {
      response <- rowMeans(bagging.ds)
    }
  }
  # Mean of probabilities
  else if (object$fit2layer$method == 'A2') {
    if (object$problemType == "binomial") {
      test.prob <- rowMeans(bagging.ds)
      response <- test.prob >= 0.5
      if (!is.null(object$responseLevels)) {
        responseFactor <- rep(object$responseLevels[1], nrow(newdata))
        responseFactor[response] <- object$responseLevels[2]
        response <- responseFactor
      }
    }
    else if (object$problemType == "multinomial") {
      # Transform list to 3D array
      preds3D <- array(unlist(snn.sets.pred), dim = c(NROW(snn.sets.pred[[1]]), NCOL(snn.sets.pred[[1]]), length(snn.sets.pred)))
      test.prob <- apply(preds3D, 2, rowMeans)

      if (length(object$responseLevels) == 2) {
        response <- rep(object$responseLevels[1], nrow(newdata))
        response[test.prob >= 0.5] <- object$responseLevels[2]
      }
      else
        response <- apply(test.prob, 1, function(p) object$responseLevels[which.max(p)[1]])
      }
    else if (object$problemType == "numeric") {
      stop('A2 only for classification')
    }

  }
  else if (object$fit2layer$method == 'B') {
    if (any(class(object$fit2layer$model) == "glmnet")) {  # Glmnet does not support data frame
      bagging.ds <- as.matrix(bagging.ds)
    }
    if (object$problemType == "binomial") {
      test.prob <- predict(object$fit2layer$model, bagging.ds, type = "response")
      response <- test.prob >= 0.5
    }
    else if (object$problemType == "multinomial") {
      if (any(class(object$fit2layer$model) == "multinom"))
        test.prob <- predict(object$fit2layer$model, bagging.ds, type = "probs")
      else
        test.prob <- predict(object$fit2layer$model, bagging.ds, type = "response")
      response <- apply(test.prob, 1, function(p) object$responseLevels[which.max(p)[1]])
    }
    else if (object$problemType == "numeric") {
      response <- cbind(1, as.matrix(bagging.ds)) %*% coef(object$fit2layer$model)
      if (is.matrix(response)) response <- response[, 1]      
      #response <- predict(object$fit2layer$model, bagging.ds)
    }
  }
  else if (object$fit2layer$method == 'C') {
    stop('ToDo')
  }

  z <- list()

  if ("response" %in% type)
    z$response <- response

  if (exists("test.prob") && ("prob" %in% type && (object$problemType == "binomial" || object$problemType == "multinomial")))
    z$prob <- test.prob

  if (length(z) == 1)
    return(z[[1]])

  z
}


getTypeOfProblem <- function(y) {
  if (is.logical(y) || (is.factor(y) && nlevels(y) == 2)) type <- 'binomial'
  else if (is.factor(y) && nlevels(y) > 2) type <- 'multinomial'
  else if (is.numeric(y)) type <- 'numeric'
  else stop(gettextf("Output type not supported"))
  type
}