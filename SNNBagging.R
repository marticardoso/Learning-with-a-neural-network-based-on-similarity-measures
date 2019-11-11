source('SNN.R')
library(tictoc)
snn.bagging <- function(formula, data, subset = NULL, nSNN = 10, simil.types = list(), ..., trace = TRUE) {
  if (!is.null(subset) && length(subset) < nrow(data)) data.train <- data[subset,]
  else data.train <- data

  cat('Computing dissimilarities \n') # Compute similarities before in order to speed up

  data.train.inputs <- model.frame(formula, data.train)[, -1]
  x.daisy <- daisy2(data.train.inputs, metric = "gower", type=simil.types)
  fDaisy <<- x.daisy

  cat('Computing Models \n')
  howManyRows <- function() max(20,ceiling(nrow(data.train) * runif(1) / 2))

  snn.sets <- lapply(1:nSNN, function(i) {
    nrows <- howManyRows()
    cat('Model ', i, 'of', nSNN, '(', nrows, ' observations) \n')
    
    bag.Ids <- sample(1:nrow(data.train))[1:nrows]
    snn(formula, data.train[bag.Ids,], daisyObj = x.daisy, ..., trace = FALSE, clust.control = list(clust.method = "R", nclust.method = "U"))
  })

  cat('Computing output for all models (Bagging) \n')
  tic()
  snn.sets.pred <- lapply(1:nSNN, function(i) predict(snn.sets[[i]], data.train, x.daisy = x.daisy, type = "prob"))
  toc()

  bagging.ds <- data.frame(row.names = row.names(data.train))
  for (snn.i.pred in snn.sets.pred)
    bagging.ds <- cbind(bagging.ds, snn.i.pred)
  colnames(bagging.ds) <- 1:ncol(bagging.ds)
  bagging.ds$Target <- model.response(model.frame(formula, data.train))

  cat('Creating lm model (Bagging) \n')
  y <- bagging.ds$Target
  if (is.logical(y) || (is.factor(y) && nlevels(y) == 2))
    model <- glm(Target ~ ., data = bagging.ds, family = "binomial", control = list(maxit = 100))
  else if (is.factor(y) && nlevels(y) > 2)
    model <- multinom(Target ~ ., data = bagging.ds, trace = FALSE, maxit = 500, abstol = 1e-6)
  else if (is.numeric(y))
    model <- lm(Target ~ ., data = bagging.ds)
  else
    stop(gettextf("Output type not supported"))

  z <- list()
  class(z) <- c("snn.bagging")
  z$snn.sets <- snn.sets
  z$bagging.model <- model
  z$formula <- formula
  z$outputType <- z$snn.sets[[1]]$outputType
  z$outputLevels <- levels(y)

  # Use test set
  if (!is.null(subset) && length(subset) < nrow(data)) {
    if (trace) cat('Predicting test data\n')

    test.y <- model.response(model.frame(formula, data = data[-subset,]))

    if (is.logical(y) || is.factor(y)) {
      pred <- predict(z, newdata = data[-subset,], type = c("response", "prob"))
      z$testResponse <- pred$response
      z$testProb <- pred$prob
      tab <- table(Truth = test.y, Pred = pred$response)
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

predict.snn.bagging = function(object, newdata, type = c("response", "prob")) {
  mf <- model.frame(object$formula, newdata)
  x <- mf[, -1]
  y <- model.response(mf)

  nmodels <- length(object$snn.sets)
  snn.sets.pred <- lapply(1:nmodels, function(i) predict(object$snn.sets[[i]], newdata, type = "prob"))
  #Transform to dataset
  bagging.ds <- data.frame(row.names = row.names(newdata))
  for (snn.i.pred in snn.sets.pred)
    bagging.ds <- cbind(bagging.ds, snn.i.pred)
  colnames(bagging.ds) <- 1:ncol(bagging.ds)
  bagging.ds$Target <- y


  if (object$outputType == "logical") {
    test.prob <- predict(object$bagging.model, bagging.ds, type = "response")
    response <- test.prob >= 0.5
  }
  else if (object$outputType == "factor") {
    if (any(class(object$bagging.model) == "multinom"))
      test.prob <- predict(object$bagging.model, bagging.ds, type = "probs")
    else
      test.prob <- predict(object$bagging.model, bagging.ds, type = "response")
    if (length(object$outputLevels) == 2) {
      response <- rep(object$outputLevels[1], length(y))
      response[test.prob >= 0.5] <- object$outputLevels[2]
    }
    else
      response <- apply(test.prob, 1, function(p) object$outputLevels[which.max(p)[1]])
    }
  else if (object$outputType == "numeric") {
    response <- predict(object$bagging.model, bagging.ds)
  }
  else
    stop("[Predicting] Output type not supported")

  z <- list()

  if ("response" %in% type)
    z$response <- response

  if ("prob" %in% type && (object$outputType == "logical" || object$outputType == "factor"))
    z$prob <- test.prob

  if (length(z) == 1)
    return(z[[1]])

  z
}
