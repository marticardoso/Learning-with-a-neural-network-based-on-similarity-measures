## This file contains all functions related to the Ensemble of SNNs 
## (create a model and predict new data)

source('SNN.R')
source('MoE.R')
source('test/benchmarkutils.R')

# Function that creates en Ensemble of SNNs
snn.bagging <- function(formula, data, subset = NULL,
                        nSNN = 100, # Number of SNNs
                        simil.types = list(),
                        regularization = FALSE, # Regularize the ensemble learner
                        snn.reg = FALSE, # Regularize SNNs
                        runDaisyOnce = FALSE, # Execute daisy once at the beginning with all data
                        useGlobalDaisyTransformations = TRUE, # Use daisy transofrmations global (EnsSNN level) or local (SNN level)
                        snnbag.expectedRows = 0.5, # Expected number of observations to train each SNN
                        snnbag.rowMethod = 'P', # Distribution to set the number of observations to train each SNN
                        bagging.method = 'B', # Ensemble method
                        clust.control = NULL,
                        ..., trace = TRUE) {
  if (!is.null(subset) && length(subset) < nrow(data)) data.train <- data[subset,]
  else data.train <- data

  
  mf <- model.frame(formula, data.train, na.action = NULL)
  data.train.inputs <- mf[, -1]
  y <- model.response(mf)
  if (runDaisyOnce) {
    if(trace) cat('Daisy is computed only once, at the begging!\n')
    daisyObject <- daisy2(data.train.inputs, metric = "gower", type = simil.types)
  }
  else if (useGlobalDaisyTransformations) {
    if (trace) cat('Daisy will be computed in each SNN! (but sx and min computed at bagging)\n')
    daisyObject <- daisy2_noComputation(data.train.inputs, metric = "gower", type = simil.types)
  }
  else {
    if(trace) cat('Daisy will be computed in each SNN!\n')
    daisyObject <- NULL
  }

  if (trace) cat('Computing Models \n')

  # Number of observations to train each SNN
  if (snnbag.rowMethod == "U" || snnbag.rowMethod == "Uniform") {
    howManyRows <- runif(nSNN, 1, nrow(data.train) * snnbag.expectedRows*2)
  } else if (snnbag.rowMethod == "B" || snnbag.rowMethod == "Binomial") {
    howManyRows <- rbinom(nSNN, nrow(data.train), snnbag.expectedRows)
  } else if (snnbag.rowMethod == "P" || snnbag.rowMethod == "Poisson") {
    howManyRows <- rpois(nSNN, nrow(data.train) * snnbag.expectedRows)
  } else if (snnbag.rowMethod == "C" || snnbag.rowMethod == "Constant") {
    howManyRows <- rep(nrow(data.train) * snnbag.expectedRows, nSNN)    
  } else stop('Wrong paramater')
  howManyRows <- sapply(howManyRows, function(r) round(min(4000, nrow(data.train), max(50, r))))

  if (bagging.method == "E") { # Method C2 
    rows <- sample(1:nrow(data.train))[1:min(4000, nrow(data.train))] # At most 4000 observations
    daisyObject <- daisy2_noComputation(data.train.inputs, metric = "gower", type = simil.types)
    clust.data <- daisy2.newObservations(data.train.inputs[rows,], daisyObject)
    clustering <- pam(clust.data, k = nSNN, metric = "euclidean", stand = FALSE, diss = TRUE, keep.diss = FALSE, keep.data = FALSE)$clustering
  }

  # Train each SNN
  if (is.null(clust.control)) clust.control = list(clust.method = "R", nclust.method = "U") # By default use random prototype selection
  snn.sets <- lapply(1:nSNN, function(snnId) {
    if (bagging.method == "E") bag.Ids <- names(which(clustering == snnId))
    else {
      nrows <- howManyRows[snnId]
      bag.Ids <- sample(1:nrow(data.train))[1:nrows]
    }
    if (trace) cat('Model ', snnId, 'of', nSNN, '(', length(bag.Ids), ' observations) \n')

    snn(formula, data.train[bag.Ids,], daisyObj = daisyObject, regularization = snn.reg, simil.types = simil.types, ...,
        trace = FALSE, clust.control = clust.control)
  })

  # Fit ensemble learner
  fit2layer <- snn.bagging.fit.second.layer(data.train.inputs, y, snn.sets, daisyObject = daisyObject, regularization = regularization, bagging.method = bagging.method, ..., trace = trace)

  # Output class
  z <- list()
  class(z) <- c("snn.bagging")
  z$nSNN <- nSNN
  z$snn.sets <- snn.sets
  z$fit2layer <- fit2layer
  z$formula <- formula
  z$problemType <- z$fit2layer$problemType
  z$responseLevels <- levels(y)
  z$runDaisyOnce <- runDaisyOnce
  z$useGlobalDaisyTransformations <- useGlobalDaisyTransformations
  z$snn.reg <- snn.reg

  # Use test set
  if (!is.null(subset) && length(subset) < nrow(data)) {
    if (trace) cat('Predicting test data\n')
    test.y <- model.response(model.frame(formula, data = data[-subset,], na.action=NULL))
    if (is.logical(y) || is.factor(y)) {
      pred <- predict(z, newdata = data[-subset,], type = c("response", "prob"), trace = trace)
      if (is.list(pred)) {
        z$testResponse <- pred$response
        z$testProb <- pred$prob
      }
      else z$testResponse <- pred
      
      z$testReal <- test.y
      tab <- table(Truth = test.y, Pred = z$testResponse)
      z$testError <- 1 - sum(z$testResponse == test.y) / length(test.y)
      z$testAccuracy <- 100 * (1 - z$testError)
      z$testContingencyTable <- tab
    }
    else if (is.numeric(y)) {
      z$testResponse <- predict(z, newdata = data[-subset,], type = "response", trace = trace)
      z$testReal <- test.y
      z$mse <- sum((z$testResponse - test.y) ^ 2) / (length(test.y) - 1)
      z$nrmse <- sum((z$testResponse - test.y) ^ 2) / ((length(test.y) - 1) * var(test.y))
    }
    else stop('Output type not supported')
  }
  z
}

# Fit the Ensemble learner
snn.bagging.fit.second.layer <- function(data.train.input, y, snn.sets, daisyObject, bagging.method = "B", regularization=FALSE, ..., trace = TRUE) {

  z <- list()
  z$method <- bagging.method
  z$problemType <- getTypeOfProblem(y)
  z$regularization <- regularization

  if (trace) cat('Fitting second layer... ')
  
  if (bagging.method == "B" || bagging.method == "C" || bagging.method == "E") {
    # Compute the response of all SNNs
    snn.sets.pred <- lapply(1:length(snn.sets), function(i) predict(snn.sets[[i]], data.train.input, type = c("response", "prob", "simils"), daisyObj = daisyObject, trace=trace))

    bagging.ds <- data.frame(row.names = row.names(data.train.input))
    for (i in 1:length(snn.sets)) {
      snn.i.pred <- snn.sets.pred[[i]]
      if (z$problemType == "numeric") {
        bagging.ds <- cbind(bagging.ds, snn.i.pred$response)
      } else {
        probs <- snn.i.pred$prob
        if (nlevels(y) > 2) { # Fix the probabilities matrix
          if (!is.matrix(probs)) {
            probs.corrected <- cbind(probs)
            if (length(snn.sets[[i]]$outputLevels) == 2) probs.corrected <- cbind(1 - probs.corrected, probs.corrected) # Add absence case (binomial)
            colnames(probs.corrected) <- snn.sets[[i]]$outputLevels
            probs <- as.data.frame(probs.corrected)
          }
          if (ncol(probs) < nlevels(y)) { # Some columns (modalities) are missing
            probs <- as.data.frame(probs)
            for (l in levels(y)) {
              if (all(colnames(probs) != l)) probs[l] <- 0 # Add column
            }
            probs <- probs[, levels(y)] # Resort by level id
          }
        }
        bagging.ds <- cbind(bagging.ds, probs)
      }
    }
    colnames(bagging.ds) <- 1:ncol(bagging.ds)
  }

 
  if(trace) cat(c('Problem type: ', z$problemType, ' - Method', bagging.method,'\n'))
  if (bagging.method == "A" || bagging.method == 'A2') {
    # Nothing to do
  }
  else if (bagging.method == "B") { # Fit a GLM model
    bagging.ds$Target <- y
    if (!regularization && z$problemType == 'binomial') {
      if (trace) cat("[2nd layer] Fitting glm...\n")
      z$model <- glm(Target ~ ., data = bagging.ds, family = "binomial", ...)
    }
    else if (!regularization && z$problemType == "multinomial") {
      if (trace) cat("[2nd layer] Fitting mulitnomial...\n")
      z$model <- multinom(Target ~ ., data = bagging.ds, trace = FALSE, maxit = 500, abstol = 1e-6,MaxNWts = 1e10,...)
    }
    else if (regularization && (z$problemType == 'binomial' || z$problemType == 'multinomial')) {
      if (trace) cat("[2nd layer] Fitting glmnet...\n")
      bagging.ds <- fixDatasetForGlmnetCV(bagging.ds) # GLMNET bug => duplicate row when less than 8 observations      
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
  else if (bagging.method == "C" || bagging.method == "E") { # Mixture of Experts
    # For Mixture of Expert cases, it created a dataframe with the similarities
    simils.ds <- data.frame(row.names = row.names(data.train.input))
    for (i in 1:length(snn.sets.pred))  simils.ds <- cbind(simils.ds, snn.sets.pred[[i]]$simils)
    colnames(simils.ds) <- 1:ncol(simils.ds)
    predByM <- list() # List with the mapping of SNNs and its prototypes
    counter <- 1
    for (i in 1:length(snn.sets.pred)) {
      nC <- ncol(snn.sets.pred[[i]]$simils)
      predByM[[i]] <- counter:(counter + nC - 1)
      counter <- counter + nC
    }
    # Creates the MoE model
    if (z$problemType == 'numeric') {
      z$model <- MoE.optimize(simils.ds, bagging.ds, y, 'numeric', predByM = predByM)
    }
    else if (z$problemType == 'binomial') {
      z$model <- MoE.optimize(simils.ds, bagging.ds, y, 'binomial', predByM = predByM)
    }
    else if (z$problemType == 'multinomial') {
      z$model <- MoE.optimize(simils.ds, bagging.ds, y, 'multinomial', predByM = predByM)
    }
    else stop('Not yet implemented')
  }
  z
}

# # # # # # # #
# Prediction! #
# # # # # # # #
predict.snn.bagging = function(object, newdata, type = c("response", "prob"), trace = TRUE) {
  mf <- model.frame(object$formula, newdata, na.action = NULL)
  x <- mf[, -1]
  if (trace) print('Computing test responses')
  # Predict the SNNs
  nmodels <- length(object$snn.sets)
  snn.sets.pred <- lapply(1:nmodels, function(i) {
    if (trace) cat(c("Response", i))
    tmp <- predict(object$snn.sets[[i]], newdata, type = c("response", "prob","simils"), trace=trace)
    return(tmp)})

  if (trace) print('Join results (responses)')
  # Transform prediction into a dataframe
  bagging.ds <- data.frame(row.names = row.names(newdata))
  for (i in 1:length(snn.sets.pred)) {
    snn.i.pred <- snn.sets.pred[[i]]
    if (object$problemType == "numeric") {
      bagging.ds <- cbind(bagging.ds, snn.i.pred$response)
    } else {
      probs <- snn.i.pred$prob
      if (!is.null(object$responseLevels) && length(object$responseLevels) > 2) {
        if (!is.matrix(probs)) {
          probs.corrected <- cbind(probs)
          if (length(object$snn.sets[[i]]$outputLevels) == 2) probs.corrected <- cbind(1 - probs.corrected, probs.corrected)
          colnames(probs.corrected) <- object$snn.sets[[i]]$outputLevels
          probs <- as.data.frame(probs.corrected)
        }
        if (ncol(probs) < length(object$responseLevels)) {
          probs <- as.data.frame(probs)
          for (l in object$responseLevels) {
            if (all(colnames(probs) != l)) probs[l] <- 0
            }
          probs <- probs[, object$responseLevels] # Resort by level id
        }
      }
      bagging.ds <- cbind(bagging.ds, probs)
    }
  }
  colnames(bagging.ds) <- 1:ncol(bagging.ds)


  if (trace) print('Ensemble of SNNs methods')

  # Max vote and mean
  if (object$fit2layer$method == "A") {
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
        probs <- snn.sets.pred[[i]]$prob
        if (!is.null(object$responseLevels) && length(object$responseLevels) > 2) {
          if (!is.matrix(probs)) {
            probs.corrected <- cbind(probs)
            if (length(object$snn.sets[[i]]$outputLevels) == 2) {
              probs.corrected <- cbind(1 - probs.corrected, probs.corrected)
            }
            colnames(probs.corrected) <- object$snn.sets[[i]]$outputLevels
            probs <- as.data.frame(probs.corrected)
          }
          if (ncol(probs) < length(object$responseLevels)) {
            probs <- as.data.frame(probs)
            for (l in object$responseLevels) {
              if (all(colnames(probs) != l)) probs[l] <- 0
              }
            probs <- probs[, object$responseLevels] # Resort by level id
          }
        }
        snns.preds[, i] <- apply(probs, 1, function(p) object$responseLevels[which.max(p)[1]])
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
      probs <- lapply(snn.sets.pred, function(r) r$prob)
      preds3D <- array(as.matrix(bagging.ds), dim = c(nrow(newdata), length(object$responseLevels), length(snn.sets.pred)))
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
  else if (object$fit2layer$method == "B") { # GLM
    if (any(class(object$fit2layer$model) == "glmnet")) {  # Glmnet does not support data frame
      bagging.ds <- as.matrix(bagging.ds)
    }
    if (object$problemType == "binomial") {
      test.prob <- predict(object$fit2layer$model, bagging.ds, type = "response")
      response <- test.prob >= 0.5
      if (!is.null(object$responseLevels)) {
        responseFactor <- rep(object$responseLevels[1], nrow(newdata))
        responseFactor[response] <- object$responseLevels[2]
        response <- responseFactor
      }
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
    }
  }
  else if (object$fit2layer$method == "C" || object$fit2layer$method == "E") { # Mixture of Experts
    # Similarity dataframe
    simils.ds <- data.frame(row.names = row.names(newdata))
    for (i in 1:length(snn.sets.pred)) simils.ds <- cbind(simils.ds, snn.sets.pred[[i]]$simils)
    colnames(simils.ds) <- 1:ncol(simils.ds)
    # Predict MoE
    if (object$problemType == 'numeric') {
      response <- MoE.predict(object$fit2layer$model, simils.ds, bagging.ds)
    }
    else if (object$problemType == 'binomial') {
      test.prob <- MoE.predict(object$fit2layer$model, simils.ds, bagging.ds)
      response <- test.prob >= 0.5
      if (!is.null(object$responseLevels)) {
        response <- factor(response, levels = c(FALSE, TRUE))
        levels(response) <- object$responseLevels
      }
    }
    else if (object$problemType == 'multinomial') {
      test.prob <- MoE.predict(object$fit2layer$model, simils.ds, bagging.ds)
      response <- apply(test.prob, 1, function(p) object$responseLevels[which.max(p)[1]])
    }
    else stop('Not implemented')
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

# Function that gives the type of the response variable
getTypeOfProblem <- function(y) {
  if (is.logical(y) || (is.factor(y) && nlevels(y) == 2)) type <- 'binomial'
  else if (is.factor(y) && nlevels(y) > 2) type <- 'multinomial'
  else if (is.numeric(y)) type <- 'numeric'
  else stop(gettextf("Output type not supported"))
  type
}


# Function that prints the Ensemble of SNNs model
print.snn.bagging <- function(object, firstSNN = FALSE, ...) {
  cat('--- Bagging of SNNs ---\n')
  cat(c('A set of ', object$nSNN, ' SNN\n'), sep = '')
  cat(c(object$problemType, 'problem\n'))

  cat('formula: ')
  print(object$formula)

  cat('options were: \n')
  ind <- '    '
  cat(c(ind, '2n layer fit method: ', object$fit2layer$method, ifelse(object$fit2layer$regularization, '(Reg)', ''), '\n'))
  if (object$runDaisyOnce) cat(c(ind, 'Daisy only once, at begging level !\n'))
  else if (object$useGlobalDaisyTransformations) cat(c(ind, 'Daisy in each SNN (but rangs computed at bagging level)\n'))
  else cat(c(ind, 'Daisy in each SNN!\n'))

  cat(c(ind, 'SNN reg.:', ifelse(object$snn.reg, 'YES', 'NO'), '\n'))

  if (firstSNN) {
    cat('--- First SNN ---\n')
    print(object$snn.sets[[1]])
  }
}

# Function that shows the summary of the Ensemble of SNNs
summary.snn.bagging <- function(object,...) {
  ind <- '    '
  print(object,...)

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