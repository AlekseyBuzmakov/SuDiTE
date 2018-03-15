#' Evaluates several algos by the given train-test split
#'
#' Evaluates several algos by the given train-test split
#'
#' Takes a set of models and returns the quality of the selected groups by means of subgroupQualityFunc
#'
#' @param trainModelFuncs a vector of functions that build models. The prototype is function(Y,Trt,X), where Y is the response variable, Trt is the 0-1 treatment variable, and X is the covariate matrix.
#' @param predictByModelFuncs a vector of functions that predicts by models build by the corresponding furnction from trainModelFuncs. The prototype is function(m,X), where m is the model returned by the corresponding function, and X is the covariate matrix.
#' @param subgroupQualityFunc a function that evaluate the quality of a subgroup. The prototype is function(subgroup, Y, Trt), where subgroup is T-F vector defining a subgroup, and Y and Trt are similar as for the functions from trainModelFuncs
#' @param trainY a train response variable
#' @param trainTrt a train treatment 0-1 variable
#' @param trainX train covariates
#' @param testY a test response variable
#' @param testTrt a test treatment 0-1 variable
#' @param testX test covariates
#' @return a list with found subgroups in the test set, their sizes, qualities, and qualities of a random subset of similar size for all the algorithms
#' @export
#' @examples
#'
#' # Generating dataset
#' Trt = rbinom(1000,1,0.5)
#' X = data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(1000) ) > 0 )
#' # Evaluating algos
#' res = evaluateAlgos(
#'     c(trainModelRandomForest,trainModelModLM), # Two functions that trains the model
#'     c(predictByModelRandomForest,predictByModelModLM), # The corresponding prediction functions
#'     subgroupAverageTreatmentEffect, # The function that compute the quality of a subgroup
#'     Y[1:800], Trt[1:800], X[1:800,], # Train dataset
#'     Y[801:1000], Trt[801:1000], X[801:1000,] # Holdout dataset
#'     )
#' print(res$Qualities)
#'
evaluateAlgos = function(
  trainModelFuncs, predictByModelFuncs, subgroupQualityFunc,
  trainY, trainTrt, trainX, testY, testTrt, testX )
{
  stopifnot(length(trainModelFuncs) == length(predictByModelFuncs) )
  stopifnot(length(trainY) == length(trainTrt)
            && length(trainTrt) == nrow(trainX))
  stopifnot(length(testY) == length(testTrt)
            && length(testTrt) == nrow(testX))
  stopifnot(is.function(subgroupQualityFunc))

  # We need vectors of length at least 2
  trainModelFuncs = c(NULL, trainModelFuncs)
  predictByModelFuncs = c(NULL, predictByModelFuncs)
  subgroups = NULL
  sizes = NULL
  qualities = NULL
  qrnd = NULL
  for(i in 1:length(trainModelFuncs)) {
    print(paste0("    Training model ", i))
    m = trainModelFuncs[[i]](trainY, trainTrt, trainX)
    print(paste0("    Testing model ", i))
    res = predictByModelFuncs[[i]](m, testX)
    q = subgroupQualityFunc(res, testY, testTrt)
    subgroups = cbind(subgroups, res)
    sizes = c(sizes, sum(res))
    qualities = c(qualities, q)
    qrnd = c(qrnd,
             subgroupQualityFunc(rep( 1, nrow(testX)), testY, testTrt)
             * sum(res) / nrow(testX))
  }
  return(list(Subgroups = subgroups, Sizes = sizes, Qualities = qualities, QRnd = qrnd))
}


#' Evaluates the quality of different algos by cross-validation of their predictions
#'
#' Evaluates the quality of different algos by cross-validation of their predictions
#'
#' Takes a set of models and returns the quality of the selected groups by means of subgroupQualityFunc in Cross-Validation
#'
#' @param trainModelFuncs a vector of functions that build models. The prototype is function(Y,Trt,X), where Y is the response variable, Trt is the 0-1 treatment variable, and X is the covariate matrix.
#' @param predictByModelFuncs a vector of functions that predicts by models build by the corresponding furnction from trainModelFuncs. The prototype is function(m,X), where m is the model returned by the corresponding function, and X is the covariate matrix.
#' @param subgroupQualityFunc a function that evaluate the quality of a subgroup. The prototype is function(subgroup, Y, Trt), where subgroup is T-F vector defining a subgroup, and Y and Trt are similar as for the functions from trainModelFuncs
#' @param dbY a response variable
#' @param dbTrt a treatment 0-1 variable
#' @param dbX train covariates
#' @param numTrials a number of times a random division in train-test subdataset should be taken
#' @param splitFunc a function that splits the dataset into training and holdout sets
#' @param splitOpts  the options that are passed to trainHoldoutSplittingFunc
#'
#' @return a list with found subgroups in the test set of length testProportion*length(dbY)*numTrials, their sizes, qualities, and qualities of a random subset of similar size for all the algorithms
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' Trt = rbinom(1000,1,0.5)
#' X = data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(1000) ) > 0 )
#' # Evaluating algos
#' res = crossValidateAlgos(
#'     c(trainModelRandomForest,trainModelModLM), # Two functions that trains the model
#'     c(predictByModelRandomForest,predictByModelModLM), # The corresponding prediction functions
#'     subgroupAverageTreatmentEffect, # The function that compute the quality of a subgroup
#'     Y, Trt, X,
#'     numTrials = 5,
#'     balansedSplit, list(InitSplitProportion=0.2)
#'     )
#' print(paste0("Random Forest Model Quality = ",mean(res$Qualities[,1])))
#' print(paste0("Modified Linear Model Quality = ",mean(res$Qualities[,2])))
#'
crossValidateAlgos = function(
  trainModelFuncs, predictByModelFuncs, subgroupQualityFunc,
  dbY, dbTrt, dbX,
  numTrials, splitFunc, splitOpts )
{
  # Prerequesities
  stopifnot(length(trainModelFuncs) == length(predictByModelFuncs) )
  stopifnot(length(dbY) == length(dbTrt) && length(dbTrt) == nrow(dbX))
  stopifnot(is.function(subgroupQualityFunc))
  stopifnot(is.numeric(numTrials))
  stopifnot(!is.null(splitFunc))

  result = list(NumTrials = 5, TrainSize=NULL, TestSize=NULL,
                Subgroups = NULL, Sizes = NULL,
                Qualities = NULL, QRnd = NULL)
  for (trial in 1:numTrials) {
    print(paste0("Trial ", trial))

    ths = splitFunc(splitOpts,dbY,dbTrt,dbX)
    trainX = dbX[ths$Train, ]
    trainY = dbY[ths$Train]
    trainTrt = dbTrt[ths$Train]

    testX = dbX[ths$Holdout, ]
    testY = dbY[ths$Holdout]
    testTrt = dbTrt[ths$Holdout]

    res = evaluateAlgos(
      trainModelFuncs,
      predictByModelFuncs,
      subgroupQualityFunc,
      trainY, trainTrt, trainX, testY, testTrt, testX )
    result$TrainSize=c(result$TrainSize,length(ths$train))
    result$TestSize=c(result$TestSize,length(ths$test))
    result$Subgroups = rbind(result$Subgroups, res$Subgroups)
    result$Sizes = rbind(result$Sizes, res$Sizes)
    result$Qualities = rbind(result$Qualities, res$Qualities)
    result$QRnd = rbind(result$QRnd, res$QRnd)
  }

  return(result)
}

NULL
