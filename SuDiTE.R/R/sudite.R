#' SuDiTE: A package for comaparison of different uplift models
#'
#' It includes comparison infrastructure as well as wrap-ups for many existing algos
#'
#' It is written as a part of SuDiTE project in the National Research University Higher School of Economics, Perm, Russia
#'
#' @name SuDiTE
#' @docType package


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
             subgroupQuality(rep( 1, nrow(testX)), testY, testTrt)
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
#' @param testProportion a proportion of observation to be put in the test set
#' @param testX test covariates
#' @return a list with found subgroups in the test set of length testProportion*length(dbY)*numTrials, their sizes, qualities, and qualities of a random subset of similar size for all the algorithms
#' @export
#' @examples
#'
#'
crossValidateAlgos = function(
  trainModelFuncs, predictByModelFuncs, subgroupQualityFunc,
  dbY, dbTrt, dbX, numTrials = 5, testProportion = 0.2 )
{
  stopifnot(length(trainModelFuncs) == length(predictByModelFuncs) )
  stopifnot(length(dbY) == length(dbTrt) && length(dbTrt) == nrow(dbX))
  stopifnot(is.function(subgroupQualityFunc))
  stopifnot(is.integer(numTrials))
  stopifnot(0 < testProportion && testProportion < 1)

  trainSize = nrow(dbX) * (1 - testProportion)
  testSize = nrow(dbX) - trainSize

  result = list(NumTrials = 5, TestNum = testSize,
                Subgroups = NULL, Sizes = NULL,
                Qualities = NULL, QRnd = NULL)
  for (trial in 1:numTrials) {
    print(paste0("Trial ", trial))
    inds = sample(nrow(dbX), trainSize)
    trainX = dbX[inds, ]
    trainY = dbY[inds]
    trainTrt = dbTrt[inds]

    testX = dbX[-inds, ]
    testY = dbY[-inds]
    testTrt = dbTrt[-inds]

    res = evaluateAlgos(
      trainModelFuncs,
      predictByModelFuncs,
      subgroupQualityFunc,
      trainY, trainTrt, trainX, testY, testTrt, testX )
    result$Subgroups = rbind(result$Subgroups, res$Subgroups)
    result$Sizes = rbind(result$Sizes, res$Sizes)
    result$Qualities = rbind(result$Qualities, res$Qualities)
    result$QRnd = rbind(result$QRnd, res$QRnd)
  }

  return(result)
}

NULL
