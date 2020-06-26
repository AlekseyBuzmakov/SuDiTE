#' Evaluates several algos by the given train-holdout split
#'
#' Evaluates several algos by the given train-holdout split
#'
#' Takes a set of models and returns the quality of the selected groups by means of subgroupQualityFunc
#'
#' @param models is a vector of model descriptios that contains Train function, Train options, Predict function and the name of the model.
#'  The prototype of the train function is function(Y,Trt,X, opts),
#'    where Y is the response variable, Trt is the 0-1 treatment variable, X is the covariate matrix, and opts is the options from the corresponding model.
#'  The prototype for the Predict function is function(m,X) where m is a trained model and X the observations for prediction.
#' @param subgroupQualityFuncs is a vector of functions that evaluate the quality of a subgroup. The prototype is function(subgroup, Y, Trt), where subgroup is T-F vector defining a subgroup, and Y and Trt are similar as for the functions from trainModelFuncs
#' @param quantile.probs is a vector of probabilities for quantile in order to determine subgroups where an effect should be computed
#' @param trainY a train response variable
#' @param trainTrt a train treatment 0-1 variable
#' @param trainX train covariates
#' @param holdoutY a holdout response variable
#' @param holdoutTrt a holdout treatment 0-1 variable
#' @param holdoutX holdout covariates
#' @return a list with found subgroups in the holdout set, their sizes, qualities, and qualities of a random subset of similar size for all the algorithms
#' @export
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Defining models
#' models=list(
#'   list(Name="RandomForest", TrainFunc=trainModelRandomForest, PredictFunc=predictByModelRandomForest, TrainOpts=NULL),
#'   list(Name="LMbyTian", TrainFunc=trainModelModLM, PredictFunc=predictByModelModLM, TrainOpts=NULL)
#' )
#' Ntr=0.8*N
#' # Evaluating algos
#' res = evaluateAlgos(
#'     models, # The description of the evaluated models
#'     c(subgroupAverageTreatmentEffect,subgroupTotalTreatmentEffect), # The set of functions that compute the quality of a subgroup
#'     seq(0,by=0.2,to = 1), # Groups of 20%
#'     Y[1:Ntr], Trt[1:Ntr], X[1:Ntr,], # Train dataset
#'     Y[(Ntr+1):N], Trt[(Ntr+1):N], X[(Ntr+1):N,] # Holdout dataset
#'     )
#' print(res$Qualities)
#'
evaluateAlgos = function(
  models, subgroupQualityFuncs,
  quantile.probs,
  trainY, trainTrt, trainX, holdoutY, holdoutTrt, holdoutX )
{
  stopifnot(NROW(trainY) == NROW(trainTrt)
            && NROW(trainTrt) == NROW(trainX))
  stopifnot(NROW(holdoutY) == NROW(holdoutTrt)
            && NROW(holdoutTrt) == NROW(holdoutX))

  # We need vectors of length at least 2
  subgroups = NULL
  sizes = NULL
  qualities = NULL
  qrnd = NULL
  names = NULL
  quant=NULL
  for(model in models) {
    names=c(names,model$Name)
    print(paste0("    Training model ", model$Name))
    m = model$TrainFunc(trainY, trainTrt, trainX, model$TrainOpts)
    print(paste0("    Testing model ", model$Name))
    res = model$PredictFunc(m, holdoutX)
    q=NULL
    subgroupQualityFuncs=c(NULL,subgroupQualityFuncs) # Converting to vector
    for( holdoutFunc in subgroupQualityFuncs ) {
     q = c(q, holdoutFunc(res>0, holdoutY, holdoutTrt))
    }
    breaks=quantile(res,quantile.probs)
    # Computing efficience for a quantile
    for(i in 1:(length(quantile.probs)-1)) {
      quant.result = list(Name=model$Name, Block = i, Q.Left.Eq=breaks[i], Q.Right = breaks[i+1])
      if( i < length(quantile.probs)-1) {
        sg = breaks[i] <= res & res < breaks[i+1] | breaks[i] == breaks[i+1] & breaks[i] == res
      } else {
        sg = breaks[i] <= res & res <= breaks[i+1]
      }
      for( holdoutFunc in subgroupQualityFuncs ) {
        quant.result[length(quant.result)+1] = holdoutFunc(sg, holdoutY, holdoutTrt)
      }
      names(quant.result)[-(1:4)]=paste0("QFunc",1:(length(quant.result)-4))
      quant=rbind.data.frame(quant,quant.result,deparse.level = 0,stringsAsFactors = F)
    }
    subgroups = cbind(subgroups, res)
    sizes = c(sizes, sum(res))
    qualities = rbind(qualities, q)
  }
  rownames(qualities)=names
  colnames(subgroups)=names
  names(sizes)=names
  return(list(Subgroups = subgroups, Sizes = sizes, Qualities = qualities, Quantile=quant))
}


#' Evaluates the quality of different algos by cross-validation of their predictions
#'
#' Evaluates the quality of different algos by cross-validation of their predictions
#'
#' Takes a set of models and returns the quality of the selected groups by means of subgroupQualityFunc in Cross-Validation
#'
#' @param models is a vector of model descriptios that contains Train function, Train options, Predict function and the name of the model.
#'  The prototype of the train function is function(Y,Trt,X, opts),
#'    where Y is the response variable, Trt is the 0-1 treatment variable, X is the covariate matrix, and opts is the options from the corresponding model.
#'  The prototype for the Predict function is function(m,X) where m is a trained model and X the observations for prediction.
#' @param subgroupQualityFuncs is a vector of functions that evaluate the quality of a subgroup. The prototype is function(subgroup, Y, Trt), where subgroup is T-F vector defining a subgroup, and Y and Trt are similar as for the functions from trainModelFuncs
#' @param quantile.probs is a vector of probabilities for quantile in order to determine subgroups where an effect should be computed
#' @param dbY a response variable
#' @param dbTrt a treatment 0-1 variable
#' @param dbX train covariates
#' @param numTrials a number of times a random division in train-holdout subdataset should be taken
#' @param splitFunc a function that splits the dataset into training and holdout sets
#' @param splitOpts  the options that are passed to trainHoldoutSplittingFunc
#'
#' @return a list with found subgroups in the holdout set of length holdoutProportion*length(dbY)*numTrials, their sizes, qualities, and qualities of a random subset of similar size for all the algorithms
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Defining models
#' models=list(
#'   list(Name="RandomForest", TrainFunc=trainModelRandomForest, PredictFunc=predictByModelRandomForest, TrainOpts=NULL),
#'   list(Name="LMbyTian", TrainFunc=trainModelModLM, PredictFunc=predictByModelModLM, TrainOpts=NULL),
#'   list(Name="ALL", TrainFunc=function(a,b,c,d){NULL}, PredictFunc=function(m,X){rep(1,nrow(X))},TrainOpts=NULL)
#' )
#' # Evaluating algos
#' res = crossValidateAlgos(
#'     models, # The description of the evaluated models
#'     c(subgroupAverageTreatmentEffect,subgroupTotalTreatmentEffect), # The set of functions that compute the quality of a subgroup
#'     seq(0,by=0.2,to = 1), # Groups of 20%
#'     Y, Trt, X,
#'     numTrials = 5,
#'     balansedSplit, list(InitSplitProportion=0.2)
#'     )
#' aggregate(cbind(V1, V2) ~ Model, res$Qualities, FUN=mean)
#'
crossValidateAlgos = function(
  models, subgroupQualityFuncs,
  quantile.probs,
  dbY, dbTrt, dbX,
  numTrials, splitFunc, splitOpts )
{
  # Prerequesities
  stopifnot(NROW(dbY) == NROW(dbTrt) && NROW(dbTrt) == NROW(dbX))
  stopifnot(is.numeric(numTrials))
  stopifnot(!is.null(splitFunc))

  result = list(NumTrials = numTrials, TrainSize=NULL, holdoutSize=NULL,
                Subgroups = list(), Sizes = NULL,
                Qualities = NULL, QRnd = NULL, Quantiles=NULL)
  for (trial in 1:numTrials) {
    print(paste0("Trial ", trial))

    ths = splitFunc(splitOpts,dbY,dbTrt,dbX)
    trainX = dbX[ths$Train, ]
    trainY = dbY[ths$Train]
    trainTrt = dbTrt[ths$Train]

    holdoutX = dbX[ths$Holdout, ]
    holdoutY = dbY[ths$Holdout]
    holdoutTrt = dbTrt[ths$Holdout]

    res = evaluateAlgos(
      models,
      subgroupQualityFuncs, quantile.probs,
      trainY, trainTrt, trainX, holdoutY, holdoutTrt, holdoutX )
    result$TrainSize=c(result$TrainSize,length(ths$Train))
    result$holdoutSize=c(result$holdoutSize,length(ths$Holdout))
    rownames(res$Subgroups)=ths$Holdout
    result$Subgroups[[length(result$Subgroups)+1]] = res$Subgroups
    result$Sizes = rbind(result$Sizes, res$Sizes)
    res$Qualities=as.data.frame(res$Qualities)
    res$Qualities$Trial = trial
    res$Qualities$Model = rownames(res$Qualities)
    rownames(res$Qualities)=NULL
    result$Qualities = rbind(result$Qualities, res$Qualities)
    result$Quantiles = rbind(result$Quantiles, res$Quantile)
  }
  result$Qualities$Model=as.factor(res$Qualities$Model)
  return(result)
}

NULL






#' Evaluates the quality of different algos by parallel cross-validation of their predictions
#'
#' Evaluates the quality of different algos by parallel cross-validation of their predictions
#'
#' Takes a set of models and returns the quality of the selected groups by means of subgroupQualityFunc in Cross-Validation
#'
#' @param models is a vector of model descriptios that contains Train function, Train options, Predict function and the name of the model.
#'  The prototype of the train function is function(Y,Trt,X, opts),
#'    where Y is the response variable, Trt is the 0-1 treatment variable, X is the covariate matrix, and opts is the options from the corresponding model.
#'  The prototype for the Predict function is function(m,X) where m is a trained model and X the observations for prediction.
#' @param subgroupQualityFuncs is a vector of functions that evaluate the quality of a subgroup. The prototype is function(subgroup, Y, Trt), where subgroup is T-F vector defining a subgroup, and Y and Trt are similar as for the functions from trainModelFuncs
#' @param quantile.probs is a vector of probabilities for quantile in order to determine subgroups where an effect should be computed
#' @param dbY a response variable
#' @param dbTrt a treatment 0-1 variable
#' @param dbX train covariates
#' @param numTrials a number of times a random division in train-holdout subdataset should be taken
#' @param splitFunc a function that splits the dataset into training and holdout sets
#' @param splitOpts  the options that are passed to trainHoldoutSplittingFunc
#'
#' @return a list with found subgroups in the holdout set of length holdoutProportion*length(dbY)*numTrials, their sizes, qualities, and qualities of a random subset of similar size for all the algorithms
#' @export
#'
#' @examples
#'
# crossValidateAlgos_par
crossValidateAlgos_par = function(
  models, subgroupQualityFuncs,
  quantile.probs,
  dbY, dbTrt, dbX,
  numTrials, splitFunc, splitOpts )
{
  # Prerequesities
  ####NEWNEWNEW####
  require(parallel)
  ####NEWNEWNEW####
  #кластер
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)

  stopifnot(NROW(dbY) == NROW(dbTrt) && NROW(dbTrt) == NROW(dbX))
  stopifnot(is.numeric(numTrials))
  stopifnot(!is.null(splitFunc))

  result = list(NumTrials = numTrials, TrainSize=NULL, holdoutSize=NULL,
                Subgroups = list(), Sizes = NULL,
                Qualities = NULL, QRnd = NULL, Quantiles=NULL)

  #######################

  thsTune = splitFunc(splitOpts,dbY,dbTrt,dbX)  ### - тюнинг всех моделей на одном датасете (две опции (splitOpts): если тюнинг, то одна опция, если нет - то другая)
  tuneX = dbX[thsTune$Train,]
  tuneY = dbY[thsTune$Train,]
  tuneTrt = dbTrt[thsTune$Train,]


  ### функция распараллеливания тюнинга ###


  parallel_tune <- function(x, tuneY, tuneTrt, tuneX) {
    model = x
    names=c(names,model$Name)
    if (is.null(model$TuneOpts) == T) {
      model$TrainOpts = model$TrainOpts
    } else {
      model$TrainOpts = model$TuneFunc(tuneY, tuneTrt, tuneX, model$TuneOpts)
    }

    list(list(Name = model$Name, TrainFunc = model$TrainFunc, PredictFunc = model$PredictFunc, TuneFunc = model$TuneFunc, TuneOpts = model$TuneOpts, TrainOpts = model$TrainOpts))
  }

  clusterExport(cl, c("models", "subgroupQualityFuncs"))

  models <- parSapply(cl, models, parallel_tune, tuneY, tuneTrt, tuneX)

  ### функция распараллеливания тюнинга ###


  #######################


  ### функция распараллеливания кросс-валидации ###

  numTrials <- 1:numTrials
  num <- split(numTrials, ceiling(seq_along(numTrials)/4))

  parallel_loop <- function(x, y) {
    numTrials = x
    subgroupQualityFuncs = y
    for (trial in numTrials) {
      print(paste0("Trial ", trial))

      ths = splitFunc(splitOpts,dbY,dbTrt,dbX)
      trainX = dbX[ths$Train, ]
      trainY = dbY[ths$Train,]
      trainTrt = dbTrt[ths$Train,]

      holdoutX = dbX[ths$Holdout, ]
      holdoutY = dbY[ths$Holdout,]
      holdoutTrt = dbTrt[ths$Holdout,]

      res = evaluateAlgos(
        models,
        subgroupQualityFuncs, quantile.probs,
        trainY, trainTrt, trainX, holdoutY, holdoutTrt, holdoutX )
      result$TrainSize=c(result$TrainSize,length(ths$Train))
      result$holdoutSize=c(result$holdoutSize,length(ths$Holdout))
      rownames(res$Subgroups)=ths$Holdout
      result$Subgroups[[length(result$Subgroups)+1]] = res$Subgroups
      result$Sizes = rbind(result$Sizes, res$Sizes)
      res$Qualities=as.data.frame(res$Qualities)
      res$Qualities$Trial = trial
      res$Qualities$Model = rownames(res$Qualities)
      rownames(res$Qualities)=NULL
      result$Qualities = rbind(result$Qualities, res$Qualities)
      result$Quantiles = rbind(result$Quantiles, res$Quantile)
    }
  }

  res <- parSapply(cl, num, subgroupQualityFuncs, parallel_loop)

  ### функция распараллеливания кросс-валидации ###



  result$Qualities$Model=as.factor(res$Qualities$Model)
  return(result)

  #
  stopCluster(cl)
}
