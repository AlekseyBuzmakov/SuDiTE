#' Train function for modified outcome method using generalized linear model
#'
#' This function fits generalized linear model where binary response transformation as defined by Weisberg et al., 2015 is used
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options or list of model parameters such as alpha, lambda
#'
#' @return a model that is able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fitting model
#' trainWeisbergGLM(Y, Trt, X, opts = list(alpha = 0, lambda = 0.15))
#'
trainWeisbergGLM = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  #p=sum(Trt)/NROW(Trt)
  Z = modifyDataByWeisberg(Y, Trt)
  Z <- as.vector(Z)
  names(Z) <- "Z"
  X <- as.matrix(X)
  require(glmnet)
  model=glmnet(X, Z, family = "binomial", alpha = opts$alpha, lambda = opts$lambda)
  return(model)
}

#' Train function for modified outcome method using Random Forest
#'
#' This function fits Random Forest model where binary response transformation as defined by Weisberg et al., 2015 is used
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options or list of model parameters such as mtry, ntree, nodesize
#'
#' @return a model that is able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fitting model
#' trainWeisbergRF(Y, Trt, X, opts = list(mtry = 2, ntree = 15, nodesize = 200))
#'

trainWeisbergRF = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  #p=sum(Trt)/NROW(Trt)
  Z = modifyDataByWeisberg(Y, Trt)
  Z <- as.data.frame(Z)
  colnames(Z) <- "Z"
  d = cbind(X, Z)
  require(randomForest)
  model = randomForest(formula = as.factor(Z) ~ ., data = d, mtry = opts$mtry, ntree = opts$ntree, nodesize = opts$nodesize)
  return(model)
}

#' Train function for modified outcome method using XGBoost
#'
#' This function fits XGBoost model where binary response transformation as defined by Weisberg et al., 2015 is used
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options or list of model parameters such as nrounds, eta, subsample, depth
#'
#' @return a model that is able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fitting model
#' trainWeisbergXGb(Y, Trt, X, opts = list(nrounds = 15, eta = 0.3, subsample = 0.5, depth = 4))
#'
trainWeisbergXGb = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  #p=sum(Trt)/NROW(Trt)
  Z = modifyDataByWeisberg(Y, Trt)
  Z <- as.data.frame(Z)
  colnames(Z) <- "Z"
  d = cbind(X, Z)
  require(xgboost)
  model = xgboost(data = as.matrix(d[, -length(d)]),
                  label = as.matrix(Z),
                  nrounds = opts$nrounds,
                  objective = "binary:logistic",
                  eta = opts$eta,
                  subsample = opts$subsample,
                  depth = opts$depth,
                  verbose = 0)
  return(model)
}

#' Train function for modified outcome method using SVM
#'
#' This function fits SVM model where binary response transformation as defined by Weisberg et al., 2015 is used
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options or list of model parameters such as scale, kernel, subset
#'
#' @return a model that is able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fitting model
#' trainWeisbergSVM(Y, Trt, X, opts = list(scale = F, kernel = "linear", subset = 0.5))
#'
trainWeisbergSVM = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  #p=sum(Trt)/NROW(Trt)
  Z = modifyDataByWeisberg(Y, Trt)
  Z <- as.data.frame(Z)
  colnames(Z) <- "Z"
  d = cbind(X, Z)
  require(e1071)
  d$Z = as.factor(d$Z)
  model = svm(x = d[,-length(d)], y = d[,length(d)], scale = opts$scale, kernel = opts$kernel, probability = TRUE, subset = sample(NROW(d), opts$subset*NROW(d)))
  return(model)
}


#' Train function for modified covariate method using generalized linear model
#'
#' This function fits generalized linear model where covariate transformation as defined by Tian et al., 2014 is used
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options or list of model parameters such as
#'
#' @return a model that is able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fitting model
#' trainTianGLM(Y, Trt, X, opts = list(alpha = 0, lambda = 0.15))
#'
trainTianGLM = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  W = modifyDataByTian(Trt, X)
  W <- as.matrix(W)
  Y <- as.vector(Y)
  require(glmnet)
  model=glmnet(W, Y, family = "binomial", alpha = opts$alpha, lambda = opts$lambda)
  return(model)
}

#' Train function for difference score method using generalized linear models
#'
#' This function fits two independent generalized linear models for the binary response Y, one based on the treated observations (Trt = 1), and one based on the control observations (Trt = 0)
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options to fit models, Trt is a list of options for model based on the treated observations (Trt = 1), and Non_Trt is a list of options for model based on the control observations (Trt = 0). Available parameters are alpha, lambda
#'
#' @return a list of models that are able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fitting models
#' train2MGML(Y, Trt, X, opts = list(Trt = list(alpha = 1, lambda = 0.25), Non_Trt = list(alpha = 0, lambda = 0.15)))
#'
train2MGML = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  Y <- as.data.frame(Y)
  colnames(Y) <- "Y"
  d = cbind(Trt, X, Y)

  d_Trt = d[d[, 1] == 1,]
  Y_Trt <- as.vector(d_Trt$Y)
  X_Trt = as.matrix(d_Trt[, -c(1, length(d_Trt))])
  names(Y_Trt) <- "Y_Trt"

  d_NonTrt = d[d[, 1] == 0,]
  Y_NonTrt <- as.vector(d_NonTrt$Y)
  X_NonTrt = as.matrix(d_NonTrt[, -c(1, length(d_NonTrt))])
  names(Y_NonTrt) <- "Y_NonTrt"

  require(glmnet)
  model_Trt=glmnet(X_Trt, Y_Trt, family = "binomial", alpha = opts$Trt$alpha, lambda = opts$Trt$lambda)
  model_NonTrt=glmnet(X_NonTrt, Y_NonTrt, family = "binomial", alpha = opts$Non_Trt$alpha, lambda = opts$Non_Trt$lambda)

  return(list(model_Trt, model_NonTrt))
}

#' Train function for difference score method using Random Forest models
#'
#' This function fits two independent Random Forest models for the binary response Y, one based on the treated observations (Trt = 1), and one based on the control observations (Trt = 0)
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options to fit models, Trt is a list of options for model based on the treated observations (Trt = 1), and Non_Trt is a list of options for model based on the control observations (Trt = 0). Available parameters are mtry, ntree, nodesize
#'
#' @return a list of models that are able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fitting models
#' train2MRF(Y, Trt, X, opts = list(Trt = list(mtry = 2, ntree = 15, nodesize = 200), Non_Trt = list(mtry = 2, ntree = 15, nodesize = 200)))
#'
train2MRF = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  Y <- as.data.frame(Y)
  colnames(Y) <- "Y"
  d = cbind(Trt, X, Y)

  d_Trt = d[d[, 1] == 1,]
  d_Trt = d_Trt[, -1]

  d_NonTrt = d[d[, 1] == 0,]
  d_NonTrt = d_Trt[, -1]

  require(randomForest)
  model_Trt = randomForest(as.factor(Y) ~ ., data = d_Trt, mtry = opts$mtry, ntree = opts$ntree, nodesize = opts$nodesize)
  model_NonTrt = randomForest(as.factor(Y) ~ ., data = d_NonTrt, mtry = opts$mtry, ntree = opts$ntree, nodesize = opts$nodesize)

  return(list(model_Trt, model_NonTrt))
}

#' Train function for difference score method using XGBoost models
#'
#' This function fits two independent XGBoost models for the binary response Y, one based on the treated observations (Trt = 1), and one based on the control observations (Trt = 0)
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options to fit models, Trt is a list of options for model based on the treated observations (Trt = 1), and Non_Trt is a list of options for model based on the control observations (Trt = 0). Available parameters are nrounds, eta, subsample, depth
#'
#' @return a list of models that are able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fitting models
#' train2MXGb(Y, Trt, X, opts = list(Trt = list(nrounds = 15, eta = 0.3, subsample = 0.5, depth = 4), Non_Trt = list(nrounds = 10, eta = 0.4, subsample = 0.75, depth = 3)))
#'
train2MXGb = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  Y <- as.data.frame(Y)
  colnames(Y) <- "Y"
  d = cbind(Trt, X, Y)

  d_Trt = d[d[, 1] == 1,]
  d_Trt = d_Trt[, -1]

  d_NonTrt = d[d[, 1] == 0,]
  d_NonTrt = d_Trt[, -1]

  require(xgboost)

  model_Trt = xgboost(data = as.matrix(d_Trt[, -length(d_Trt)]),
                  label = as.matrix(d_Trt[, length(d_Trt)]),
                  nrounds = opts$nrounds,
                  objective = "binary:logistic",
                  eta = opts$eta,
                  subsample = opts$subsample,
                  depth = opts$depth,
                  verbose = 0)

  model_NonTrt = xgboost(data = as.matrix(d_NonTrt[, -length(d_NonTrt)]),
                      label = as.matrix(d_NonTrt[, length(d_NonTrt)]),
                      nrounds = opts$nrounds,
                      objective = "binary:logistic",
                      eta = opts$eta,
                      subsample = opts$subsample,
                      depth = opts$depth,
                      verbose = 0)

  return(list(model_Trt, model_NonTrt))
}


#' Train function for difference score method using SVM models
#'
#' This function fits two independent SVM models for the binary response Y, one based on the treated observations (Trt = 1), and one based on the control observations (Trt = 0)
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options to fit models, Trt is a list of options for model based on the treated observations (Trt = 1), and Non_Trt is a list of options for model based on the control observations (Trt = 0). Available parameters are scale, kernel, subset
#'
#' @return a list of models that are able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fitting models
#' train2MSVM(Y, Trt, X, opts = list(Trt = list(scale = F, kernel = "linear", subset = 0.5), Non_Trt = list(scale = T, kernel = "linear", subset = 0.3)))
#'
train2MSVM = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  Y <- as.data.frame(Y)
  colnames(Y) <- "Y"
  d = cbind(Trt, X, Y)

  d_Trt = d[d[, 1] == 1,]
  d_Trt = d_Trt[, -1]
  d_Trt$Y = as.factor(d_Trt$Y)

  d_NonTrt = d[d[, 1] == 0,]
  d_NonTrt = d_Trt[, -1]
  d_NonTrt$Y = as.factor(d_NonTrt$Y)

  require(e1071)

  model_Trt = svm(x = d_Trt[,-length(d_Trt)], y = d_Trt[,length(d_Trt)], scale = opts$scale, kernel = opts$kernel, probability = TRUE, subset = sample(NROW(d_Trt), opts$subset*NROW(d_Trt)))
  model_NonTrt = svm(x = d_NonTrt[,-length(d_NonTrt)], y = d_NonTrt[,length(d_NonTrt)], scale = opts$scale, kernel = opts$kernel, probability = TRUE, subset = sample(NROW(d_NonTrt), opts$subset*NROW(d_NonTrt)))

  return(list(model_Trt, model_NonTrt))
}


#' Train function for Uplift Random forest model
#'
#' This function fits Uplift Random forest model based on Guelman, 2014
#'
#' Train Uplift model
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options or list of model parameters such as split_method, ntree, bag.fraction
#'
#' @return a model that is able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fitting models
#' trainUpliftModelRF(Y, Trt, X, opts = list(split_method = "Chisq", ntree = 55, bag.fraction = 0.5))
#'
trainUpliftModelRF = function(Y, Trt, X, opts) {
  require(uplift)
  Y1 = as.matrix(Y)
  Y1 = c(Y1)
  Trt1 = as.matrix(Trt)
  Trt1 = c(Trt1)
  Trt1 = as.integer(Trt1)
  return(upliftRF(X, Y1, Trt1, split_method = opts$split_method, ntree = opts$ntree, bag.fraction = opts$bag.fraction))
}

