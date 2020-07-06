#' Tune function for generalized linear model where binary response transformation as defined by Weisberg et al., 2015 is used (Tune function for *trainWeisbergGLM*)
#'
#' This function tunes parameters of generalized linear model using a grid search over supplied parameter ranges such as alpha, lambda.
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options over that parameters of generalized linear model is chosen
#'
#' @return parameters found over grid search that then are passed in TrainFunc
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fiting model
#' tuneWeisbergGLM(Y, Trt, X, opts = list(alpha = c(0, 1), lambda = c(0.05, 0.15, 0.2)))
#'
tuneWeisbergGLM <- function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  #p=sum(Trt)/NROW(Trt)
  Z = modifyDataByWeisberg(Y, Trt)
  Z <- Y
  Z <- as.data.frame(Z)
  names(Z) <- "Z"
  d = cbind(X, Z)
  d$Z <- as.factor(d$Z)
  require(caret)


  control <- trainControl(method="repeatedcv", number=5, repeats=3, verboseIter=FALSE)
  tunegrid <- expand.grid(.alpha=opts$alpha, .lambda=opts$lambda)
  set.seed(123)
  metric <- "Accuracy"

  custom <- train(d[,-length(d)], d$Z, method="glmnet", metric=metric, maximize=FALSE, tuneGrid=tunegrid, trControl=control)

  TrainOpts=list(alpha = custom$bestTune$alpha, lambda = custom$bestTune$lambda)
  return(TrainOpts)
}





#' Tune function for Random Forest model where binary response transformation as defined by Weisberg et al., 2015 is used (Tune function for *trainWeisbergRF*)
#'
#' This function tunes parameters of Random Forest model using a grid search over supplied parameter ranges such as mtry, ntree, nodesize.
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options over that parameters of Random Forest model is chosen
#'
#' @return parameters found over grid search that then are passed in TrainFunc
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fiting model
#' tuneWeisbergRF(Y, Trt, X, opts = list(mtry = c(1:2), ntree = c(5, 10, 15), nodesize = c(100, 200, 500)))
#'
tuneWeisbergRF <- function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  #p=sum(Trt)/NROW(Trt)
  Z = modifyDataByWeisberg(Y, Trt)
  Z <- as.data.frame(Z)
  colnames(Z) <- "Z"
  d = cbind(X, Z)
  d$Z <- as.factor(d$Z)
  require(randomForest)
  require(caret)

  customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
  customRF$parameters <- data.frame(parameter = c("mtry", "ntree", "nodesize"), class = rep("numeric", 3), label = c("mtry", "ntree", "nodesize"))
  customRF$grid <- function(x, y, len = NULL, search = "grid") {}
  customRF$fit <- function(x, y, param, ...) {
    randomForest(x, y, mtry = param$mtry, ntree=param$ntree, nodesize=param$nodesize, ...)
  }

  customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata)
  customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata, type = "prob")
  customRF$sort <- function(x) x[order(x[,1]),]
  customRF$levels <- function(x) x$classes

  control <- trainControl(method="repeatedcv", number=5, repeats=3)
  tunegrid <- expand.grid(.mtry=opts$mtry, .ntree=opts$ntree, .nodesize=opts$nodesize)
  set.seed(123)
  metric <- "Accuracy"
  custom <- train(Z~., data=d, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
  TrainOpts=list(mtry = custom$bestTune$mtry, ntree = custom$bestTune$ntree, nodesize = custom$bestTune$nodesize)
  return(TrainOpts)
}




#' Tune function for XGBoost model where binary response transformation as defined by Weisberg et al., 2015 is used (Tune function for *trainWeisbergXGb*)
#'
#' This function tunes parameters of XGBoost model using a grid search over supplied parameter ranges such as nrounds, eta, subsample, depth.
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options over that parameters of XGBoost model is chosen
#'
#' @return parameters found over grid search that then are passed in TrainFunc
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fiting model
#' tuneWeisbergXGb(Y, Trt, X, opts = list(nrounds = c(10, 15, 20), eta = c(0.3, 0.35), subsample = c(0.5, 0.6, 0.8), depth = c(2, 4, 5)))
#'
tuneWeisbergXGb <- function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  #p=sum(Trt)/NROW(Trt)
  Z = modifyDataByWeisberg(Y, Trt)
  Z <- as.data.frame(Z)
  colnames(Z) <- "Z"
  d = cbind(X, Z)
  require(xgboost)
  require(caret)

  customXG <- list(type = "Regression", library = "xgboost", loop = NULL)
  customXG$parameters <- data.frame(parameter = c("nrounds", "eta", "subsample", "depth"), class = rep("numeric", 4), label = c("nrounds", "eta", "subsample", "depth"))
  customXG$grid <- function(x, y, len = NULL, search = "grid") {}
  customXG$fit <- function(x, y, param, ...) {
    xgboost(as.matrix(x), as.matrix(y),
            objective = "binary:logistic",
            verbose = 0,nrounds = param$nrounds, eta=param$eta, subsample=param$subsample, depth=param$depth, ...)
  }


  customXG$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, as.matrix(newdata))
  customXG$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, as.matrix(newdata))
  customXG$sort <- function(x) x[order(x[,1]),]
  customXG$levels <- function(x) x$classes

  control <- trainControl(method="repeatedcv", number=5, repeats=3)
  tunegrid <- expand.grid(.nrounds=opts$nrounds, .eta=opts$eta, .subsample=opts$subsample, .depth=opts$depth)
  set.seed(123)
  metric <- "RMSE"

  custom <- train(d[, -length(d)], d$Z, method=customXG, metric=metric, tuneGrid=tunegrid, trControl=control)
  TrainOpts=list(nrounds = custom$bestTune$nrounds, eta = custom$bestTune$eta, subsample = custom$bestTune$subsample, depth = custom$bestTune$depth)
  return(TrainOpts)
}




#' Tune function for SVM model where binary response transformation as defined by Weisberg et al., 2015 is used (Tune function for *trainWeisbergSVM*)
#'
#' This function tunes parameters of SVM model using a grid search over supplied parameter ranges such as scale, kernel, subset.
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options over that parameters of SVM model is chosen
#'
#' @return parameters found over grid search that then are passed in TrainFunc
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fiting model
#' tuneWeisbergSVM(Y, Trt, X, opts = list(scale = c(F, T), kernel = "linear", subset = c(0.05, 0.15, 0.2)))
#'
tuneWeisbergSVM <- function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  #p=sum(Trt)/NROW(Trt)
  Z = modifyDataByWeisberg(Y, Trt)
  Z <- as.data.frame(Z)
  colnames(Z) <- "Z"
  Z$Z <- as.factor(Z$Z)
  Z1 <- Z$Z
  require(e1071)
  require(caret)


  customSVM <- list(type = "Classification", library = "e1071", loop = NULL)
  customSVM$parameters <- data.frame(parameter = c("scale", "kernel", "subset"), class = c("logical", "character", "numeric"), label = c("scale", "kernel", "subset"))
  customSVM$grid <- function(x, y, len = NULL, search = "grid") {}
  customSVM$fit <- function(x, y, param, ...) {
    svm(x, y, probability = TRUE, scale = param$scale, kernel=param$kernel, subset=sample(NROW(x), param$subset*NROW(x)))
  }


  customSVM$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    attr(predict(modelFit, newdata, probability = TRUE), "probabilities")[, 2]
  customSVM$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    attr(predict(modelFit, newdata, probability = TRUE), "probabilities")[, 2]
  customSVM$sort <- function(x) x[order(x[,1]),]
  customSVM$levels <- function(x) x$classes

  control <- trainControl(method="repeatedcv", number=5, repeats=3)
  tunegrid <- expand.grid(.scale=opts$scale, .kernel=opts$kernel, .subset=opts$subset)
  set.seed(123)
  metric <- "Accuracy"
  custom <- train(X, Z1, method=customSVM, metric=metric, tuneGrid=tunegrid, trControl=control)
  TrainOpts=list(scale = custom$bestTune$scale, kernel = custom$bestTune$kernel, subset = custom$bestTune$subset)
  return(TrainOpts)
}



#' Tune function for generalized linear models where difference score method is used (Tune function for *train2MGML*)
#'
#' This function tunes parameters of generalized linear models using a grid search over supplied parameter ranges such as alpha, lambda. Found parameters are passed in TrainFunc
#'
#' @param Y the binary response varibale
#' @param Trt the binary treatment variable
#' @param X the numeric covariates matrix
#' @param opts a list of options over that parameters of generalized linear models is chosen (NOTE: should be only ONE common list of parameters for the two models, see Example)
#'
#' @return two lists of parameters found over grid search that then are passed in TrainFunc, one list of parameters for model based on the treated observations (Trt = 1), and one list of parameters for model based on the control observations (Trt = 0)
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fiting model
#' tune2MGML(Y, Trt, X, opts = list(alpha = c(0, 1), lambda = c(0.05, 0.15, 0.2)))
#'
tune2MGML <- function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  Y <- as.data.frame(Y)
  colnames(Y) <- "Y"
  d = cbind(Trt, X, Y)
  d$Y <- as.factor(d$Y)

  d_Trt = d[d[, 1] == 1,]
  d_Trt = d_Trt[, -1]

  d_NonTrt = d[d[, 1] == 0,]
  d_NonTrt = d_Trt[, -1]

  require(caret)

  control <- trainControl(method="repeatedcv", number=5, repeats=3, verboseIter=FALSE)
  tunegrid <- expand.grid(.alpha=opts$alpha, .lambda=opts$lambda)
  set.seed(123)
  metric <- "Accuracy"

  custom_Trt <- train(d_Trt[,-length(d_Trt)], d_Trt$Y, method="glmnet", metric=metric, maximize=FALSE, tuneGrid=tunegrid, trControl=control)
  custom_NonTrt <- train(d_NonTrt[,-length(d_NonTrt)], d_NonTrt$Y, method="glmnet", metric=metric, maximize=FALSE, tuneGrid=tunegrid, trControl=control)

  TrainOpts=list(Trt = list(alpha = custom_Trt$bestTune$alpha, lambda = custom_Trt$bestTune$lambda),
                 NonTrt = list(alpha = custom_NonTrt$bestTune$alpha, lambda = custom_NonTrt$bestTune$lambda))
  return(TrainOpts)

}


#' Tune function for Random Forest models where difference score method is used (Tune function for *train2MRF*)
#'
#' This function tunes parameters of Random Forest models  using a grid search over supplied parameter ranges such as mtry, ntree, nodesize. Found parameters are passed in TrainFunc
#'
#' @param Y the binary response varibale
#' @param Trt the binary treatment variable
#' @param X the numeric covariates matrix
#' @param opts a list of options over that parameters of generalized linear models is chosen (NOTE: should be only ONE common list of parameters for the two models, see Example)
#'
#' @return two lists of parameters found over grid search that then are passed in TrainFunc, one list of parameters for model based on the treated observations (Trt = 1), and one list of parameters for model based on the control observations (Trt = 0)
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fiting model
#' tune2MRF(Y, Trt, X, opts = list(mtry = c(1:2), ntree = c(5, 10, 15), nodesize = c(100, 200, 500)))
#'
tune2MRF <- function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  Y <- as.data.frame(Y)
  colnames(Y) <- "Y"
  d = cbind(Trt, X, Y)
  d$Y <- as.factor(d$Y)

  d_Trt = d[d[, 1] == 1,]
  d_Trt = d_Trt[, -1]

  d_NonTrt = d[d[, 1] == 0,]
  d_NonTrt = d_Trt[, -1]

  require(randomForest)
  require(caret)

  customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
  customRF$parameters <- data.frame(parameter = c("mtry", "ntree", "nodesize"), class = rep("numeric", 3), label = c("mtry", "ntree", "nodesize"))
  customRF$grid <- function(x, y, len = NULL, search = "grid") {}
  customRF$fit <- function(x, y, param, ...) {
    randomForest(x, y, mtry = param$mtry, ntree=param$ntree, nodesize=param$nodesize, ...)
  }

  customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata)
  customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata, type = "prob")
  customRF$sort <- function(x) x[order(x[,1]),]
  customRF$levels <- function(x) x$classes

  control <- trainControl(method="repeatedcv", number=5, repeats=3)
  tunegrid <- expand.grid(.mtry=opts$mtry, .ntree=opts$ntree, .nodesize=opts$nodesize)
  set.seed(123)
  metric <- "Accuracy"
  custom_Trt <- train(Y ~ ., data=d_Trt, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
  custom_NonTrt <- train(Y ~ ., data=d_NonTrt, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
  TrainOpts=list(Trt = list(mtry = custom_Trt$bestTune$mtry, ntree = custom_Trt$bestTune$ntree, nodesize = custom_Trt$bestTune$nodesize),
                 NonTrt = list(mtry = custom_NonTrt$bestTune$mtry, ntree = custom_NonTrt$bestTune$ntree, nodesize = custom_NonTrt$bestTune$nodesize))
  return(TrainOpts)

}


#' Tune function for XGBoost models where difference score method is used (Tune function for *train2MXGb*)
#'
#' This function tunes parameters of XGBoost models using a grid search over supplied parameter ranges such as nrounds, eta, subsample, depth. Found parameters are passed in TrainFunc
#'
#' @param Y the binary response varibale
#' @param Trt the binary treatment variable
#' @param X the numeric covariates matrix
#' @param opts a list of options over that parameters of generalized linear models is chosen (NOTE: should be only ONE common list of parameters for the two models, see Example)
#'
#' @return two lists of parameters found over grid search that then are passed in TrainFunc, one list of parameters for model based on the treated observations (Trt = 1), and one list of parameters for model based on the control observations (Trt = 0)
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fiting model
#' train2MXGb(Y, Trt, X, opts = list(nrounds = c(10, 15, 20), eta = c(0.3, 0.35), subsample = c(0.5, 0.6, 0.8), depth = c(2, 4, 5)))
#'
train2MXGb <- function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  Y <- as.data.frame(Y)
  colnames(Y) <- "Y"
  d = cbind(Trt, X, Y)

  d_Trt = d[d[, 1] == 1,]
  d_Trt = d_Trt[, -1]

  d_NonTrt = d[d[, 1] == 0,]
  d_NonTrt = d_Trt[, -1]

  require(xgboost)
  require(caret)

  customXG <- list(type = "Regression", library = "xgboost", loop = NULL)
  customXG$parameters <- data.frame(parameter = c("nrounds", "eta", "subsample", "depth"), class = rep("numeric", 4), label = c("nrounds", "eta", "subsample", "depth"))
  customXG$grid <- function(x, y, len = NULL, search = "grid") {}
  customXG$fit <- function(x, y, param, ...) {
    xgboost(as.matrix(x), as.matrix(y),
            objective = "binary:logistic",
            verbose = 0,nrounds = param$nrounds, eta=param$eta, subsample=param$subsample, depth=param$depth, ...)
  }


  customXG$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, as.matrix(newdata))
  customXG$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, as.matrix(newdata))
  customXG$sort <- function(x) x[order(x[,1]),]
  customXG$levels <- function(x) x$classes

  control <- trainControl(method="repeatedcv", number=5, repeats=3)
  tunegrid <- expand.grid(.nrounds=opts$nrounds, .eta=opts$eta, .subsample=opts$subsample, .depth=opts$depth)
  set.seed(123)
  metric <- "RMSE"

  custom_Trt <- train(d_Trt[,-length(d_Trt)], d_Trt$Y, method=customXG, metric=metric, tuneGrid=tunegrid, trControl=control)
  custom_NonTrt <- train(d_NonTrt[,-length(d_NonTrt)], d_NonTrt$Y, method=customXG, metric=metric, tuneGrid=tunegrid, trControl=control)
  TrainOpts=list(Trt = list(nrounds = custom_Trt$bestTune$nrounds, eta = custom_Trt$bestTune$eta, subsample = custom_Trt$bestTune$subsample, depth = custom_Trt$bestTune$depth),
                 NonTrt = list(nrounds = custom_NonTrt$bestTune$nrounds, eta = custom_NonTrt$bestTune$eta, subsample = custom_NonTrt$bestTune$subsample, depth = custom_NonTrt$bestTune$depth))
  return(TrainOpts)

}


#' Tune function for SVM models where difference score method is used (Tune function for *train2MSVM*)
#'
#' This function tunes parameters of SVM models using a grid search over supplied parameter ranges such as scale, kernel, subset. Found parameters are passed in TrainFunc
#'
#' @param Y the binary response varibale
#' @param Trt the binary treatment variable
#' @param X the numeric covariates matrix
#' @param opts a list of options over that parameters of generalized linear models is chosen (NOTE: should be only ONE common list of parameters for the two models, see Example)
#'
#' @return two lists of parameters found over grid search that then are passed in TrainFunc, one list of parameters for model based on the treated observations (Trt = 1), and one list of parameters for model based on the control observations (Trt = 0)
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' N = 1000
#' Trt = rbinom(N,1,0.5)
#' X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
#' # Fiting model
#' tune2MSVM(Y, Trt, X, opts = list(scale = c(F, T), kernel = "linear", subset = c(0.05, 0.15, 0.2)))
#'
tune2MSVM <- function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  Y <- as.data.frame(Y)
  colnames(Y) <- "Y"
  d = cbind(Trt, X, Y)
  d$Y <- as.factor(d$Y)

  d_Trt = d[d[, 1] == 1,]
  Y_Trt <- d_Trt$Y
  X_Trt = d_Trt[, -c(1, length(d_Trt))]

  d_NonTrt = d[d[, 1] == 0,]
  Y_NonTrt <- d_NonTrt$Y
  X_NonTrt = d_NonTrt[, -c(1, length(d_NonTrt))]

  require(e1071)
  require(caret)

  customSVM <- list(type = "Classification", library = "e1071", loop = NULL)
  customSVM$parameters <- data.frame(parameter = c("scale", "kernel", "subset"), class = c("logical", "character", "numeric"), label = c("scale", "kernel", "subset"))
  customSVM$grid <- function(x, y, len = NULL, search = "grid") {}
  customSVM$fit <- function(x, y, param, ...) {
    svm(x, y, probability = TRUE, scale = param$scale, kernel=param$kernel, subset=sample(NROW(x), param$subset*NROW(x)))
  }


  customSVM$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    attr(predict(modelFit, newdata, probability = TRUE), "probabilities")[, 2]
  customSVM$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    attr(predict(modelFit, newdata, probability = TRUE), "probabilities")[, 2]
  customSVM$sort <- function(x) x[order(x[,1]),]
  customSVM$levels <- function(x) x$classes

  control <- trainControl(method="repeatedcv", number=5, repeats=3)
  tunegrid <- expand.grid(.scale=opts$scale, .kernel=opts$kernel, .subset=opts$subset)
  set.seed(123)
  metric <- "Accuracy"
  custom_Trt <- train(X_Trt, Y_Trt, method=customSVM, metric=metric, tuneGrid=tunegrid, trControl=control)
  custom_NonTrt <- train(X_NonTrt, Y_NonTrt, method=customSVM, metric=metric, tuneGrid=tunegrid, trControl=control)
  TrainOpts=list(Trt = list(scale = custom_Trt$bestTune$scale, kernel = custom_Trt$bestTune$kernel, subset = custom_Trt$bestTune$subset),
                 NonTrt = list(scale = custom_NonTrt$bestTune$scale, kernel = custom_NonTrt$bestTune$kernel, subset = custom_NonTrt$bestTune$subset))
  return(TrainOpts)

}




