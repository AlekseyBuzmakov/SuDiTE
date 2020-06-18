#' Tune function for Weisberg Random Forest
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options
#'
#' @return a model that is able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
##################### RF #############################################
trainWRF_tune <- function(Y, Trt, X, opts = NULL) {
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
  customRF$fit <- function(x, y, param, ...) { ## порядок следования переменных один и тот же
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
##################### RF #############################################



#' Tune function for Weisberg XGBoost
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options
#'
#' @return a model that is able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
##################### XGb #############################################
trainWXG_tune <- function(Y, Trt, X, opts = NULL) {
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
  customXG$fit <- function(x, y, param, ...) { ## порядок следования переменных один и тот же
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
##################### XGb #############################################




#' Tune function for Weisberg SVM
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options
#'
#' @return a model that is able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
##################### SVM #############################################
trainWSVM_tune <- function(Y, Trt, X, opts = NULL) {
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
  customSVM$fit <- function(x, y, param, ...) { ## порядок следования переменных один и тот же
    svm(x, y, probability = TRUE, scale = param$scale, kernel=param$kernel, subset=sample(NROW(x), param$subset*NROW(x)))
  }


  customSVM$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    attr(predict(modelFit, newdata, probability = TRUE), "probabilities")[, 2]
  customSVM$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    attr(predict(modelFit, newdata, probability = TRUE), "probabilities")[, 2]
  customSVM$sort <- function(x) x[order(x[,1]),]
  customSVM$levels <- function(x) x$classes

  control <- trainControl(method="repeatedcv", number=5, repeats=3)
  print("1")
  tunegrid <- expand.grid(.scale=opts$scale, .kernel=opts$kernel, .subset=opts$subset)
  print("2")
  set.seed(123)
  metric <- "Accuracy"
  print("3")
  custom <- train(X, Z1, method=customSVM, metric=metric, tuneGrid=tunegrid, trControl=control)
  print("4")
  TrainOpts=list(scale = custom$bestTune$scale, kernel = custom$bestTune$kernel, subset = custom$bestTune$subset)
  return(TrainOpts)

}
##################### SVM #############################################



#' Tune function for Weisberg GLM
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#' @param opts is a list of options
#'
#' @return a model that is able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
##################### logit #############################################
trainWlogit_tune <- function(Y, Trt, X, opts = NULL) {
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
##################### logit #############################################
