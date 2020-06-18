################################ BINARY OUTCOME ############################################################################

############################################################################################################################
################################    1. WEISBERG    #########################################################################
############################################################################################################################

#' Train Weisberg GLM
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
######### Train Weisberg #########
# trainWeisbergGLM
trainWeisbergGLM = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  #p=sum(Trt)/NROW(Trt)
  Z = modifyDataByWeisberg(Y, Trt)
  Z <- Y
  Z <- as.data.frame(Z)
  names(Z) <- "Z"
  d = cbind(X, Z)
  model=glm(Z ~ ., data = d, family = quasibinomial(link = "logit"))
  return(model)
}


#' Train Weisberg Random Forest
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
# trainWeisbergRF
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


#' Train Weisberg XGBoost
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
# trainWeisbergXGb
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
                  eta = 0.3,
                  subsample = 0.85,
                  depth = 3,
                  verbose = 0)
  return(model)
}


#' Train Weisberg SVM
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
# trainWeisbergSVM
trainWeisbergSVM = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  #p=sum(Trt)/NROW(Trt)
  Z = modifyDataByWeisberg(Y, Trt)
  Z <- as.data.frame(Z)
  colnames(Z) <- "Z"
  d = cbind(X, Z)
  require(e1071)
  d$Z = as.factor(d$Z)
  d[,-13] = scale(d[,-13])
  model = svm(x = d[,-c(13, 10, 11, 7, 8)], y = d[,13], scale = F, kernel = "linear", probability = TRUE, subset = sample(NROW(d), 0.015*NROW(d)))
  return(model)
}


############################################################################################################################
################################    2. TIAN      ###########################################################################
############################################################################################################################


#' Train Tian GLM
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
######### Train Tian #########
# trainTianGLM
trainTianGLM = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  #p=sum(Trt)/NROW(Trt)
  W = modifyDataByTian(Trt, X)
  #colnames(Z) <- "Z"
  d = cbind(W, Y)
  model=glm(Y ~ ., data = d, family = quasibinomial(link = "logit"))
  return(model)
}


############################################################################################################################
################################    3. TWO MODELS    #######################################################################
############################################################################################################################

#' Train Two models GLM
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
######### Train Two models #########
# train2MGML
train2MGML = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  Y <- as.data.frame(Y)
  colnames(Y) <- "Y"
  d = cbind(Trt, X, Y)
  d1 = d[d[, 1] == 1,]
  d1 = d1[, -1]
  d2 = d[d[, 1] == 0,]
  d2 = d2[, -1]

  model1 = glm(Y ~ ., data = d1, family = quasibinomial(link = "logit") )
  model2 = glm(Y ~ ., data = d2, family = quasibinomial(link = "logit") )
  return(list(model1, model2))
}


#' Train Two models Random Forest
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
# train2MRF
train2MRF = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  Y <- as.data.frame(Y)
  colnames(Y) <- "Y"
  d = cbind(Trt, X, Y)
  d1 = d[d[, 1] == 1,]
  d1 = d1[, -1]
  d2 = d[d[, 1] == 0,]
  d2 = d2[, -1]

  require(randomForest)
  model1 = randomForest(as.factor(Y) ~ ., data = d1, ntree = opts$ntree, nodesize = 500)
  model2 = randomForest(as.factor(Y) ~ ., data = d2, ntree = opts$ntree, nodesize = 500)
  return(list(model1, model2))
}


#' Train Two models XGBoost
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
# train2MXGb
train2MXGb = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  Y <- as.data.frame(Y)
  colnames(Y) <- "Y"
  d = cbind(Trt, X, Y)
  d1 = d[d[, 1] == 1,]
  d1 = d1[, -1]
  d2 = d[d[, 1] == 0,]
  d2 = d2[, -1]

  require(xgboost)
  model1 = xgboost(data = as.matrix(d1[, -length(d1)]),
                   label = as.matrix(d1[, length(d1)]),
                   nrounds = opts$nrounds,
                   objective = "binary:logistic",
                   eta = 0.3,
                   subsample = 0.85,
                   depth = 3,
                   verbose = 0)
  model2 = xgboost(data = as.matrix(d2[, -length(d2)]),
                   label = as.matrix(d2[, length(d2)]),
                   nrounds = opts$nrounds,
                   objective = "binary:logistic",
                   eta = 0.3,
                   subsample = 0.85,
                   depth = 3,
                   verbose = 0)
  return(list(model1, model2))
}


#' Train Two models SVM
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
# train2MSVM
train2MSVM = function(Y, Trt, X, opts = NULL) {
  stopifnot(sum(Y == 1) + sum(Y == 0) == NROW(Y))
  Y <- as.data.frame(Y)
  colnames(Y) <- "Y"
  d = cbind(Trt, X, Y)
  d1 = d[d[, 1] == 1,]
  d1 = d1[, -1]
  d2 = d[d[, 1] == 0,]
  d2 = d2[, -1]

  d1$Y = as.factor(d1$Y)
  d1[,-13] = scale(d1[, -13])
  d2$Y = as.factor(d2$Y)
  d2[,-13] = scale(d2[, -13])

  require(e1071)
  model1 = svm(x = d1[,-c(13, 10, 11, 7, 8)], y = d1[, 13], scale = F, kernel = "linear", probability = TRUE, subset = sample(NROW(d1), 0.025*NROW(d1)))
  model2 = svm(x = d2[,-c(13, 10, 11, 7, 8)], y = d2[, 13], scale = F, kernel = "linear", probability = TRUE, subset = sample(NROW(d2), 0.025*NROW(d2)))
  return(list(model1, model2))
}


############################################################################################################################
################################    4. UPLIFT MODEL    #####################################################################
############################################################################################################################


#' Train Uplift model
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
######### Train Uplift model #########
# trainUpliftModelRF
trainUpliftModelRF = function(Y, Trt, X, opts) {
  require(uplift)
  Y1 = as.matrix(Y)
  Y1 = c(Y1)
  Trt1 = as.matrix(Trt)
  Trt1 = c(Trt1)
  Trt1 = as.integer(Trt1)
  return(upliftRF(X, Y1, Trt1, split_method = "Chisq", ntree = 55, bag.fraction = 0.05))
}

