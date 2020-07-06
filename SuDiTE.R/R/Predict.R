#' Predicts if an observation should be treated or not by means of the generalized linear model where binary response transformation as defined by Weisberg et al., 2015 is used (Predict function for *trainWeisbergGLM*)
#'
#' @param m a model trained with *trainWeisbergGLM*
#' @param X is the covariate matrix
#' @param opts is a list of options (NOTE: fraction is one available option, it defines percent (from 0 to 1) of total observations with highest predicted PTE, which are then used in obtaining estimates of model performance)
#'
#' @return a binary vector specifying the observations to be treated
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
#' m = trainWeisbergGLM(Y, Trt, X, opts = list(alpha = 0, lambda = 0.15))
#' # Predicting a good group for new data, target is top 30% observations with highest predicted PTE
#' predictWeisbergGLM(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000)), opts = list(fraction = 0.3))
#'
predictWeisbergGLM = function(m, X, opts = NULL) {
  XX = as.matrix(X)

  pre = (exp(predict(m, XX))/(exp(predict(m, XX)) + 1))*2 - 1
  return(ifelse(pre >= quantile(pre, opts$fraction, na.rm = T), 1, 0))
}


#' Predicts if an observation should be treated or not by means of the Random Forest model where binary response transformation as defined by Weisberg et al., 2015 is used (Predict function for *trainWeisbergRF*)
#'
#' @param m a model trained with *trainWeisbergRF*
#' @param X is the covariate matrix
#' @param opts is a list of options (NOTE: fraction is one available option, it defines percent (from 0 to 1) of total observations with highest predicted PTE, which are then used in obtaining estimates of model performance)
#'
#' @return a binary vector specifying the observations to be treated
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
#' m = trainWeisbergRF(Y, Trt, X, opts = list(mtry = 2, ntree = 15, nodesize = 200))
#' # Predicting a good group for new data, target is top 30% observations with highest predicted PTE
#' predictWeisbergRF(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000)), opts = list(fraction = 0.3))
#'
predictWeisbergRF = function(m, X, opts = NULL) {
  XX = toNumericTable(X)

  tbl = predict(m, XX, type = "prob")
  pre = 2*tbl[, 2] - 1
  return(ifelse(pre >= quantile(pre, opts$fraction, na.rm = T), 1, 0))
}


#' Predicts if an observation should be treated or not by means of the XGboost model where binary response transformation as defined by Weisberg et al., 2015 is used (Predict function for *trainWeisbergXGb*)
#'
#' @param m a model trained with *trainWeisbergXGb*
#' @param X is the covariate matrix
#' @param opts is a list of options (NOTE: fraction is one available option, it defines percent (from 0 to 1) of total observations with highest predicted PTE, which are then used in obtaining estimates of model performance)
#'
#' @return a binary vector specifying the observations to be treated
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
#' m = trainWeisbergXGb(Y, Trt, X, opts = list(nrounds = 15, eta = 0.3, subsample = 0.5, depth = 4))
#' # Predicting a good group for new data, target is top 30% observations with highest predicted PTE
#' predictWeisbergXGb(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000)), opts = list(fraction = 0.3))
#'
predictWeisbergXGb = function(m, X, opts = NULL) {
  XX = toNumericTable(X)

  pre = 2*predict(m, as.matrix(XX)) - 1
  return(ifelse(pre >= quantile(pre, opts$fraction, na.rm = T), 1, 0))
}


#' Predicts if an observation should be treated or not by means of the SVM model where binary response transformation as defined by Weisberg et al., 2015 is used (Predict function for *trainWeisbergSVM*)
#'
#' @param m a model trained with *trainWeisbergSVM*
#' @param X is the covariate matrix
#' @param opts is a list of options (NOTE: fraction is one available option, it defines percent (from 0 to 1) of total observations with highest predicted PTE, which are then used in obtaining estimates of model performance)
#'
#' @return a binary vector specifying the observations to be treated
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
#' m = trainWeisbergSVM(Y, Trt, X, opts = list(scale = F, kernel = "linear", subset = 0.5))
#' # Predicting a good group for new data, target is top 30% observations with highest predicted PTE
#' predictWeisbergSVM(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000)), opts = list(fraction = 0.3))
#'
predictWeisbergSVM = function(m, X, opts = NULL) {
  X = X[,-c(10, 11, 7, 8)]
  XX = toNumericTable(X)

  #return(2*predict(m,XX,probability = TRUE)-1)
  pre = 2*(attr(predict(m, XX, probability = TRUE), "probabilities")[, 2]) - 1
  return(ifelse(pre >= quantile(pre, opts$fraction, na.rm = T), 1, 0))
}



#' Predicts if an observation should be treated or not by means of the generalized linear model where covariate transformation as defined by Tian et al., 2014 is used (Predict function for *trainTianGLM*)
#'
#' @param m a model trained with *trainTianGLM*
#' @param X is the covariate matrix
#' @param opts is a list of options (NOTE: fraction is one available option, it defines percent (from 0 to 1) of total observations with highest predicted PTE, which are then used in obtaining estimates of model performance)
#'
#' @return a binary vector specifying the observations to be treated
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
#' m = trainTianGLM(Y, Trt, X, opts = list(alpha = 0, lambda = 0.15))
#' # Predicting a good group for new data, target is top 30% observations with highest predicted PTE
#' predictTianGLM(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000)), opts = list(fraction = 0.3))
#'
predictTianGLM = function(m, X, opts = NULL) {
  XX = toNumericTable(X/2)

  pre = (exp(predict(m, XX)) - 1)/(exp(predict(m, XX)) + 1)
  return(ifelse(pre >= quantile(pre, opts$fraction, na.rm = T), 1, 0))
}




#' Predicts if an observation should be treated or not by means difference in predictions between two independent generalized linear models (Predict function for *train2MGML*)
#'
#' @param m a model trained with *train2MGML*
#' @param X is the covariate matrix
#' @param opts is a list of options (NOTE: fraction is one available option, it defines percent (from 0 to 1) of total observations with highest predicted PTE, which are then used in obtaining estimates of model performance)
#'
#' @return a binary vector specifying the observations to be treated
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
#' m = train2MGML(Y, Trt, X, opts = list(alpha = 0, lambda = 0.15))
#' # Predicting a good group for new data, target is top 30% observations with highest predicted PTE
#' predict2MGML(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000)), opts = list(fraction = 0.3))
#'
predict2MGML = function(m, X, opts = NULL) {
  X = toNumericTable(X)
  pre = (exp(predict(m[[1]], X))/(exp(predict(m[[1]], X)) + 1)) - (exp(predict(m[[2]], X))/(exp(predict(m[[2]], X)) + 1))
  return(ifelse(pre >= quantile(pre, opts$fraction, na.rm = T), 1, 0))
}


#' Predicts if an observation should be treated or not by means difference in predictions between two independent Random Forest models (Predict function for *train2MRF*)
#'
#' @param m a model trained with *train2MRF*
#' @param X is the covariate matrix
#' @param opts is a list of options (NOTE: fraction is one available option, it defines percent (from 0 to 1) of total observations with highest predicted PTE, which are then used in obtaining estimates of model performance)
#'
#' @return a binary vector specifying the observations to be treated
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
#' m = train2MRF(Y, Trt, X, opts = list(Trt = list(mtry = 2, ntree = 15, nodesize = 200), Non_Trt = list(mtry = 2, ntree = 15, nodesize = 200)))
#' # Predicting a good group for new data, target is top 30% observations with highest predicted PTE
#' predict2MRF(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000)), opts = list(fraction = 0.3))
#'
predict2MRF = function(m, X, opts = NULL) {
  X = toNumericTable(X)
  tbl1 = predict(m[[1]], X, type = "prob")
  tbl2 = predict(m[[2]], X, type = "prob")
  pre = tbl1[,2] - tbl2[,2]
  return(ifelse(pre >= quantile(pre, opts$fraction, na.rm = T), 1, 0))
}


#' Predicts if an observation should be treated or not by means difference in predictions between two independent XGboost models (Predict function for *train2MXGb*)
#'
#' @param m a model trained with *train2MXGb*
#' @param X is the covariate matrix
#' @param opts is a list of options (NOTE: fraction is one available option, it defines percent (from 0 to 1) of total observations with highest predicted PTE, which are then used in obtaining estimates of model performance)
#'
#' @return a binary vector specifying the observations to be treated
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
#' m = train2MXGb(Y, Trt, X, opts = list(Trt = list(nrounds = 15, eta = 0.3, subsample = 0.5, depth = 4), Non_Trt = list(nrounds = 10, eta = 0.4, subsample = 0.75, depth = 3)))
#' # Predicting a good group for new data, target is top 30% observations with highest predicted PTE
#' predict2MXGb(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000)), opts = list(fraction = 0.3))
#'
predict2MXGb = function(m, X, opts = NULL) {
  X = toNumericTable(X)
  pre = predict(m[[1]], as.matrix(X)) - predict(m[[2]], as.matrix(X))
  return(ifelse(pre >= quantile(pre, opts$fraction, na.rm = T), 1, 0))
}


#' Predicts if an observation should be treated or not by means difference in predictions between two independent SVM models (Predict function for *train2MSVM*)
#'
#' @param m a model trained with *train2MSVM*
#' @param X is the covariate matrix
#' @param opts is a list of options (NOTE: fraction is one available option, it defines percent (from 0 to 1) of total observations with highest predicted PTE, which are then used in obtaining estimates of model performance)
#'
#' @return a binary vector specifying the observations to be treated
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
#' m = train2MSVM(Y, Trt, X, opts = list(Trt = list(scale = F, kernel = "linear", subset = 0.5), Non_Trt = list(scale = T, kernel = "linear", subset = 0.3)))
#' # Predicting a good group for new data, target is top 30% observations with highest predicted PTE
#' predict2MSVM(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000)), opts = list(fraction = 0.3))
#'
predict2MSVM = function(m, X, opts = NULL) {
  X = X[, -c(10, 11, 7, 8)]
  X = scale(X)
  X = toNumericTable(X)
  pre = attr(predict(m[[1]], X, probability = TRUE), "probabilities")[,2] - attr(predict(m[[2]], X, probability = TRUE), "probabilities")[,2]
  return(ifelse(pre >= quantile(pre, opts$fraction), 1, 0))
}



#' Predicts if an observation should be treated or not by means difference in predictions between two independent Uplift Random forest (Guelman, 2014) models (Predict function for *trainUpliftModelRF*)
#'
#' @param m a model trained with *trainUpliftModelRF*
#' @param X is the covariate matrix
#' @param opts is a list of options (NOTE: fraction is one available option, it defines percent (from 0 to 1) of total observations with highest predicted PTE, which are then used in obtaining estimates of model performance)
#'
#' @return a binary vector specifying the observations to be treated
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
#' m = trainUpliftModelRF(Y, Trt, X, opts = list(split_method = "Chisq", ntree = 55, bag.fraction = 0.5))
#' # Predicting a good group for new data, target is top 30% observations with highest predicted PTE
#' predictUpliftModelRF(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000)), opts = list(fraction = 0.3))
#'
predictUpliftModelRF = function(m, X, opts = NULL) {
  tbl = predict(m, X)
  pre = tbl[, 1] - tbl[, 2]
  return(ifelse(pre >= quantile(pre, opts$fraction, na.rm = T), 1, 0))
}
