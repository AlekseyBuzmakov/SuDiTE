################################ BINARY OUTCOME ################################

############################################################################################################################
################################    1. WEISBERG    #########################################################################
############################################################################################################################

#' Predicts if an observation should be treated or not by means of the model
#'
#' @param m a model trained with trainModelRandomForest
#' @param X is the covariate matrix
#'
#' @return a binary vector specifying the observations to be treated
#' @export
#'
#' @examples
#'
######### Predict Weisberg #########
# predictGLM
predictGLM = function(m, X) {
  XX = toNumericTable(X)

  pre = (exp(predict(m, XX))/(exp(predict(m, XX)) + 1))*2 - 1
  return(ifelse(pre >= quantile(pre, 0.7, na.rm = T), 1, 0))
}


#' Predicts if an observation should be treated or not by means of the model
#'
#' @param m a model trained with trainModelRandomForest
#' @param X is the covariate matrix
#'
#' @return a binary vector specifying the observations to be treated
#' @export
#'
#' @examples
#'
# predictRF
predictRF = function(m, X) {
  XX = toNumericTable(X)

  tbl = predict(m, XX, type = "prob")
  pre = 2*tbl[, 2] - 1
  return(ifelse(pre >= quantile(pre, 0.7, na.rm = T), 1, 0))
}


#' Predicts if an observation should be treated or not by means of the model
#'
#' @param m a model trained with trainModelRandomForest
#' @param X is the covariate matrix
#'
#' @return a binary vector specifying the observations to be treated
#' @export
#'
#' @examples
#'
# predictXGb
predictXGb = function(m, X) {
  XX = toNumericTable(X)

  pre = 2*predict(m, as.matrix(XX)) - 1
  return(ifelse(pre >= quantile(pre, 0.7, na.rm = T), 1, 0))
}


#' Predicts if an observation should be treated or not by means of the model
#'
#' @param m a model trained with trainModelRandomForest
#' @param X is the covariate matrix
#'
#' @return a binary vector specifying the observations to be treated
#' @export
#'
#' @examples
#'
# predictSVM
predictSVM = function(m, X) {
  X = X[,-c(10, 11, 7, 8)]
  XX = toNumericTable(X)

  #return(2*predict(m,XX,probability = TRUE)-1)
  pre = 2*(attr(predict(m, XX, probability = TRUE), "probabilities")[, 2]) - 1
  return(ifelse(pre >= quantile(pre, 0.7, na.rm = T), 1, 0))
}


############################################################################################################################
################################    2. TIAN      ###########################################################################
############################################################################################################################

#' Predicts if an observation should be treated or not by means of the model
#'
#' @param m a model trained with trainModelRandomForest
#' @param X is the covariate matrix
#'
#' @return a binary vector specifying the observations to be treated
#' @export
#'
#' @examples
#'
######### Predict Tian #########
# predictGLM
predictTianGLM = function(m, X) {
  XX = toNumericTable(X/2)

  pre = (exp(predict(m, XX)) - 1)/(exp(predict(m, XX)) + 1)
  return(ifelse(pre >= quantile(pre, 0.7, na.rm = T), 1, 0))
}


############################################################################################################################
################################    3. TWO MODELS    #######################################################################
############################################################################################################################


#' Predicts if an observation should be treated or not by means of the model
#'
#' @param m a model trained with trainModelRandomForest
#' @param X is the covariate matrix
#'
#' @return a binary vector specifying the observations to be treated
#' @export
#'
#' @examples
#'
######### Predict Two models #########
# predict2MGML
predict2MGML = function(m, X) {
  X = toNumericTable(X)
  pre = (exp(predict(m[[1]], X))/(exp(predict(m[[1]], X)) + 1)) - (exp(predict(m[[2]], X))/(exp(predict(m[[2]], X)) + 1))
  return(ifelse(pre >= quantile(pre, 0.7, na.rm = T), 1, 0))
}


#' Predicts if an observation should be treated or not by means of the model
#'
#' @param m a model trained with trainModelRandomForest
#' @param X is the covariate matrix
#'
#' @return a binary vector specifying the observations to be treated
#' @export
#'
#' @examples
#'
# predict2MRF
predict2MRF = function(m, X) {
  X = toNumericTable(X)
  tbl1 = predict(m[[1]], X, type = "prob")
  tbl2 = predict(m[[2]], X, type = "prob")
  pre = tbl1[,2] - tbl2[,2]
  return(ifelse(pre >= quantile(pre, 0.7, na.rm = T), 1, 0))
}


#' Predicts if an observation should be treated or not by means of the model
#'
#' @param m a model trained with trainModelRandomForest
#' @param X is the covariate matrix
#'
#' @return a binary vector specifying the observations to be treated
#' @export
#'
#' @examples
#'
# predict2MXGb
predict2MXGb = function(m, X) {
  X = toNumericTable(X)
  pre = predict(m[[1]], as.matrix(X)) - predict(m[[2]], as.matrix(X))
  return(ifelse(pre >= quantile(pre, 0.7, na.rm = T), 1, 0))
}


#' Predicts if an observation should be treated or not by means of the model
#'
#' @param m a model trained with trainModelRandomForest
#' @param X is the covariate matrix
#'
#' @return a binary vector specifying the observations to be treated
#' @export
#'
#' @examples
#'
# predict2MSVM
predict2MSVM = function(m, X) {
  X = X[, -c(10, 11, 7, 8)]
  X = scale(X)
  X = toNumericTable(X)
  pre = attr(predict(m[[1]], X, probability = TRUE), "probabilities")[,2] - attr(predict(m[[2]], X, probability = TRUE), "probabilities")[,2]
  return(ifelse(pre >= quantile(pre, 0.7), 1, 0))
}


############################################################################################################################
################################    4. UPLIFT MODEL    #####################################################################
############################################################################################################################

#' Predicts if an observation should be treated or not by means of the model
#'
#' @param m a model trained with trainModelRandomForest
#' @param X is the covariate matrix
#'
#' @return a binary vector specifying the observations to be treated
#' @export
#'
#' @examples
#'
######### Predict Uplift model #########
# predictByModelRandomForest
predictUpliftModelRF = function(m, X) {
  tbl = predict(m, X)
  pre = tbl[, 1] - tbl[, 2]
  return(ifelse(pre >= quantile(pre, 0.7, na.rm = T), 1, 0))
}
