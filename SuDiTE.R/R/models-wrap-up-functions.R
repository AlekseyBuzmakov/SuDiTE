#' Uplift Random Forest by Guelman et al.
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#'
#' @return a model that is able to classify clients into treatment and control group
#' @export
#'
#' @examples
trainModelRandomForest=function(Y,Trt,X) {
  require(uplift)
  return( upliftRF(X, Y, Trt,split_method = "Chisq") )
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
predictByModelRandomForest=function(m, X) {
  tbl=predict(m,X)
  return( (tbl[,1]-tbl[,2]) > 0)
}

#' Transformations of factors to logical variables
#'
#' @param X is the covariate matrix
#'
#' @return returns a numeric-only covariate matrix
toNumericTable=function(X) {
  require(data.table)
  d=data.table(X)
  for( c in colnames(d) ) {
    if( is.factor(d[[c]]) ) {
      d[, c(paste0(c,".",levels(d[[c]]))[-1], c) :=
          c(lapply(levels(d[[c]])[-1], function(x) as.integer(x == d[[c]])), .(NULL))]
    }
  }
  return(as.data.frame(d))
}

#' Covariate transformation as defined by Tian et al., 2014
#'
#' @param Trt the binary treatment varibale
#' @param X the covariates
#'
#' @return the modified covariate matrix
modifyDataByTian=function(Trt,X) {
  X=toNumericTable(X)
  return(X*Trt/2)
}

#' Model by Tian et al 2014
#'
#' @param Y is the binary response varibale
#' @param Trt is the binary treatment variable
#' @param X is the numeric covariates matrix
#'
#' @return a model that is able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
trainModelModLM=function(Y,Trt,X) {
  stopifnot(sum(Y==1)+sum(Y==0) == length(Y))
  p=sum(Trt)/length(Trt)
  d=modifyDataByTian(2*Trt-1,X)
  d$Y=Y
  return( glm(Y~0+., data=d,weights = Trt*(1-p)+(1-Trt)*p, family = quasibinomial(link="logit") ) )
}
#' Predicts if an observation should be treated or not by means of the model trained with trainModelModLM
#'
#' @param m a model trained with trainModelModLM
#' @param X is the covariate matrix
#'
#' @return a binary vector specifying the observations to be treated
#' @export
#'
#' @examples
predictByModelModLM=function(m, X) {
  X=toNumericTable(X)
  return(predict(m,X) > 0)
}
