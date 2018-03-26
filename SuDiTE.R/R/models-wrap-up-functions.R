#' Uplift Random Forest by Guelman et al.
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
#' # Generating dataset
#' Trt = rbinom(1000,1,0.5)
#' X = data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(1000) ) > 0 )
#' # Training
#' m = trainModelRandomForest(Y,Trt,X)
#' # Predicting a good group for new data
#' sum(predictByModelRandomForest(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))))
#'
trainModelRandomForest=function(Y, Trt, X, opts) {
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
#'
#' # Generating dataset
#' Trt = rbinom(1000,1,0.5)
#' X = data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(1000) ) > 0 )
#' # Training
#' m = trainModelRandomForest(Y,Trt,X)
#' # Predicting a good group for new data
#' sum(predictByModelRandomForest(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))))
#'
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
  for( col in colnames(d) ) {
    if( is.factor(d[[col]]) ) {
      d[, c(paste0(col,".",levels(d[[col]]))[-1], col) :=
          c(lapply(levels(d[[col]])[-1], function(x) as.integer(x == d[[col]])), .(NULL))]
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
#' @param opts is a list of options
#'
#' @return a model that is able to classify clients into treatment and control group
#' @export
#'
#' @examples
#'
#' # Generating dataset
#' Trt = rbinom(1000,1,0.5)
#' X = data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(1000) ) > 0 )
#' # Training
#' m = trainModelModLM(Y,Trt,X)
#' # Predicting a good group for new data
#' sum(predictByModelModLM(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))))
#'
trainModelModLM=function(Y,Trt,X, opts) {
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
#'
#' # Generating dataset
#' Trt = rbinom(1000,1,0.5)
#' X = data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))
#' Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(1000) ) > 0 )
#' # Training
#' m = trainModelModLM(Y,Trt,X)
#' # Predicting a good group for new data
#' sum(predictByModelModLM(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))))
#'
predictByModelModLM=function(m, X) {
  X=toNumericTable(X)
  return(predict(m,X) > 0)
}
