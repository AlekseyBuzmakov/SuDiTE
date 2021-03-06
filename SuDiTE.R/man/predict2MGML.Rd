% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Predict.R
\name{predict2MGML}
\alias{predict2MGML}
\title{Predict function for train2MGML}
\usage{
predict2MGML(m, X)
}
\arguments{
\item{m}{a model trained with train2MGML}

\item{X}{is the covariate matrix}
}
\value{
a vector with predicted PTE
}
\description{
Predicts PTE of observations by means difference in predictions between two independent generalized linear models (Predict function for train2MGML)
}
\examples{

# Generating dataset
N = 1000
Trt = rbinom(N,1,0.5)
X = data.frame(X1=rbinom(N,1,0.6), X2=rnorm(N), X3=rnorm(N))
Y = as.numeric( ( 2*X$X1 - 1 + X$X2*Trt + rnorm(N) ) > 0 )
# Fitting model
m = train2MGML(Y, Trt, X, opts = list(alpha = 0, lambda = 0.15))
# Predicting PTE
predict2MGML(m, data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000)))

}
