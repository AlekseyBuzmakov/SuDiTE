% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subgroup-quality.R
\name{subgroupTotalTreatmentEffect}
\alias{subgroupTotalTreatmentEffect}
\title{Quality of the group as its total treatment effect}
\usage{
subgroupTotalTreatmentEffect(subgroup, Y, Trt)
}
\arguments{
\item{subgroup}{a boolean vector defining a subgroup}

\item{Y}{a response variable}

\item{Trt}{a treatment variable}
}
\value{
a total treatment  effect of the subgroup
}
\description{
Returns the quality of the group in terms of its total treatment effect
}
\details{
Takes a subgroup and computes its total treatment effect
}
\examples{
# Generating dataset
Trt = rbinom(1000,1,0.5)
X = data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))
Y = ( 2*X$X1 - 1 + X$X2*Trt + rnorm(1000) ) > 0
# Generating a random subgroup
subgroup = as.logical(rbinom(1000,1,0.7))
# The quality
subgroupTotalTreatmentEffect( subgroup, Y, Trt )

}
