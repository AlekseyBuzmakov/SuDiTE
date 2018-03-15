#' Quality of the group as its total treatment effect
#'
#' Returns the quality of the group in terms of its total treatment effect
#'
#' Takes a subgroup and computes its total treatment effect
#'
#' @param subgroup a boolean vector defining a subgroup
#' @param Y a response variable
#' @param Trt a treatment variable
#'
#' @return a total treatment  effect of the subgroup
#' @export
#'
#' @examples
#' # Generating dataset
#' Trt = rbinom(1000,1,0.5)
#' X = data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))
#' Y = ( 2*X$X1 - 1 + X$X2*Trt + rnorm(1000) ) > 0
#' # Generating a random subgroup
#' subgroup = as.logical(rbinom(1000,1,0.7))
#' # The quality
#' subgroupTotalTreatmentEffect( subgroup, Y, Trt )
#'
subgroupTotalTreatmentEffect = function(subgroup, Y, Trt) {
  return( ( sum( Y * Trt * subgroup ) / sum( Trt * subgroup )
            - sum( Y * ( 1 - Trt ) * subgroup )
            / sum( ( 1 - Trt ) * subgroup ) ) * sum( subgroup ) )
}

#' Quality of the group as its average treatment effect
#'
#' Returns the quality of the group in terms of its average treatment effect
#'
#' Takes a subgroup and computes its average treatment effect
#'
#' @param subgroup a boolean vector defining a subgroup
#' @param Y a response variable
#' @param Trt a treatment variable
#'
#' @return a average treatment  effect of the subgroup
#' @export
#'
#' @examples
#' # Generating dataset
#' Trt = rbinom(1000,1,0.5)
#' X = data.frame(X1=rbinom(1000,1,0.6), X2=rnorm(1000), X3=rnorm(1000))
#' Y = ( 2*X$X1 - 1 + X$X2*Trt + rnorm(1000) ) > 0
#' # Generating a random subgroup
#' subgroup = as.logical(rbinom(1000,1,0.7))
#' # The quality
#' subgroupAverageTreatmentEffect( subgroup, Y, Trt )
#'
subgroupAverageTreatmentEffect = function(subgroup, Y, Trt) {
  return( sum( Y * Trt * subgroup ) / sum( Trt * subgroup )
            - sum( Y * ( 1 - Trt ) * subgroup )
            / sum( ( 1 - Trt ) * subgroup ) )
}
NULL
