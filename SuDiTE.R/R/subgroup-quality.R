#' Returns the quality of the group in terms of its average treatment effect
#'
#' Returns the quality of the group in terms of its average treatment effect
#'
#' Takes a subgroup and computes its average treatment effect
#'
#' @param subgroup a boolean vector defining a subgroup 
#' @param Y a response variable
#' @param Trt a treatment variable
#' @return an average treatment  effect of the subgroup 
#' @export
#' @examples
#'
#'
subgroupAverageTreatmentEffect = function(subgroup, Y, Trt) {
  return( ( sum( Y * Trt * subgroup ) / sum( Trt * subgroup )
            - sum( Y * ( 1 - Trt ) * subgroup )
            / sum( ( 1 - Trt ) * subgroup ) ) * sum( subgroup ) )
}
NULL