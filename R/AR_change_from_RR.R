#' Calculates Absolute Risk Reduction from RR
#'
#' @param RR Risk Ratio
#' @param AR_c Absolute Risk from the Control Group
#'
#' @return
#' @export
#'
#' @examples
#' AR_change_from_RR(3, 0.01)
AR_change_from_RR <- function(RR, AR_c){
  absolute_risk_intervention <- AR_c * RR
  AR_change <- absolute_risk_intervention - AR_c
  return(AR_change)
}

