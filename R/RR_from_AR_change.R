#' Calculates Risk Reduction from ARR
#'
#' @param ARR Risk Ratio
#' @param RR Absolute Risk from the Control Group
#'
#' @return
#' @export
#'
#' @examples
#' RR_from_AR_change(-0.005, 0.01)
RR_from_AR_change <- function(AR_change, AR_c){
  absolute_risk_intervention <- AR_change + AR_c
  if(absolute_risk_intervention < 0){
    stop("Absolute risk reduction is greater than absolute risk in control group")
  }

  RR <- absolute_risk_intervention / AR_c
  return(RR)
}
