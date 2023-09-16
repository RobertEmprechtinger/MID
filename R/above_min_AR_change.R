#' Calculate area under the curve of a CI that is above a certain treatment effect
#'
#' @param AR_change change of absolute risk
#' @param LCL_AR_change lower confidence limit of AR_change
#' @param min_diff_AR_change minimum important difference of AR change
#' @param AR_control absolute risk of control group
#'
#' @return
#' @export
#'
#' @examples
#' above_min_AR_change(-0.02, -0.03, -0.015, 0.05)
above_min_AR_change <-  function(AR_change, LCL_AR_change, min_diff_AR_change, AR_control,
                                 only_outcome = FALSE, only_graphdata = FALSE, lower_better = TRUE, RR = TRUE){

  # transform to RR
  TE <- RR_from_AR_change(AR_change, AR_control)
  LCL <- RR_from_AR_change(LCL_AR_change, AR_control)
  min_diff <- RR_from_AR_change(min_diff_AR_change, AR_control)


  above_min(TE, LCL, min_diff, only_outcome = only_outcome, only_graphdata = only_graphdata,
            lower_better = TRUE, RR = TRUE)

  # Graph output is currently in RR

}


