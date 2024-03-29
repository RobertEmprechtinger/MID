% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/above_min_AR_change.R
\name{above_min_AR_change}
\alias{above_min_AR_change}
\title{Calculate area under the curve of a CI that is above a certain treatment effect}
\usage{
above_min_AR_change(
  AR_change,
  LCL_AR_change,
  min_diff_AR_change,
  AR_control,
  only_outcome = FALSE,
  only_graphdata = FALSE,
  lower_better = TRUE,
  RR = TRUE
)
}
\arguments{
\item{AR_change}{change of absolute risk}

\item{LCL_AR_change}{lower confidence limit of AR_change}

\item{min_diff_AR_change}{minimum important difference of AR change}

\item{AR_control}{absolute risk of control group}
}
\description{
Calculate area under the curve of a CI that is above a certain treatment effect
}
\examples{
above_min_AR_change(-0.02, -0.03, -0.015, 0.05)
}
