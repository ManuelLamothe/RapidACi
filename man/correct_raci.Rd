% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correct_raci_fct.r
\name{correct_raci}
\alias{correct_raci}
\title{Correct_raci function}
\usage{
correct_raci(data, curve)
}
\arguments{
\item{data}{The umpteenth element of the list under construction}

\item{curve}{Portion of the curve to use when there are both up (increasing [CO2]) and
down (decreasing [CO2]) ramps (either "positive" or "negative")}
}
\value{
The correction factor used, the A corrected (Aleaf) and the Ci corrected
}
\description{
Internal function to correct A and Ci using the non-orthogonal
  coefficients obtained from the polynomial fitted to the matching empty chamber
  measurements.
}
