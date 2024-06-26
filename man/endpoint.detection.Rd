% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/endpoint.detection.r
\name{endpoint.detection}
\alias{endpoint.detection}
\title{Determines Thermogram Endpoints for Baseline Subtraction}
\usage{
endpoint.detection(
  x,
  w = 90,
  exclusion.lwr = 60,
  exclusion.upr = 80,
  point.selection = "innermost",
  explicit = TRUE
)
}
\arguments{
\item{x}{One thermogram sample}

\item{w}{The number of points in the window. To find the lowest variance, "windows" of w points are selected from lowest temp to exclusion temp and the variance is calculated.}

\item{exclusion.lwr}{The lower bound of the exclusion window}

\item{exclusion.upr}{The upper bound of the exclusion window}

\item{point.selection}{The method of selecting the endpoint. Options are "innermost", "outmost", "mid".}

\item{explicit}{logical: Should text be displayed as the function runs}
}
\value{
Data frame containing the two endpoints for selection and method of selection
}
\description{
Function takes in raw thermogram data and scans from upper or lower temperature to exclusion window boundary.
The variance of each window is calculated and window with the lowest variance is selected.
Endpoint is then selected based on point selection criteria. Innermost selects the point closest to the center of the data set. Outermost selects the most extreme temperature (the point farthest from the center). Mid selects the middle point of the window.
}
\details{
Exclusion zone is set so that the main curve does not get selected as an endpoint. Typically it is set to 60-80 for urine thermograms and 48-82 for plasma thermograms
}
