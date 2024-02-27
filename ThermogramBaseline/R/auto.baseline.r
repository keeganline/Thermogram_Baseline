#' Automatic Thermogram Baseline Subtraction
#'
#' Automatically detects two endpoints for baseline subtraction, then preforms baseline subtraction and returns final sample interpolated on the given grid
#'
#' @param x Raw thermogram data
#' @param w The number of points in the endpoint selection window
#' @param exclusion.lwr The lower bound of the exclusion window
#' @param exclusion.upr The upper bound of the exclusion window
#' @param grid.temp The set of temperatures to interpolate onto
#' @param plot.on logical: Should graphs be produced?
#' @param point he method of selecting the endpoint. Options are "innermost", "outmost", "mid"
#' @return Data frame with temperature and dCp of baseline subtracted and interpolated sample
#' @export
auto.baseline <- function(x, w = 90, exclusion.lwr = 60, exclusion.upr = 80,
                          grid.temp = seq(45, 90, 0.1), plot.on = FALSE,
                          point = "outmost")
{
  ### automate selection of endpoints
  require(dplyr)
  require(ggplot2)
  endpoints <- endpoint.detection(
    x = x,
    w = w,
    exclusion.lwr = exclusion.lwr,
    exclusion.upr = exclusion.upr,
    point.selection = point)
  ### baseline subtraction with auto-selected upr/lwr points
  baseline.output <- baseline.subtraction.byhand(
    x = x,
    lwr.temp = endpoints$lower,
    upr.temp = endpoints$upper,
    plot.on = plot.on)
  ### generate a final sample on chosen grid!
  final.sample <- final.sample.interpolate(
    x = baseline.output,
    grid.temp = grid.temp,
    plot.on = plot.on)
  ### return the interpolated baseline-subtracted result
  return(final.sample)
}
