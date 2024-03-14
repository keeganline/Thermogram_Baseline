#' Determines Thermogram Endpoints for Baseline Subtraction
#'
#' Function takes in raw thermogram data and scans from upper or lower temperature to exclusion window boundary.
#' The variance of each window is calculated and window with the lowest variance is selected.
#' Endpoint is then selected based on point selection criteria. Innermost selects the point closest to the center of the data set. Outermost selects the most extreme temperature (the point farthest from the center). Mid selects the middle point of the window.
#'
#' Exclusion zone is set so that the main curve does not get selected as an endpoint. Typically it is set to 60-80 for urine thermograms and 48-82 for plasma thermograms
#'
#' @param x One thermogram sample
#' @param w The number of points in the window
#' @param exclusion.lwr The lower bound of the exclusion window
#' @param exclusion.upr The upper bound of the exclusion window
#' @param point.selection The method of selecting the endpoint. Options are "innermost", "outmost", "mid".
#' @return Data frame containing the two endpoints for selection and method of selection
#' @export
endpoint.detection <- function(x, w = 90, exclusion.lwr = 60, exclusion.upr = 80, point.selection = "outmost", explicit = TRUE)
{
  require(tidyverse,quietly = TRUE)
  if(point.selection == "outmost"){
    k <- w
  }
  if(point.selection == "innermost"){
    k <- 0
  }
  if(point.selection == "mid"){
    k <- round(w/2)
  }

  try(if(min(x$Temperature) > exclusion.lwr) stop("Exclusion zone is too low"))
  try(if(max(x$Temperature) < exclusion.upr) stop("Exclusion zone is too high"))
  try(if(nrow(x %>% filter(Temperature < exclusion.lwr)) < w) stop("Not enough points. Need to lower W"))
  try(if(nrow(x %>% filter(Temperature > exclusion.upr)) < w) stop("Not enough points. Need to lower W"))

  ### fit a CV spline
  full.spline.fit <- smooth.spline(x$Temperature, x$dCp, cv=TRUE)
  ### spline residuals into a data.frame with ids for tracking.
  r <- resid(full.spline.fit)
  r.df <- data.frame(Temperature = x$Temperature,
                     r=r,
                     id = 1:length(x$Temperature))
  ### scan through lower region calculating variance within a window size of w
  if(explicit == TRUE){
    cat('Scanning Lower. \n')
  }
  i=0
  df.var <- data.frame()
  ### how far do we need to scan?
  points.in.lower <- nrow(x %>% filter(Temperature < exclusion.lwr))
  ### scan and calculate variance for each window
  while ((w+i) < points.in.lower){
    rout <- r.df %>% slice((1+i):(w+i)) %>% summarise(temp.stop=(w+i),mean = mean(r), sd=sd(r))
    df.var <- rbind(df.var, rout)
    i=i+1
  }
  ### where was the minimum variance?
  low <- df.var %>% filter(sd == min(sd))
  lower <- x$Temperature[low$temp.stop-k+1]

  ### total size of thermogram
  if(explicit == TRUE){
    cat('Scanning Upper. \n')
  }
  l <- length(x$Temperature)
  j=0
  df.var.upper <- data.frame()
  ### how far from upper endpoint do we need to scan?
  smallest.point.in.upr <- nrow(x) - nrow(x %>% filter(Temperature > exclusion.upr))
  ### scan from highest point to region of exclusion
  while ((l-w-j) > smallest.point.in.upr) {
    rout <- r.df %>% slice((l-w-j):(l-j)) %>% summarise(i=(l-w-j),mean = mean(r), sd=sd(r))
    df.var.upper <- rbind(df.var.upper, rout)
    j=j+1
  }
  ### where was the minimum variance?
  up <- df.var.upper %>% filter(sd == min(sd))
  upper <- x$Temperature[up$i+k]

  output <- data.frame(lower = lower, upper = upper, method = point.selection)
  return(output)
}
