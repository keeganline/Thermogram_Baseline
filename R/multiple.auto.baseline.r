#' Interpolates multiple thermograms
#'
#' Takes in a data frame of multiple thermograms and returns a baseline subtracted data frame of clean thermograms
#'
#' @param x Data frame of raw thermogram data. Must be filtered to desired grid beforehand
#' @param w The number of points in the window. To find the lowest variance, "windows" of w points are selected from lowest temp to exclusion temp and the variance is calculated.
#' @param exclusion.lwr The lower bound of the exclusion window
#' @param exclusion.upr The upper bound of the exclusion window
#' @param grid.temp The grid of temperatures the samples are interpolated onto
#' @param point The method of selecting the endpoint. Options are "innermost", "outmost", "mid".
#' @param explicit logical: Should text be displayed as the function runs
#' @param file.on logical: Should a csv file be saved of the output
#' @return Data frame of temperature and dCp for multiple thermograms.
#' @export
multiple.auto.baseline <- function(x,w=90,exclusion.lwr = 60, exclusion.upr = 80,
                                            grid.temp = seq(45, 90, 0.1),
                                            point = "outmost", explicit = TRUE, file.on = FALSE)
{
  require(tidyverse, quietly = TRUE)
  ### Isolate unique sample IDs
  SampleIDs <- x %>% pull(SampleID) %>% unique() %>% as.vector()

  ### Set empty data frame to hold results
  Final.Results <- data.frame(Temperature = grid.temp)

  ### Number of thermograms to run
  n.samples <- length(SampleIDs)

  ### Run each sample
  for(j in 1:n.samples)
  {
    if(explicit == TRUE){
      cat('Working on Sample ', SampleIDs[j], 'element ', j,' of', n.samples,' \n')
    }
    ### select a sample
    working.sample <- x %>%
      filter(SampleID == SampleIDs[j]) %>%
      select(Temperature, dCp)
    ### get a baseline-subtracted and interpolated final result!
    auto.output <- auto.baseline(x = working.sample, w=w, exclusion.lwr = exclusion.lwr,
                                 exclusion.upr = exclusion.upr,plot.on = FALSE,
                                 point = point, grid.temp = grid.temp, explicit = explicit)
    Final.Results <- Final.Results %>% cbind(out = auto.output$dCp)
    if(explicit == TRUE){
      cat("\014")
    }
  }
  colnames(Final.Results)[-1] <- SampleIDs

  if(file.on == TRUE){
    write.csv(Final.Results,file="Final.Thermogram.Data.csv")
  }
 return(Final.Results)
}
