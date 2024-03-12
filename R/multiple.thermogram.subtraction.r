#' Intepolates multiple thermograms
#'
#' Fits a spline to the baseline subtracted data and interpolates the thermogram on the given temperature grid.
#'
#' @param x Data frame of raw thermogram data. Must be filtered to desired grid beforehand
#' @param grid.temp The grid of temperatures the samples are interpolated onto
#' @param plot.on logical: should the output be graphed?
#' @return Data frame of temperature and dCp
#' @export
multiple.thermogram.subtraction <- function(x,w=90,exclusion.lwr = 60, exclusion.upr = 80,
                                            grid.temp = seq(45, 90, 0.1), plot.on = FALSE,
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
    write.csv(Final.Results)
  }
 return(Final.Results)
}
