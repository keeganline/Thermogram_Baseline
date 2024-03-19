#' Determines if the thermogram is signal or noise
#'
#' Takes the first difference of the thermogram sample and uses auto.arima to determine if the sample is noise or if it is signal.
#'
#' @param sample One thermogram sample with two columns labeled as Temperature and dCp
#' @return Data frame with result of signal or no signal
#' @export
signal.detection <- function(sample){
  working.sample <- sample %>% select(Temperature, dCp)
  diff.sample <- diff(working.sample$dCp)

  out <- auto.arima(diff.sample)$model$Delta
  detection <- ifelse(length(out)==0, 'No Signal', 'Signal')

  result <- data.frame(Signal = detection)
  return(result)
}
