
#' Create a list consisting of a normally distributed, 
#' uniformly distributed dataset and weather data. All datasets 
#' have 40 observations. 
#'
#' They used for the visual and detailed comparison
#'
#' @return a list with 3 data.frams
#' @seealso \code{\link{visualComparison}} and \code{\link{detailedAnalysis}}
generateDataSets <- function() {
  
  if (exists("lineData")) {
    lineData <- get("lineData")
  } else {
    lineData <- list()
  }
  
  x <- seq(1,40)
  
  y <- rnorm(x, 5, 1)
  lineData[[1]] <- list(data = data.frame(x = x, y = y), main = "Normally distributed", eps = NA)  
  
  y <- runif(x, 0, 1)
  lineData[[2]]<- list(data = data.frame(x = x, y = y), main = "Uniformly distributed", eps = NA)
  
  y <- getSummarizedWeather("DRS", "2016-05-23", "2016-07-01")
  lineData[[3]] <- list(data = data.frame(x = x, y = y$Mean_TemperatureC), main ="Weather data", eps = NA)
  
  return(lineData)
}

