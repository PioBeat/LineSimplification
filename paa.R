#' Piecewise Aggregation Approximation
#' 
#' Performs a PPA on a given 2d dataset
#' 
#' @param x dataset with two columns 
#' @param w specify the number of segments for the approximation
#' @param complete.data if T then then the approximation will be returned with the same length as
#' the original data.frame. If F then only one mean value of each segment is returned (the first one)
#' @return a compressed data.frame
paa <- function(x, w = NULL, complete.data = T) {
  # browser()
  N <- nrow(x)
  if(is.null(w))
    w <- ceiling(N / (2^2))
  
  fullvec <- c()
  smallvec <- c()
  ix <- seq(1,w)
  for (i in 1:w) {
    anfang <- (N / w) * (i - 1) + 1
    bis <- N/w * i
    val <- (w/N) * sum(x[anfang:bis, 2])
    fullvec <- rbind(fullvec, data.frame(x = anfang:bis, y = rep(val, N/w))) 
    smallvec <- rbind(smallvec, data.frame(x = anfang, y = val)) 
  }
  if(complete.data) {
    return(fullvec)
  }
  return(smallvec)
}
