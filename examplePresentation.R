showImportantPointsExample <- function(data) {
  R <- c(1, 1.2, 1.4, 1.6)
  
  layout(mat = matrix(c(1,2,3,4), ncol=2, byrow=T))
  for (r in R) {
    out <- finkImportantPoints(data = data, w = floor(sqrt(nrow(data)-1)), R = r, complete.dataset = F) 
  
    plot(data, type="l", main=paste0("R = ", r))  
    points(out$minima, col="green",pch=19, cex=1.2)
    points(out$maxima, col="red",pch=19, cex=1.2)
  }
}

showPAAExample <- function(data) {
  W <- c(5, 10, 15, 20)
  layout(mat = matrix(c(1,2,3,4), ncol=2, byrow=T))
  for (w in W) {
    out <- paa(x = data, w = w, complete.data = T) 
    plot(data, type="l", main=paste0("w = ", w))  
    lines(out, lwd=2, col="red", type="l")
  }
}



