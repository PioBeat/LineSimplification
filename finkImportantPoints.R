
#' Importand points selection algorithm
#' 
#' @param data 2d data.frame
#' @param w size for the sliding window
#' @param R threshold value for determining significant maximum or minimum value in searched area.
#' @param complete.dataset if F mark the found points as minima and maxima
#' must be > 1
#' @return compressed (approximated) data.frame
finkImportantPoints <- function(data, w = 1, R, complete.dataset = T) {
  # browser()  
  # w <- 3
  importantMin <- c()
  importantMax <- c()
  # a <- seq(1, nrow(data), by = w)
  a <- seq(1, nrow(data), by = 1)
  # a <- head(a, n = -1)
  # for (i in 1:(length(a) - 1)) {#für seq by =w
    for (i in 1:(length(a) - w)) {
    # ix <- a[i]:(a[i+1] - 1) #für seq by =w
    ix <- a[i]:(a[i+w-1])
    data[ix,]
    ix.min <- which.min(data[ix,2])
    ix.max <- which.max(data[ix,2])
    minPoint <- data[ix[ix.min], 2]
    maxPoint <- data[ix[ix.max], 2]
    # browser()
    if(abs(data[ix[1], 2]/minPoint) >= R && abs(data[tail(ix, 1), 2]/minPoint) >=R)
      importantMin <- rbind(importantMin, c(ix[ix.min], data[ix[ix.min], 2]))
    if(abs(maxPoint/data[ix[1], 2]) >= R && abs(maxPoint/data[tail(ix, 1),2]) >=R)
      importantMax <- rbind(importantMax, c(ix[ix.max], data[ix[ix.max], 2]))
    }
  # browser()
  importantMin <- importantMin[!duplicated(importantMin[,1]), ,drop=F]
  importantMax <- importantMax[!duplicated(importantMax[,1]), ,drop=F]
  out <- list(minima = importantMin, maxima = importantMax, firstlast = rbind(data[1,], tail(data,1)))
  if(!complete.dataset) {
    return(out)
  }
  data.compr <- data[c(out$minima[,1], out$maxima[,1], out$firstlast[,1]),]
  return(data.compr)
}




