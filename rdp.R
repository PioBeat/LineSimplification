
#' Perpendicular Distance between a point and a line
#' 
#' @param x 2d point in space 
#' @param p1 Start point of the line
#' @param p2 End point of the line
#' @return single value of the perpendicular distance from \code{x} and the line
perpDist <- function(x, p1, p2) {
#   d <- abs((p2[2] - p1[2])*x[1] - 
#         (p2[1] - p1[1])*x[2] +
#           p2[1]*p1[2] -
#           p2[2]*p1[1]) / 
#     sqrt((p2[2] - p1[2])^2 + (p2[1] - p1[1])^2)
  
  n = abs((p2[1,1] - p1[1,1]) * (p1[1,2] - x[1,2]) - (p1[1,1] - x[1,1]) * (p2[1,2] - p1[1,2]))
  d = sqrt((p2[1,1] - p1[1,1]) * (p2[1,1] - p1[1,1]) + (p2[1,2] - p1[1,2]) * (p2[1,2] - p1[1,2]))
  return(n / d)
  
}

#' Ramer–Douglas–Peucker algorithm
#' 
#' @param x 2d data.frame
#' @param eps threshold value for the maximum distance
#' @return compressed (approximated) data.frame
rdp <- function(x, eps) {
  dmax <- 0
  index <- 0
  ende <- nrow(x)
  # browser()
  #Look for the point with the biggest distance from the whole currentline
  for( i in 2:(ende)) {
    d <- perpDist(x[i, ], x[1, ], x[ende, ])
    if(d > dmax) {
      index <- i
      dmax <- d
    }
  }
  # points(index, x[index], col="red")
  resultList <- c()
  if(dmax > eps) {
    results1 <- rdp(x[1:index, ], eps)
    results2 <- rdp(x[index:ende, ], eps)
    resultList <- c()
    resultList <- rbind(resultList, results1[1:(nrow(results1)-1), ], results2[1:nrow(results2), ])
  } else {
    resultList <- data.frame(x = c(x[1,1], x[ende, 1]),
                    y = c(x[1, 2], x[ende, 2]))
  }
  return(resultList)
}

