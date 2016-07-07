visualComparison <- function(lineData, method = "ip") {

  # browser()
  old <- par(mfrow=c(3,1))
  cnt <- 1
  for (each in lineData) {
    data <- each$data
    plot(data, type="l", lwd=2, main=each$main)
    # browser()
    data.compr <- list()
    
    data.compr$a <- finkImportantPoints(data = data, w = floor(sqrt(nrow(data))), R = 1.2, complete.dataset = T)
    if(method == "paa") {
      data.compr$a <- paa(data, w = nrow(data.compr$a), complete.data = F)
    }
    
    Np <- nrow(data.compr$a) #+ 1
    eps <- 0.1
    # only calculate eps if not computed before
    # if(is.na(lineData[[cnt]]$eps)) {
      while(T) {
        data.compr$b <- rdp(data, eps)
        Np <- nrow(data.compr$b)
        eps <- eps + 0.1
        if(Np <= nrow(data.compr$a)) break;
      }
      lineData[[cnt]]$eps <- eps
      # browser()
    # } else {
      # data.compr$b <- rdp(data, lineData[[cnt]]$eps)
    # }
    
    data.approx <- list()
    data.approx$a <- approx(data.compr$a[,1], data.compr$a[,2], n = nrow(data), method="linear")
    data.approx$b <- approx(data.compr$b[,1], data.compr$b[,2], n = nrow(data), method="linear")
    
    points(data.compr$a, col="blue",pch=19, cex=2)
    points(data.compr$b, col="red",pch=19, cex=1.2)
    lines(data.approx$a$x, data.approx$a$y, col="blue")
    lines(data.approx$b$x, data.approx$b$y, col="red")
    
    cnt <- cnt + 1
  }
  par(old)
  return(lineData)
}




detailedAnalysis <- function(lineData) {
# browser()
  result <- c()
  
  for (each in lineData) {
    e.ipa <- finkImportantPoints(data = each$data, R = 1.2, w = floor(sqrt(nrow(data))))
    e.paa <- paa(x = each$data, w = nrow(e.ipa), complete.data = F)
    e.rdp <- rdp(x = each$data, eps = each$eps)
    
    a <- list()
    a[[1]] <- approx(e.ipa[,1], e.ipa[,2], n = nrow(each$data), method="linear")
    a[[2]] <- approx(e.paa[,1], e.paa[,2], n = nrow(each$data), method="linear")
    a[[3]] <- approx(e.rdp[,1], e.rdp[,2], n = nrow(each$data), method="linear")
  
    meanDiff <- sapply(1:length(a), function(i) {
      meanDiff(each$data, a[[i]])
    })
    rmsDiff <- sapply(1:length(a), function(i) {
      rmsDiff(each$data, a[[i]])
    })  
    
    result <- rbind(result, c(meanDiff, rmsDiff))
  }
  
  rownames(result) <- sapply(lineData, function(x) {x$main})
  colnames(result) <- rep(c("IP", "PAA", "RDP"), 2)
  result
  
}

# result <- detailedAnalysis(lineData = lineData)
# Vergleich mit important points method und line simplification

#performs multiple tests on artifically generated datasets
multiAnalysis <- function(N = 20) {
  # browser()
  
  meanDiffs <- c()
  rmsDiffs <- c()
  x <- 1:40
  for (i in 1:N) {
    data <- data.frame(x = x, y = rnorm(x, 0, 1))
    data.ips <- finkImportantPoints(data, R = 1.2, w = floor(sqrt(nrow(data))))
    
    Np <- nrow(data.ips) #+ 1
    eps <- 0.1
    while(T) {
      data.rdp <- rdp(data, eps)
      Np <- nrow(data.rdp)
      eps <- eps + 0.1
      if(Np <= nrow(data.ips)) break;
    }
    
    a <- list()
    a[[1]] <- approx(data.ips[,1], data.ips[,2], n = nrow(data), method="linear")
    a[[2]] <- approx(data.rdp[,1], data.rdp[,2], n = nrow(data), method="linear")
    
    meanDiff <- sapply(1:length(a), function(i) {
      meanDiff(data, a[[i]])
    })
    rmsDiff <- sapply(1:length(a), function(i) {
      rmsDiff(data, a[[i]])
    })  
    meanDiffs <- rbind(meanDiffs, c(meanDiff))
    rmsDiffs <- rbind(rmsDiffs, c(rmsDiff))
    print(paste0("Dataset ", i, "/", N))
  }
  # browser()
  return(list(meanDiffs = colMeans(meanDiffs), rmsDiffs = colMeans(rmsDiffs)))
}






