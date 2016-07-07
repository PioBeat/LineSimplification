#' Calculate the mean difference of two series
#' 
#' @param dataset a
#' @param dataset b
#' @return single value representing the similarity
meanDiff <- function(a, b) {
  sum(abs(a - b)) / nrow(a)
}

#' Calculate the root mean squared difference of two series
#' 
#' @param dataset a
#' @param dataset b
#' @return single value representing the similarity
rmsDiff <- function(a, b) {
  sqrt(sum((a - b)^2) / nrow(a))
}