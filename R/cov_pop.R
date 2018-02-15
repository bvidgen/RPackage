#' @title cov_pop
#' @description Calculates the covariance of two vectors using n as the denominator (suitable for populations) rather than n-1 (suitable for samples).
#' @param x a vector of numeric values
#' @param y a vector of numeric values
#' @return the covariance of the inputs is returned.

cov_pop = function(x, y){
  if (!is.numeric(x) || !is.numeric(y))
    stop("input vector must be numeric")
  if(length(x) != length(y))
    stop("'x' and 'y' must be of equal lengths")
  if(length(x) < 3)
    stop("'x' and 'y' must be at least 3 values long")
  
  output = 0
  
  for (i in seq(1,length(x))){
    temp = ( (x[i] - mean(x)) * (y[i] - mean(y)) )
    output = output + temp
  }
  output = output / length(x)
  return(output)
}
