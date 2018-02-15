#' @title cor_sample
#' @description Calculates the correlation of two vectors using n-1 as the denominator (suitable for samples), rather than n (suitable for populations).
#' @param x a vector of numeric values
#' @param y a value of numeric values
#' @return the sample level correlation of the inputs is returned.


cor_sample = function(x, y){
  if (!is.numeric(x) || !is.numeric(y))
    stop("input vector must be numeric")
  if(length(x) != length(y))
    stop("'x' and 'y' must be of equal lengths")
  if(length(x) < 3)
    stop("'x' and 'y' must contain at least 3 values")
  
  output = 0
  for (i in seq(1,length(x))){
    temp = ( (x[i] - mean(x)) * (y[i] - mean(y)) )
    output = output + temp }
  output = (output / (length(x)-1))
  # we use n-1 as otherwise we have a biased population estimate; it is biased towards under-measuring the correlation
  # this seemed to me counterintuitive... see here:
  # http://stats.stackexchange.com/questions/166568/why-do-we-divide-by-n-1-when-calculating-sample-correlation
  
  sd_x = 0
  for (i in seq(1,length(x))){
    sd_x = sd_x + ((x[i] - mean(x))^2)
  }
  sd_x = sd_x / (length(x) -1)
  sd_x = sqrt(sd_x)
  
  sd_y = 0
  for (i in seq(1,length(y))){
    sd_y = sd_y + ((y[i] - mean(y))^2)
  }
  sd_y = sd_y / (length(y) -1)
  sd_y = sqrt(sd_y)
  
  output = output / (sd_y * sd_x)
  
  return(output)
}




