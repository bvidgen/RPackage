#' @title cor_pop
#' @description Calculates the correlation of two vectors using n as the denominator (suitable for populations) rather than n-1 (suitable for samples).
#' @param x a vector of numeric values
#' @param y a value of numeric values
#' @return the population level correlation of the inputs is returned.

cor_pop = f unction(x, y){
  # note that for large samples the difference between cor_sample() and cor_pop() will be trivially small.
  # it only really matters if n is low (e.g. around 50 values).
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
  output = (output / length(x))
  
  sd_x = 0
  for (i in seq(1,length(x))){
    sd_x = sd_x + ((x[i] - mean(x))^2)
  }
  sd_x = sd_x / (length(x)-1)
  sd_x = sqrt(sd_x)
  
  sd_y = 0
  for (i in seq(1,length(y))){
    sd_y = sd_y + ((y[i] - mean(y))^2)
  }
  sd_y = sd_y / (length(y)-1)
  sd_y = sqrt(sd_y)
  
  output = (output / (sd_y * sd_x))
  
  return(output)
}