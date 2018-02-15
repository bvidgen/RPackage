#' @title sd_pop
#' @description Calculates the standard deviation of a vector using n as the denominator (suitable for populations) rather than n-1 (suitable for samples).
#' @param x a vector of numeric values
#' @return the standard deviation of the input is returned.

sd_pop = function(x){
  if (!is.numeric(x))
    stop("input vector must be numeric")
  
  sd_output = 0
  for (i in seq(1,length(x))){
    sd_output = sd_output + ((x[i] - mean(x))^2)
  }
  sd_output = sd_output / length(x)
  sd_output = sqrt(sd_output)
  return(sd_output)
}

