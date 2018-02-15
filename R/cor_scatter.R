#' @title cor_scatter
#' @description Plots separate scatter plots in a lattice for several lag values
#' @param x a vector of numeric values
#' @param y a vector of numeric values
#' @param lags a list containing the values to be lagged
#' @return a lattice of scatter plots.
#' @export

cor_scatter = function(x, y=NULL, lags){
  # if y is missing, then make y=x (this shows that the user wants to do autocorrelation)
  if(missing(y)) {y = x
  type = 'ACF'} else {
    type = 'CCF' }
  
  if (!is.list(lags))
    stop("'lags' must be formatted as a list: e.g. lags = list(10,20,30,40,50)")
  if (length(lags) < 3)
    stop("'lags' must include at least 3 values")
  max_lag = max(unlist(lags))
  if (max_lag > (length(x)-2))
    stop("all values within 'lags' must be smaller (by at least 2) than the length of 'x'")
  
  # get the names of the variables:
  series_x = deparse(substitute(x))
  series_y = deparse(substitute(y))
  
  # check the format of the variables:
  if (!is.numeric(x) || !is.numeric(y))
    stop("both 'x' and 'y' must be numeric")
  if (length(x) != length(y))
    stop("'x' and 'y' must be of equal lengths")
  if (length(x) < 5)
    stop("'x' must be at least 5 values long")
  
  x = as.matrix(x)
  y = as.matrix(y)
  df = as.data.frame(cbind(x,y))
  
  vector_length <- as.integer(nrow(x))
  nser <- as.integer(ncol(x))
  if (is.na(vector_length) || is.na(nser))
    stop("'vector_length' must be integer")
  
  result = replicate(length(lags), data.frame()) #initialise a list of empty dataframes (n= length of the lags list)
  
  # I have chosen NOTE to account for whether the output is wrapped or not
  for (j in seq(1, length(lags))){
    lag_len = lags[[j]]
    total = nrow(df)
    start = df[1:(total-lag_len), 1] # x value
    end = df[(1 + lag_len):total, 2] # y value
    df_temp = as.data.frame(cbind(start,end))
    title = title = paste('time lag of ', lag_len)
    plot = ggplot(df_temp, aes(start,end)) + geom_point() + ggtitle(title) +
      geom_smooth(method = "lm", se = FALSE, col='red', size=0.3)
    result[[j]] = plot
  }
  
  # http://stackoverflow.com/questions/10706753/how-do-i-arrange-a-variable-list-of-plots-using-grid-arrange
  n <- length(result)
  nCol <- floor(sqrt(n))
  do.call("grid.arrange", c(result, ncol=nCol))
  
}