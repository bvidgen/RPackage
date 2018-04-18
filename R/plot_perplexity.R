#' @title plot_perplexity
#' @description Plots the perplexity values for K, alpha and beta, showing each fold as a point and the average as a line
#' @param data.frame Dataframe. Perplexity values returned by fit.topics.perplexity(), fit.alpha.perplexity() and fit.beta.perplexity() 
#' @param value.write Character. What perplexity is calculated for; either 'K', 'Alpha' or 'Beta' 
#' @param x.intercept.write Numeric. Optional parameter: value of x-intercept to plot a vertical dotted red line showing chosen value 
#' @param logx Logical. Whether to plot the x axis logarithmically (often useful for Beta and Alpha plots)
#' @return ggplot2 plot showing the values of perplexity (both each fold and the average)
#' @export


plot_perplexity = function(data.frame, value.write, xintercept.write, logx){
  
  if(missing(data.frame)){
    stop('data.frame is a required parameter')
  }
  if(missing(value.write)){
    stop('value.write is a required parameter')
  }
  
  if(missing(xintercept.write)){
    xintercept.write = NULL
  }
  
  if(missing(logx)){
    logx = F
  }
  
  # Check formatting of data
  if(class(data.frame) != 'data.frame'){
    stop('data frame must be of class data.frame (not a data.table or matrix). Pass the output from the fit.perplexity functions directly! Check the class by using class()')
  }
  
  if(class(value.write) != 'character'){
    stop('value.write frame must be of class character (e.g. "K" or "Alpha"). Check the class by using class()')
  }
  
  if(class(logx) != 'logical'){
    stop('logx frame must be of class logical (e.g. F or T). Check the class by using class()')
  }
  
  if(class(xintercept.write) != 'numeric' && class(xintercept.write) != 'NULL'){
    stop('xintercept.write must be of class numeric. Check the class by using class()')
  }  
  
  # Check for missing values in the dataframe
  if(any(sapply(data.frame, function(x){
    is.nan(x)
  }))){
    stop('Your dataframe contains NaN values. This might be because you used too many folds')
  }
  
  if(any(sapply(data.frame, function(x){
    is.na(x)
  }))){
    stop('Your dataframe contains NA values. This might be because you used too many folds')
  }
  
  
  # Make data long rather than wide
  data.frame.plot = data.frame %>%
    tidyr::gather(value, perplexity)
  # check formatting
  data.frame.plot$value = as.numeric(data.frame.plot$value)
  data.frame.plot$perplexity = as.numeric(data.frame.plot$perplexity)
  
  # Calculate averges
  data.frame.plot.average = as.data.frame(colMeans(data.frame))
  data.frame.plot.average$value = rownames(data.frame.plot.average)
  colnames(data.frame.plot.average) = c('perplexity', 'value')
  # check formatting
  data.frame.plot.average$value = as.numeric(data.frame.plot.average$value)
  data.frame.plot.average$perplexity = as.numeric(data.frame.plot.average$perplexity)
  
  # Plot results
  if(missing(xintercept.write) & logx == F){
    p.out = ggplot() + 
      geom_point(data = data.frame.plot, aes(value, perplexity)) + # scatter plot of values, calculated from each of the k-folds testing (for each value of K we get the number of dots which we set 'folds' equal to)
      geom_line(data = data.frame.plot.average, aes(value, perplexity)) + # line of values, calculated as an average from the k-folds testing
      ylab('Perplexity') +
      xlab(value.write) +
      ggtitle(paste0('Perplexity vs. ', value.write)) }
  
  if(missing(xintercept.write) & logx == T){
    p.out = ggplot() + 
      geom_point(data = data.frame.plot, aes(value, perplexity)) + # scatter plot of values, calculated from each of the k-folds testing (for each value of K we get the number of dots which we set 'folds' equal to)
      geom_line(data = data.frame.plot.average, aes(value, perplexity)) + # line of values, calculated as an average from the k-folds testing
      ylab('Perplexity') +
      xlab(value.write) +
      ggtitle(paste0('Perplexity vs. ', value.write)) +
      scale_x_log10() }
  
  if(!(missing(xintercept.write)) & logx == F){
    p.out = ggplot() + 
      geom_point(data = data.frame.plot, aes(value, perplexity)) + # scatter plot of values, calculated from each of the k-folds testing (for each value of K we get the number of dots which we set 'folds' equal to)
      geom_line(data = data.frame.plot.average, aes(value, perplexity)) + # line of values, calculated as an average from the k-folds testing
      ylab('Perplexity') +
      xlab(value.write) +
      ggtitle(paste0('Perplexity vs. ', value.write)) +
      geom_vline(xintercept = xintercept.write, linetype = 2, color = 'red')}
  
  if(!(missing(xintercept.write)) & logx == T){
    p.out = ggplot() + 
      geom_point(data = data.frame.plot, aes(value, perplexity)) + # scatter plot of values, calculated from each of the k-folds testing (for each value of K we get the number of dots which we set 'folds' equal to)
      geom_line(data = data.frame.plot.average, aes(value, perplexity)) + # line of values, calculated as an average from the k-folds testing
      ylab('Perplexity') +
      xlab(value.write) +
      ggtitle(paste0('Perplexity vs. ', value.write)) +
      geom_vline(xintercept = xintercept.write, linetype = 2, color = 'red') +
      scale_x_log10()} 
  
  return(p.out)
}