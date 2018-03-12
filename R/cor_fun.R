#' @title cor_fun
#' @description Calculates either the cross-correlation function for two variables or the auto-correlation function for a single variable. It can be used to calculate values with bounded periodicity (i.e. the values wrap around'). It uses the population level standard deviation; as such, for self-repeating patterns the ACF does not decay over time.
#' @details To autocorrelate a single variable, just pass x. To cross-correlate two variables, pass both x and y.
#' @param x a vector of numeric values.
#' @param y a vector of numeric values #optional input.
#' @param lag.max the total number of lag times that cross-correlation or autocorrelation  will be calculated for.
#' @param wrap determines whether bounded periodicty should be used (wrap=T). Bounded periodicity means that the  values 'wrap round'. Usually, not required.
#' @param level determines whether correlation is calculated using the population-level correlation denominator of 'n' or the sample-level correlation demoninator of 'n-1'. Level only accepts values of 'population' or 'sample'. By default it is set to 'sample'.
#' @return cor.out object is returned. There are nine variables that can be accessed: cor, lag, series, x, y, fit_best, wrap, type, level, conf.int.
#' @export

# NOTES
# http://stats.stackexchange.com/questions/254227/manual-calculation-of-acf-of-a-time-series-in-r-close-but-not-quite
#x = df1$x
#y=df1$y
#lag.max=200
#type = 'CCF'
 

cor_fun = function(x, y=NULL, lag.max=200, wrap=F, level='sample'){
  
  # if y is missing, then make y=x (this shows that the user wants to do autocorrelation)
  # automatically generate the 'type' based on the number of input variables
  if(missing(y)) {y = x
  type = 'ACF'} else{
    type = 'CCF'
  }
  
  # get the names of the variables
  series_x = deparse(substitute(x))
  series_y = deparse(substitute(y))
  
  
  # check variables are correctly formatted
  if (!is.numeric(x) || !is.numeric(y))
    stop("both 'x' and 'y' must be numeric")
  if (length(x) != length(y))
    stop("'x' and 'y' must be of equal lengths")
  if (length(x) < 15)
    stop("there must be at least 15 values in the vectors passed to cor_fun")
  #  if (type != 'CCF' && type != 'ACF')
  #    stop("'type' can only take either 'CCF' or 'ACF'. Please note that by default it takes 'CCF'")
  if (level != 'population' && level != 'sample')
    stop("'level' can only take either 'population' or 'sample'. Please note that by default it takes 'sample'")
  
  # convert x and y to matrixes
  x = as.matrix(x)
  y = as.matrix(y)
  
  # check vector length
  # (I think this code might be redundant now as it is already covered in the code above)
  vector_length = as.integer(nrow(x))
  nser <- as.integer(ncol(x))
  if (is.na(vector_length) || is.na(nser))
    stop("the length of the vectors must be greater than 1")
  
  # check the lag.max value is set appropriately
  lag.max = as.integer(min(lag.max, (vector_length - 20), (vector_length/ 2)))
  #take the smaller value of either number of values in vector (minus 20), the number of values divided by 2 or the lag.max value; so if you put in a huge value it will still only use the correct max number (based on the vector)
  if (is.na(lag.max) || lag.max < 4)
    stop("'lag.max' must be at least 5.")
  
  # calculate confidence interval (for samples only):
  if (level == 'sample'){
    lower_bound = cor.test(x,y)$conf.int[1]
    upper_bound = cor.test(x,y)$conf.int[2]
    conf = data.frame('lower_bound' = lower_bound,
                      'upper_bound' = upper_bound)
  } else {
    conf = 'confidence intervals are not reported for populations'
  }
  
  
  
  ## calculate the cross-correlations
  # the output is the 'result' df, which contains all of the cross-correlation values
  # the 'lag' dg contains the lag timeframes (these correspond to the output in the 'result' df)
  
  
  # wrap == F
  # wrap==F means that there are NO periodic boundary conditions
  if (wrap == F){
    result_pos = rep(NA, lag.max+1) # add 1 as we start from / account for a lag of 0 in the for loop below
    result_neg = rep(NA, lag.max)
    
    if (level == 'population'){
      # for population level correlation, use the cor_pop function
      for (j in seq(0, lag.max)){
        dist = length(y)-j
        #       result_pos[[j+1]] = cor_pop(x[(1+j):length(x)], y[1:dist])
        result_pos[[j+1]] = cor_pop(x[1:dist], y[(1+j):length(y)])}
      
      # for the negative values (e.g. a lag of -1 .. -lag.max), I am lagging the x values against the y -> this makes the most sense to me
      # by keeping y unlagged and moving x 'forward', we get the effect of lagging x backwards.
      # I foud this hard to get my head round initially
      # to test,  write out on paper an x variable going from A to E and a y variable going from F to J + manually 'lag' them.
      for (j in seq(1, lag.max)){
        dist = length(y)-j
        result_neg[[j]] = cor_pop(x[(1+j):length(x)], y[1:dist])}
    }
    
    
    if (level == 'sample') {
      # for sample level correlation, use the cor_sample function
      for (j in seq(0, lag.max)){
        dist = length(y)-j
        result_pos[[j+1]] = cor_sample(x[1:dist], y[(1+j):length(y)])}
      
      for (j in seq(1, lag.max)){
        dist = length(y)-j
        result_neg[[j]] = cor_sample(x[(1+j):length(x)], y[1:dist])}
    }
    
    
    ## Combine the values for negative and positive lags:
    result = c(result_neg, result_pos)
    #print(result)
    names(result) <- paste0("lag", seq.int(-length(result_neg), lag.max))
    result = as.matrix(result)
    
    # create and format the lag matrix:
    lag = as.matrix(seq.int(-length(result_neg), lag.max) )
    dimnames(lag) = dimnames(result)
    
    
    ## Calculate which lag value produces the best fit:
    
    # First, we need to make sure that we take the right value - for ACF we want the second best value (as a lag of 0 will always be 1, i.e. the best - which we don't want returned as that isn't very interesting)
    if (type == 'CCF'){index = 1}
    if (type == 'ACF'){index = 2}
    
    # calculate the best correlation
    result_df = as.data.frame(result)
    cor_value = result_df$V1[order(abs(result_df$V1),decreasing=T)[index]] # uses the abs value -> the value with the greatest magnitude. It should therefore return highly correlated negative values too.
    
    # calculate the best row position (as we now have negative and positive lags, this is not the same as the best lag value)
    row_index = order(abs(result_df$V1),decreasing=T)[index]
    row_index = as.integer(row_index)
    
    # calculate the best lag
    lag_best = row.names(lag)[row_index]
    
    # combine the results
    #best = c(lag_best, cor_value, row_lag)
    best = data.frame('lag_best' = lag_best,
                      'cor_value' = cor_value,
                      'row_index' = row_index)
  }
  
  
  # wrap == T
  # if wrap == T it means that there are periodic boundary conditions i.e. the pattern is self-looping.
  if (wrap == T){
    
    # create a variable for the periodic boundary length (i.e. the length of the vector)
    bound = length(y) #wrap=T doesn't use lag.max as it just uses the full length of y to lag on (so, it is much easier to set this value).
    
    # create an empty df ('result_pos' and 'result_neg') to fill with values:
    result_pos = rep(NA, bound) # do NOT add 1 as in this instance, 1000 is the same as 0 (because it is periodic!)
    # note that we cannot have negative values for a looping pattern; we just 'go round' back to 1000 for the plotting.
    # for 'negative' values, we should just swap the y with the x when we input it.
    
    ## Correlate the wrapped around values, depending on whether level is 'sample' or 'population':
    
    if (level == 'population'){
      
      # calculate the positive lagged value
      for (j in seq(0, (bound-1))){ #because it starts at 0, we only need to go to bound-1 -> the full range of 'bound' values will still be covered
        dist = bound-j
        
        # the formula does not work for j==0 as it adds on extra values, making vectors that are 1001 and 1002 in length
        # so, where j==0 we just set x1 to x and y1 to y (that is, fortunately, where there is no lag we can just use the variables as they are; we don't need to 'wrap them round'.)
        if (j == 0){
          x1 = x
          y1 = y
        } else{
          x1 = x # 'x' just stays stationary and is always the same; it always covers the full range of x values.
          #y1 = c(y[1:dist], y[(dist+1): boun values.
          y1 = y[c((1+j):bound, 1:j)]} # this 'wraps around', by concatenating the lag values with the earlier values.          }
        
        # calculate the correlation values using cor_pop:
        result_pos[[j+1]] = cor_pop(x1,y1)}}
    
    
    if (level == 'sample'){
      
      # calculate the positive lagged value
      for (j in seq(0, (bound-1))){ #because it starts at 0, we only need to go to bound-1 -> the full range of 'bound' values will still be covered
        dist = bound-j
        
        # the formula does not work for j==0 as it adds on extra values, making vectors that are 1001 and 1002 in length
        # so, where j==0 we just set x1 to x and y1 to y (that is, fortunately, where there is no lag we can just use the variables as they are; we don't need to 'wrap them round'.)
        if (j == 0){
          x1 = x
          y1 = y
        } else{
          x1 = x[1: 1000] # 'x' just stays stationary and is always the same; it always covers the full range of x values.
          #y1 = c(y[1:dist], y[(dist+1): boun values.
          y1 = y[c((1+j):bound, 1:j)]} # this 'wraps around', by concatenating the lag values with the earlier values.          }
        
        # calculate the correlation values using cor_sample:
        result_pos[[j+1]] = cor_sample(x1,y1)}}
    
    
    # end of correlation calculations for both negative and positive values (for either samples or populations)
    
    ## Combine the values for negative and positive lags:
    #    result = c(result_neg, result_pos)
    result = result_pos
    names(result) <- paste0("lag", seq.int(0, length(result)-1))
    result = as.matrix(result)
    
    # create and format the lag matrix:
    lag = as.matrix(seq(1,length(result)))
    #lag = as.matrix(seq.int(-length(result_neg), length(result_pos)) )
    dimnames(lag) = dimnames(result)
    
    ## Calculate which lag value produces the best fit:
    # It is more efficient to do this before we repeat the values 10 times.
    
    # First, need to make sure that we take the right value - for ACF we want the second best value (as a lag of 0 will always be 1, i.e. the best - which we don't want returned as that isn't very interesting)
    if (type == 'CCF'){index = 1}
    if (type == 'ACF'){index = 2}
    
    # this code takes the abs value -> the value with the greatest magnitude. It should therefore return highly correlated negative values too.
    # calculate the best correlation
    result_df = as.data.frame(result)
    cor_value = result_df$V1[order(abs(result_df$V1),decreasing=T)[index]]
    # calculate the best row position (as we now have negative and positive lags, this is not the same as the best lag value)
    row_index = order(abs(result_df$V1),decreasing=T)[index]
    row_index = as.integer(row_index)
    # calculate the best lag
    lag_best = row.names(lag)[row_index]
    
    # combine the results
    best = data.frame('lag_best' = lag_best,
                      'cor_value' = cor_value,
                      'row_index' = row_index)
    
    
    ## Finally, make 'result' extra long (repeating 10 times) so multiple plots of the pattern can be produced
    result = as.matrix( rep(result,10) )
  }
  
  # end of main code: for both sample=T and population=T the correlation coefficients for lags have been calculated
  
  
  ## Final code
  # create some additional variables, which can then be passed to the other functions in the package
  # wrap
  if (wrap == T){ wrap1 = 'wrapped' }
  if (wrap == F){ wrap1 = 'no wrap' }
  
  # type
  # No need - can just pass the 'type' object directly to the output.
  
  # prepare the final output
  cor.out <- structure(list(cor = result,
                            lag = lag,
                            fit_best = best,
                            x = x,
                            y = y,
                            wrap = wrap1,
                            type = type,
                            level = level,
                            conf.int = conf),
                       class = "cor.out")
  
  return(cor.out)
}
