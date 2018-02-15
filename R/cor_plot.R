#' @title cor_plot
#' @description Plots the cor_fun output object using ggplot2. It is designed to only be used in conjunction with the output of the cor_fun() function ('cor.out'). The dotted red lines (shown only for samples), are 95% confidence intervals calculated from the original correlation.
#' @param cor.out a cor object, as created by the cor_fun().
#' @param results sets the number of total values that will be plotted. By default it is set to the number of cor values. Also, the maximum number of values that can be plotted is 1000.
#' @param graph sets the graphic type. Can take the values 'line' or 'bar'. By default is is set to 'bar'.
#' @return a plot of cross-correlated values is returned.
#' @export


cor_plot = function(cor.out, results=length(cor.out$cor), graph = 'bar'){
  if (!'ggplot2' %in% installed.packages())
    stop("ggplot2 must be installed and loaded to continue")
  if(!"package:ggplot2" %in% search())
    library(ggplot2)
  if (class(cor.out) != "cor.out"){
    stop("input must be an object of type \"cor.out\", as produced by the cor_fun() function.")}
  if (results > length(cor.out$cor)){
    stop("the length of 'results' cannot be more than the number of correlated values. Check:: length(cor.out$cor)")}
  if (graph != 'line' && graph != 'bar'){
    stop("'type' can only take either 'line' or 'bar'. Please note that by default it takes 'bar'")}
  
  if (cor.out$wrap == 'no wrap'){
    if (results > 1000){
      results = 1000
      print('if there is no wrap, the maximum number of values that can be printed is 1000')}}
  if (cor.out$wrap == 'wrapped'){
    if (results > 5000){
      results = 5000
      print('if there is a wrap, the maximum number of values that can be printed is 5000')}}
  
  ### Check input variables
  type = cor.out$type
  wrap = cor.out$wrap
  level = cor.out$level
  
  if (level == 'sample'){
    conf = cor.out$conf.int
    upper_bound = conf$upper_bound
    lower_bound = conf$lower_bound
  }
  
  ## CCF input
  # make sure 'ccf' and 'lag' variables are formatted correctly (remembering that if wrap=T was used, then the length of ccf will be far larger than lag )
  # check length of ccf variable
  if (results < length(cor.out$cor)){ # because a user can choose to set results to less than the automatically generated length of:: length(cor.out$cor) #(note: it is not possible for a user to have more lags than the standard input!)
    ccf = cor.out$cor[1:results]
  } else {
    ccf = cor.out$cor } #standard input
  
  ## Lag input
  if (wrap == 'no wrap'){
    # if there is 'no wrap' then the lag can be both positive and negative
    if (results < length(cor.out$cor)){ # because a user can choose to set results to less than the automatically generated length of:: length(cor.out$cor) #(note: it is not possible for a user to have more lags than the standard input!)
      lag = cor.out$lag[1:results]
    } else {
      lag = cor.out$lag } #usual input
  }
  
  if (wrap == 'wrapped'){
    # if the values are wrapped then they can only be positive
    # but, also, the 'lag' vector from the cor.out function does not repeat the 10x that the 'cor' vector does. So they are mismatched.
    lag = seq(1, length(ccf))} #as the values only positive, then we just want a lag vector as long as the ccf vector -> and as the values start at 1, we want the seq() to start at 1 too.
  
  ## Combine Lag and CCF input in a single dataframe:
  df = data.frame(lag,ccf)
  df = df[complete.cases(df),]
  
  
  ### Make plots
  
  # for samples:
  if (level == 'sample'){
    # plot the confidence intervals as horizontal lines
    if (type == 'CCF'){
      if (graph == 'bar'){
        cor_plot = ggplot(df, aes(lag, ccf)) +
          geom_hline(aes(yintercept = 0)) +
          geom_segment(mapping = aes(xend = lag, yend = 0)) +
          geom_hline(aes(yintercept = upper_bound), color = 'red', linetype = 2) +
          geom_hline(aes(yintercept = lower_bound), color = 'red', linetype = 2) +
          ggtitle(paste(type,' bar plot')) +
          ylab(type) +
          xlab('lag') }
      
      if (graph == 'line'){
        cor_plot = ggplot(df, aes(lag, ccf)) +
          geom_hline(aes(yintercept = 0)) +
          geom_line() +
          ggtitle(paste(type,' line plot')) +
          geom_hline(aes(yintercept = upper_bound), color = 'red', linetype = 2) +
          geom_hline(aes(yintercept = lower_bound), color = 'red', linetype = 2) +
          ylab(type) +
          xlab('lag') } }
    if (type == 'ACF'){
      if (graph == 'bar'){
        cor_plot = ggplot(df, aes(lag, ccf)) +
          geom_hline(aes(yintercept = 0)) +
          geom_segment(mapping = aes(xend = lag, yend = 0)) +
          #geom_hline(aes(yintercept = upper_bound), color = 'red', linetype = 2) +
          #geom_hline(aes(yintercept = lower_bound), color = 'red', linetype = 2) +
          ggtitle(paste(type,' bar plot')) +
          ylab(type) +
          xlab('lag') }
      
      if (graph == 'line'){
        cor_plot = ggplot(df, aes(lag, ccf)) +
          geom_hline(aes(yintercept = 0)) +
          geom_line() +
          ggtitle(paste(type,' line plot')) +
          #geom_hline(aes(yintercept = upper_bound), color = 'red', linetype = 2) +
          #geom_hline(aes(yintercept = lower_bound), color = 'red', linetype = 2) +
          ylab(type) +
          xlab('lag') }
    } }
  
  if (level == 'population'){
    # for populations:
    if (graph == 'bar'){
      # don't plot the confidence intervals (there are none)
      cor_plot = ggplot(df, aes(lag, ccf)) +
        geom_hline(aes(yintercept = 0)) +
        geom_segment(mapping = aes(xend = lag, yend = 0)) +
        ggtitle(paste(type,' bar plot')) +
        ylab(type) +
        xlab('lag') }
    
    if (graph == 'line'){
      cor_plot = ggplot(df, aes(lag, ccf)) +
        geom_hline(aes(yintercept = 0)) +
        geom_line() +
        ggtitle(paste(type,' line plot')) +
        ylab(type) +
        xlab('lag')}
  }
  
  cor_plot
}

