#' @title time_window
#' @description Calculates smoothed values for a dependent variable over different time windows
#' @details the smoothed depndent variable value is calculated in the middle of the time window; i.e. if the window is 24 hours then the dependent variable will smooth over the previous 12 hours and the following 12 hours
#' @param lags a vector of time periods for the dependent to be smoothed over. Periods are based on the unit of the first column in the dataframe i.e. if the first column is days then express the lag window in days (7,30,90 are recommended) or if it is in hours then express in hours (24 or 168 are recommended)
#' @param df a dataframe where the first column is the time variable, and the second column is the dependent variable
#' @return a 'long' dataframe with three columns; (1) the time variable, (2) the smoothed dependent variable, and (3) the time window
#' @export

# NOTE - need to add zoo to the list of package dependencies for Rccf
time_window = function(lags, df){
  
  #  if (!'zoo' %in% installed.packages())
  #    stop("zoo must be installed and loaded to continue")
  # if(!"package:zoo" %in% search())
  #   library(zoo)
  
  df = data.frame(df) #sometimes, if e.g. a data.table df is passed then the commands don't work
  
  # calculate the time window adjusted values
  dep.var = c()
  for (i in seq(1, length(lags))){
    out = zoo::rollapply(df[,2], width = lags[i], by =1, FUN = mean, align = "left") # takes a mean of values going forward the defined lag window
    dep.var[[i]] = out }
  
  # calculate the time window adjusted corresponding date
  time = c()
  for (i in seq(1, length(lags))){
    out = na.omit(dplyr::lead(df[,1], floor(lags[[i]]/2)))[1: length(dep.var[[i]])] # take the midway date = for 7 days, takes the date which is 3 days forward (so, the date corresponds to an averaged value going back 3 days and forward 4 days)
    time[[i]] = out }  
  
  # write out the time window lag 
  lag.window = c()
  for (i in seq(1, length(lags))){
    out = rep(paste0("lag_", lags[i]), length(dep.var[[i]]))
    lag.window[[i]] = out } 
  
  time_window = data.frame(date = c(unlist(time), df[,1]), 
                           dep.var = c(unlist(dep.var), df[,2]),
                           window = c(unlist(lag.window), rep('lag_0', nrow(df)) )
  )
  return(time_window)
}
