#' @title fit.beta.perplexity
#' @description Calculates perplexity for beta on an LDA topic model using the topicmodels package, split into training and testing sets using k-folds
#' @param dtm Document-term matrix. Constructed using the DocumentTermMatrix() command from the tm package 
#' @param folds Integer. The number of folds to make training and testing sets; recommended values are '5' and '10' - note that higher values considerably increase the time that model fitting takes
#' @param beta.values Numeric vector. The values to test beta for. A good starting point is c(0.001, 0.01, 0.1, 1)
#' @param k Integer. Optional parameter: the value of k used in the LDA model. By default k is set to 10
#' @param alpha Numeric. Optional parameter: the value of alpha used in the LDA model. By default alpha is set to 0.1
#' @param control.test List. Optional parameter: the LDA control list used in the LDA model. It is strongly recommended not to use this parameter unless you have good reason. Default settings are: nstart = 5, best = T, burnin = 1000, iter = 2000, thin = 500
#' @return Dataframe of perplexity for beta.values, calculated for the number of stipulated folds 
#' @export

fit.beta.perplexity = function(dtm, folds, beta.values, k, alpha, control.test){

  if(missing(dtm)){
    stop('dtm is a required parameter')
  }
  if(missing(folds)){
    stop('folds is a required parameter')
  }
  if(missing(beta.values)){
    stop('beta.values is a required parameter')
  }
  
  
  if(class(dtm)[[1]] != 'DocumentTermMatrix'){
    stop("dtm must be of class DocumenTermMatrix. Check the class by using class()")
  }
  
  
  if(class(folds) != 'numeric' && class(folds) != 'integer'){
    stop('folds must be of class numeric or integer. Check the class by using class()')
  }
  
  if(length(folds) != 1){
    stop('folds must be a number of length 1. Check the length by using length()')
  }
  
  
  if(class(beta.values) != 'numeric' && class(beta.values) != 'integer'){
    stop('beta.values must be of class numeric or integer. Check the class by using class()')
  }
  
  
  if(missing(k)){
    k = 10
  }
  
  if(class(k) != 'numeric' && class(k) != 'integer'){
    stop('k must be of class numeric or integer. Check the class by using class()')
  }
  
  if(length(k) != 1){
    stop('k must be a number of length 1. Check the length by using length()')
  }
  
  
  if(missing(alpha)){
    alpha = 0.1
  }
  
  if(class(alpha) != 'numeric' && class(alpha) != 'integer'){
    stop('alpha must be of class numeric or integer. Check the class by using class()')
  }
  
  if(length(alpha) != 1){
    stop('alpha must be a number of length 1. Check the length by using length()')
  }
  
  
  if (missing(control.test)){
    control.test = list(nstart = 5,
                        seed = list(1,2,3,4,5),
                        best = T,
                        burnin = 1000,
                        iter = 2000,
                        thin = 500,
                        alpha = alpha,
                        delta = 0.1)}
  
  print('fitting for beta')
  print(paste0('alpha is set to: ', alpha))
  print(paste0('k is set to: ', k))
  
  # set up the k-fold validation
  n = nrow(dtm)
  folds = folds # number of folds in the validation
  set.seed(1)
  splitfolds = sample(1:folds, n, replace = T)
  candidate_beta = beta.values
  
  
  # make empty 'beta_perplexity' df
  beta_perplexity = as.data.frame(matrix(0, nrow = folds, ncol = length(candidate_beta)))
  colnames(beta_perplexity) = candidate_beta
  
  
  # calculate perplexity for different beta values
  for (i in seq(1, length(candidate_beta))){
    beta = candidate_beta[i]
    print(paste0('fitting topic model for beta = ', beta)) # to check on how much progress is being made
    control.test$delta = beta # we just update the alpha value; that way people can still set the other params
    
    # use 5-fold verification:
    for (j in 1:folds){
      train_set = dtm[splitfolds != j, ]
      test_set = dtm[splitfolds == j, ]
      
      # fit the LDA model
      fitted = LDA(train_set, k = k, method = "Gibbs",
                   control = control.test)
      
      # calculate perplexity for test_set
      beta_perplexity[j,i] = perplexity(fitted, newdata = test_set, control = list(seed = 1))
      
    }}
  return(beta_perplexity)
}
