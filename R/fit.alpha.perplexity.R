#' @title fit.alpha.perplexity
#' @description Calculates perplexity for values of alpha on an LDA topic model using the topicmodels package, split into training and testing sets using k-folds
#' @param dtm Document-term matrix. Constructed using the DocumentTermMatrix() command from the tm package 
#' @param folds Integer. The number of folds to make training and testing sets; recommended values are '5' and '10' - note that higher values considerably increase the time that model fitting takes
#' @param alpha.values Numeric vector. Values to test alpha for. A good starting point is c(0.001, 0.01, 0.1, 1)
#' @param k Integer. Optional parameter: the value of k used in the LDA model. By default k is set to 10
#' @param beta Numeric. Optional parameter: the value of beta used in the LDA model. By default beta is set to 0.1
#' @param control.test List. Optional parameter: the LDA control list used in the LDA model. It is strongly recommended not to use this parameter unless you have good reason. Default settings are: nstart = 5, best = T, burnin = 1000, iter = 2000, thin = 500
#' @return Dataframe of perplexity for the alpha.values, calculated for the number of stipulated folds 
#' @export

fit.alpha.perplexity = function(dtm, folds, alpha.values, k, beta, control.test){
  
  if(missing(dtm)){
    stop('dtm is a required parameter')
  }
  if(missing(folds)){
    stop('folds is a required parameter')
  }
  if(missing(alpha.values)){
    stop('alpha.values is a required parameter')
  }
  
  
  if(class(dtm)[[1]] != 'DocumentTermMatrix'){
    stop("dtm must be of class DocumenTermMatrix. Check the class by using class()")
  }
  
  if(class(folds) != 'numeric' && class(folds) != 'integer'){
    stop('folds must be of class numeric or integer. Check the class by using class()')
  }
  
  if(length(folds) != 1){
    stop('folds must be a number of length 1. Check the length by using length(folds)')
  }
  
  
  if(class(alpha.values) != 'numeric' && class(alpha.values) != 'integer'){
    stop('alpha.values must be of class numeric or integer. Check the class by using class()')
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
  
  
  if(missing(beta)){
    beta = 0.1
  }
  
  if(class(beta) != 'numeric' && class(beta) != 'integer'){
    stop('beta must be of class numeric or integer. Check the class by using class()')
  }
  
  if(length(beta) != 1){
    stop('beta must be a number of length 1. Check the length by using length()')
  }
  
  
  if (missing(control.test)){
    control.test = list(nstart = 5,
                        seed = list(1,2,3,4,5),
                        best = T,
                        burnin = 1000,
                        iter = 2000,
                        thin = 500,
                        alpha = 0.1,
                        delta = beta)}
  
  print('fitting for alpha')
  print(paste0('beta is set to: ', beta))
  print(paste0('k is set to: ', k))
  
  
  # set up the k-fold validation
  n = nrow(dtm)
  folds = folds # number of folds in the validation
  set.seed(1)
  splitfolds = sample(1:folds, n, replace = T)
  candidate_alpha = alpha.values
  
  # create empty alpha_perplexity dataframe
  alpha_perplexity = as.data.frame(matrix(0, nrow = folds, ncol = length(candidate_alpha)))
  colnames(alpha_perplexity) = candidate_alpha
  
  # calculate perplexity for different alpha values
  for (i in seq(1, length(candidate_alpha))){
    
    alpha = candidate_alpha[i]
    print(paste0('fitting topic model for alpha = ', alpha)) # to check on how much progress is being made
    
    control.test$alpha = alpha # we just update the alpha value; that way people can still set the other params
    
    # use 5-fold verification:
    for (j in 1:folds){
      train_set = dtm[splitfolds != j, ]
      test_set = dtm[splitfolds == j, ]
      
      # fit the LDA model
      fitted = LDA(train_set, k = k, method = "Gibbs",
                   control = control.test)
      
      # calculate perplexity for test_set
      alpha_perplexity[j,i] = perplexity(fitted, newdata = test_set, control = list(seed = 1))
      
    }}
  return(alpha_perplexity)
}

