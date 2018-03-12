#' @title fit.topics.perplexity
#' @description Calculates perplexity for values of k on an LDA topic model using the topicmodels package, split into training and testing sets using k-folds
#' @param dtm Document-term matrix. Constructed using the DocumentTermMatrix() command from the tm package 
#' @param folds Integer. The number of folds to make training and testing sets; recommended values are '5' and '10' - note that higher values considerably increase the time that model fitting takes
#' @param k.values Numeric vector. Values to test k for. A good starting point is 2:10. All values must be greater than 1
#' @param alpha Numeric. Optional parameter: the value of alpha used in the LDA model. By default alpha is set to 0.1
#' @param beta Numeric. Optional parameter: the value of beta used in the LDA model. By default beta is set to 0.1
#' @param control.test List. Optional parameter: the LDA control list used in the LDA model. It is strongly recommended not to use this parameter unless you have good reason. Default settings are: nstart = 5, best = T, burnin = 1000, iter = 2000, thin = 500
#' @return Dataframe of perplexity for the k.values, calculated for the number of stipulated folds 
#' @export

fit.topics.perplexity = function(dtm, folds, k.values, alpha, beta, control.test){
  
  if(missing(dtm)){
    stop('dtm is a required parameter')
  }
  if(missing(folds)){
    stop('folds is a required parameter')
  }
  if(missing(k.values)){
    stop('k.values is a required parameter')
  }
  
  
  if(class(dtm)[[1]] != 'DocumentTermMatrix'){
    stop("dtm must be of class DocumenTermMatrix. Check the class by using class(dtm)")
  }
  
  if(class(folds) != 'numeric' && class(folds) != 'integer'){
    stop('folds must be of class numeric or integer. Check the class by using class(folds)')
  }
  
  if(length(folds) != 1){
    stop('folds must be a number of length 1. Check the length by using length(folds)')
  }
  
  if(class(k.values) != 'numeric' && class(k.values) != 'integer'){
    stop('k.values must be of class numeric or integer. Check the class by using class(k.values)')
  }
  
  if (1 %in% k.values){
    stop('k.values cannot include the value 1. It must start at 2, e.g. 2:10')
  }
  
  if(missing(alpha)){
    alpha = 0.1
  }
  
  if(missing(beta)){
    beta = 0.1
  }
  
  if (missing(control.test)){
    control.test = list(nstart = 5,
                        seed = list(1,2,3,4,5),
                        best = T,
                        burnin = 1000,
                        iter = 2000,
                        thin = 500,
                        alpha = alpha,
                        delta = beta)
  }
  
  print('fitting for k')
  print(paste0('alpha is set to: ', alpha))
  print(paste0('beta is set to: ', beta))
  
  # set up the k-fold validation
  n = nrow(dtm)
  folds = folds # number of folds in the validation
  set.seed(1)
  splitfolds = sample(1:folds, n, replace = T)
  candidate_k = k.values
  
  # create empty k_perplexity dataframe
  k_perplexity = as.data.frame(matrix(0, nrow = folds, ncol = length(candidate_k)))
  colnames(k_perplexity) = candidate_k
  
  # use k-fold verification to fit values of k against perplexity
  for (i in seq(1, length(candidate_k))){
    
    k = candidate_k[i]
    print(paste0('fitting topic model for k = ', k)) # to check on how much progress is being made
    
    # use 5-fold verification:
    for (j in 1:folds){
      train_set = dtm[splitfolds != j, ]
      test_set = dtm[splitfolds == j, ]
      
      # fit the LDA model
      fitted = LDA(train_set, k = k, method = "Gibbs",
                   control = control.test)
      
      # calculate perplexity for test_set
      k_perplexity[j,i] = perplexity(fitted, newdata = test_set, control = list(seed = 1),
                                     use_theta = T)
      
    } }
  return(k_perplexity)
}








# Text cleaning
# Create additional cleaning functions
# We create a replacePunctuation function to remove punctuation
# The inbuilt function is 'removePunctuation'. This is a bad function as it just straight up removes the punctuation
# Words which are connected by weird punctuation get turned into new joint words e.g. 'mp>>>,,Parliament' would become 'mpParliament' rather than 'mp Parliament'
# The function we create is 'replacePunctuation' which replaces removed punctuation with a space. In most cases, this is a far better operation
replacePunctuation <- tm::content_transformer(function(x) {return (gsub("[[:punct:]]"," ", x))})

# We create a removeSingleWords function to replace any single floating letters
removeSingleWords <- tm::content_transformer(function(x) {return(gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", x))})
# from: https://stackoverflow.com/questions/31203843/r-find-and-remove-all-one-to-two-letter-words

# We create a stem text which removes the word endings 'ed', 'es', and 'ing' - I often find the in-built stemming function too aggressive
stemText = content_transformer(function(x) {return(
  sapply(strsplit(x, split = " "), function(x){
    x = gsub(x = x, pattern = "*ed$", replacement = "")
    x = gsub(x = x, pattern = "*es$", replacement = "")
    x = gsub(x = x, pattern = "*ing$", replacement = "")
    paste(x, collapse  = " ")})
)})


