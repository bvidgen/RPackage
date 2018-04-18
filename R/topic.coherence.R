#' @title topic.coherence
#' @description Calculates topic coherence for each topic in a topic model. Uses the implementation put forward by Mimno et al. (2011) - Note, this directly uses the code in the package 'SpeedyReader'
#' @param ldaOut fitted topic model
#' @param dtm document term matrix for the fitted topic model
#' @param n.terms The number of terms in each topic to calculate topic coherence on
#' @return vector of topic coherence scores (same length as the number of topics in the model)
#' @export



# Calculate average topic coherence
topic.coherence = function(ldaOut, dtm, n.terms){
  
  # Implements UMass topic coherence based on PMI described by Minmo et al. "Optimizing Semantic Coherence in Topic Models"
  

  # coherence function - taken from SpeedReader package for calculating coherence 
  # replicated here rather than imported as SpeedyReader is still being developed
  # https://www.rdocumentation.org/packages/SpeedReader/versions/0.9.1/topics/topic_coherence
  my.topic_coherence <- function(top_words,
                              document_term_matrix,
                              vocabulary = NULL,
                              numeric_top_words = FALSE,
                              K = length(top_words)){
    
    # make sure the data is the right format
    vocabulary <- as.character(vocabulary)
    
    # perform some basic checks and throw errors if we see something weird.
    if(is.null(vocabulary) & !numeric_top_words){
      stop("You must provide a vocabulary vector!")
    }
    if(K > length(top_words)){
      K <- length(top_words)
      warning(paste("You must select a value for K that is less than length(top_words). K has automatically been set to :",K,sep = " "))
    }
    if(length(vocabulary) != ncol(document_term_matrix)){
      stop("The vocaublary vector must have the same number of entries as the number of columns in the document_term_matrix, and the word indicated by entries in the i'th column of document_term_matrix must correspond to the i'th entry in vocabulary.")
    }
    
    #if we are only using the K top words then reduce our top words vector
    top_words <- top_words[1:K]
    
    # binarize the document term matrix
    document_term_matrix <- matrix(as.numeric(document_term_matrix > 0),
                                   nrow = nrow(document_term_matrix),
                                   ncol = ncol(document_term_matrix))
    coherence_score <- 0
    for(i in 2:length(top_words)){
      for(j in 1:(i-1)){
        # we can either look up against vocab or just use indexes
        if(numeric_top_words){
          jindex <- top_words[j]
          iindex <- top_words[i]
        }else{
          jindex <- which(vocabulary == top_words[j])
          iindex <- which(vocabulary == top_words[i])
        }
        
        document_frequency <- sum(document_term_matrix[,jindex])
        j_positive <- which(document_term_matrix[,jindex] > 0)
        i_positive <- which(document_term_matrix[,iindex] > 0)
        co_document_frequency <- sum(i_positive %in% j_positive)
        
        coherence_score <- coherence_score + log((co_document_frequency + 1)/document_frequency)
        
      }
    }
    if(is.infinite(coherence_score)){
      coherence_score <- NA
      warning("The coherence score was not finite. Make sure that all words in your vocabulary appear atleast once.")
    }
    return(coherence_score)
  }
  
  # top terms in each topic
  ldaOut.topics = as.data.frame(terms(ldaOut.clean, n.terms))

  # get the coherence for each topic
  top.coherence = c()
  
  for (i in seq(1, ncol(ldaOut.topics))){
    # get the terms for just the ith topic
    top_words = as.character(ldaOut.topics[, i])
    
    # create a document term matrix with just these terms
    document_term_matrix = as.matrix(dtm)
    document_term_matrix = document_term_matrix[,which(colnames(document_term_matrix) %in% top_words)]
    
    # check that the ordering of the colnames matches up with the ordering of the terms
    top_words = top_words[match(colnames(document_term_matrix), top_words)]
    
    # calculate topic coherence using SpeedReader function
    ave.top.coherence = my.topic_coherence(top_words, document_term_matrix, vocabulary = top_words,
                                 numeric_top_words = FALSE, K = length(top_words))
    
    # save values
    top.coherence = c(top.coherence, ave.top.coherence)
  }
  
  return(top.coherence)
  
}









