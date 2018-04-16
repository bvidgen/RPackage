#' @title replaceSingleWords
#' @description Replaces single words with a space. Should be used with the tm::tm_map command. Advisable to cleanup text after other tm_map functions have been used 
#' @param corpus Corpus from the tm package. Created by using tm::Corpus(VectorSource([text data]))
#' @return Cleaned text
#' @export


replaceSingleWords <- tm::content_transformer(function(corpus) {return(gsub(" *\\b[[:alpha:]]\\b *", " ", corpus))})
  # from: https://stackoverflow.com/questions/31203843/r-find-and-remove-all-one-to-two-letter-words
