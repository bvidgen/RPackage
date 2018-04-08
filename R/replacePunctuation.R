#' @title replacePunctuation
#' @description Replaces punctuation with a space. Should be used with the tm::tm_map command.  Can lead to better results than removePunctuation
#' @param corpus Corpus from the tm package. Created by using tm::Corpus(VectorSource([text data]))
#' @return Cleaned text
#' @export


replacePunctuation= tm::content_transformer(function(corpus) {return (gsub("[[:punct:]]"," ", corpus))})

