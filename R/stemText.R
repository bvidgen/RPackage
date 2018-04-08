#' @title stemText
#' @description Basic stemming of text; removes 'ed', 'es' and 'ing' endings from words. Can lead to better results than stemDocument
#' @param corpus Corpus from the tm package. Created by using tm::Corpus(VectorSource([text data]))
#' @return Cleaned text
#' @export

stemText = tm::content_transformer(function(corpus) 
  {return(
  sapply(strsplit(corpus, split = " "), function(x){
    x = gsub(x = x, pattern = "*ed$", replacement = "")
    x = gsub(x = x, pattern = "*es$", replacement = "")
    x = gsub(x = x, pattern = "*ing$", replacement = "")
    paste(x, collapse  = " ")})
)})
