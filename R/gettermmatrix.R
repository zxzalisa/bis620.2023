#' @title Word Frequency
#' @description  Get the frequency of words from the text.
#' @param x the text.
#' @return Frequency of words with descending order.
#' @importFrom memoise
#' @export
getTermMatrix <- memoise(function(x) {
  x <- as.character(x)
  if (is.character(x)){
    myCorpus <- Corpus(VectorSource(x))
    myCorpus <- tm_map(myCorpus, content_transformer(tolower))
    myCorpus <- tm_map(myCorpus, removePunctuation)
    myCorpus <- tm_map(myCorpus, removeNumbers)
    myCorpus <- tm_map(myCorpus, removeWords,
                      c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
    myDTM <- TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    m <- as.matrix(myDTM)
    sort(rowSums(m), decreasing = TRUE)
  }
})
