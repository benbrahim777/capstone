library(data.table)
library(tm)
library(RWeka)

load("lookup.Rda")
kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))

predict <- function(inString, lookup) {
  incorp <- Corpus(VectorSource(inString))
  incorp <- tm_map(incorp, content_transformer(tolower))
  incorp <- tm_map(incorp, removePunctuation)
  incorp <- tm_map(incorp, removeNumbers)
  ngrams <- kevTokenizer(incorp$content[[1]])
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- lookup[list(lastngram[2], lastngram[3], lastngram[4])]$o1
  return(prediction)
}

lookup[list("live","and","id")]$o1
lookup[list("bruises","from","playing")]$o1
lookup[list("me", "about")]
