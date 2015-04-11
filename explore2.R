library(data.table)
library(tm)
library(RWeka)
library(pbapply)
library(dplyr)

kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))

test = c("this is one long line",
         "this is another line of a different length",
         "watch out for this one.",
         "the quick brown fox jumped over the Lazy dog")

ngrams <- pblapply(test, function(x) {
  corpus <- Corpus(VectorSource(x))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  kevTokenizer(corpus$content[[1]])
})

ngrams <- lapply(unlist(ngrams), function(x) {strsplit(x, " ")})
ngrams <- rbindlist(lapply(ngrams, function(ngram) as.list(unlist(ngram))))

setkey(ngrams, V1, V2, V3)
ngrams[J("this","is","one")]
ngrams[list("this","is","one")]$V4[1]
