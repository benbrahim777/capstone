library(data.table)
library(tm)
library(RWeka)
library(pbapply)
library(dplyr)

kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))

kevSplitter <- function(x) {
  tokens <- strsplit(x, " ")
  if(length(tokens[[1]]) != 4) { warning(paste(x, "does not split into 4", collapse = " "))}
  tokens[[1]] <- c(tokens[[1]], "nop","nop","nop","nop")
  return(tokens[[1]][1:4])
}

buildDB <- function(inDirectory, sampleRate=0.01, verbose=TRUE, threshold=0) {
  pboptions(type="txt")
  if(!verbose) { pboptions(type="none")}
  if(verbose) { print(paste("reading source files from", inDirectory, collapse = " "))}
  allLines <- unlist(pblapply(dir(inDirectory, full.names = T), function(inFile) readLines(inFile, skipNul = TRUE, encoding = "utf-8")))
  
  if(verbose) { print("building corpus from sample")}
  corpus <- Corpus(VectorSource(sample(allLines, round(length(allLines)*sampleRate))))
  
  if(verbose) { print("cleaning up the corpus")}
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)

  if(verbose) { print("extracting ngrams from the corpus")}
  ngrams <- unlist(pblapply(corpus$content, kevTokenizer))
  
  if(verbose) { print("splitting the ngrams by whitespace")}
  ngrams <- pblapply(unlist(ngrams), kevSplitter)
  
  if(verbose) { print("now building data.table")}
  ngrams <- rbindlist(pblapply(ngrams, function(ngram) as.list(unlist(ngram))))
  names(ngrams) <- c("p1","p2","p3","o1")
  ngrams <- ngrams %>% count(p1,p2,p3,o1)
  ngrams <- ngrams %>% filter(n >= threshold)
  setkey(ngrams, p1,p2,p3)
  return(ngrams)
}

sourceDir <- "final/en_US"
#sourceDir <- "test"

d1 <- date()
lookup <- buildDB(sourceDir, sampleRate = 0.5, threshold = 10)
d2 <- date()

lookup <- lookup %>% group_by(p1,p2,p3) %>% top_n(1, n)