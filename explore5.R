library(data.table)
library(tm)
library(RWeka)
library(pbapply)
library(dplyr)

unlist(lapply(1:4, function(x) {
  paste0("p",x)
}))

kevSplitter <- function(x, ngramsize=4) {
  tokens <- strsplit(x, " ")
  if(length(tokens[[1]]) != ngramsize) { warning(paste(x, "does not split into", ngramsize, collapse = " "))}
  tokens[[1]] <- c(tokens[[1]], rep("nop",ngramsize))
  return(tokens[[1]][1:ngramsize])
}

buildDB <- function(inDirectory, sampleRate=0.01, verbose=TRUE, threshold=0, ngramsize=4) {
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=ngramsize, max=ngramsize))
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
  ngrams <- pblapply(unlist(ngrams), kevSplitter, ngramsize)
  
  if(verbose) { print("now building data.table")}
  ngrams <- rbindlist(pblapply(ngrams, function(ngram) as.list(unlist(ngram))))
  newnames <- unlist(lapply(1:(ngramsize-1), function(x) { paste0("p",x)}))
  names(ngrams) <- c(newnames, "o1")
  ngrams <- ngrams %>% count_(c(newnames, "o1"))
  ngrams <- ngrams %>% filter(n >= threshold)
  setkeyv(ngrams, newnames)
  return(ngrams)
}

predict <- function(inString, lookup) {
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=ncol(lookup)-2, max=ncol(lookup)-2))
  incorp <- Corpus(VectorSource(inString))
  incorp <- tm_map(incorp, content_transformer(tolower))
  incorp <- tm_map(incorp, removePunctuation)
  incorp <- tm_map(incorp, removeNumbers)
  ngrams <- kevTokenizer(incorp$content[[1]])
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  print(lastngram)
  prediction <- lookup[as.list(lastngram)]$o1
  return(prediction)
}

sourceDir <- "final/en_US"
#sourceDir <- "test"

fourGrams <- buildDB(sourceDir, sampleRate = 0.66, threshold = 10)
fourGrams <- fourGrams %>% group_by(p1,p2,p3) %>% top_n(5, n)

triGrams <- buildDB(sourceDir, sampleRate = 0.5, threshold = 10, ngramsize = 3)
triGrams <- triGrams %>% group_by(p1, p2) %>% top_n(5, n)

biGrams <- buildDB(sourceDir, sampleRate = 0.33, threshold = 10, ngramsize = 2)
biGrams <- biGrams %>% group_by(p1) %>% top_n(5, n)