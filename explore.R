library(tm)
library(RWeka)
library(pbapply)
library(dplyr)

kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=5))

tokenizeFiles <- function(x, sampleRate=0.01, verbose=TRUE) {
  pboptions(type="txt")
  if(!verbose) { pboptions(type="none")}
  tokenVector = c()
  for(eachFile in x) {
    if(verbose) { print(paste("Processing",eachFile)) }
    allLines <- readLines(eachFile, skipNul = TRUE, encoding = "utf-8")
    list_of_tokens <- pblapply(sample(allLines, round(length(allLines)*sampleRate)), function(x) {
      corpus <- Corpus(VectorSource(x))
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      kevTokenizer(corpus$content[[1]])
    })
    tokenVector <- c(tokenVector, unlist(list_of_tokens))
  }
  data.frame(tokens = tokenVector, stringsAsFactors = FALSE) %>% count(tokens)
}

splitter <- function(x, prefix=FALSE) {
  array <- unlist(strsplit(x, " "))
  if(prefix) {
    return(paste(array[1:length(array)-1], collapse=" "))
  }
  return(array[length(array)])
  
}

predictor <- function(prefix, df) {
  searchString <- paste0("^",prefix,"$")
  temp <- df[grep(searchString, df$prefix),] %>% arrange(desc(n))
  temp$suffix
}

deezTokens <- tokenizeFiles(c("final/en_US/en_US.blogs.txt", "final/en_US/en_US.news.txt", "final/en_US/en_US.twitter.txt"), 
                            sampleRate = 0.2)
deezTokens$suffix <- unlist(lapply(deezTokens$tokens, splitter))
deezTokens$prefix <- unlist(lapply(deezTokens$tokens, splitter, prefix=TRUE))
deezTokens$tokens <- NULL
deezTokens <- deezTokens %>% group_by(prefix) %>% top_n(1, n) %>% filter(n>=10)