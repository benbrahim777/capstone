library(shiny)
library(data.table)
library(tm)
library(RWeka)

load("lookup.Rda")
load("fourGrams.Rda")
kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))

predict <- function(inString) {
  incorp <- Corpus(VectorSource(inString))
  incorp <- tm_map(incorp, content_transformer(tolower))
  incorp <- tm_map(incorp, removePunctuation)
  incorp <- tm_map(incorp, removeNumbers)
  ngrams <- kevTokenizer(incorp$content[[1]])
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  # prediction <- lookup[list(lastngram[1], lastngram[2], lastngram[3])]$o1
  prediction <- fourGrams[list(lastngram[1], lastngram[2], lastngram[3])]$o1
  return(prediction)
}

shinyServer(function(input, output) {
  output$text1 <- renderText({ predict(input$inputText) })
})