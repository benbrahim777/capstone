library(shiny)
library(data.table)
library(tm)
library(RWeka)
library(ggplot2)
library(dplyr)
options(mc.cores=1)

load("lookup.Rda")
load("fourGrams.Rda")
load("triGrams.Rda")
load("biGrams.Rda")

herokuPurple <- "#3C378B"
herokuLight <- "#D3CBED"

cleanText <- function(inString) {
  incorp <- Corpus(VectorSource(inString))
  incorp <- tm_map(incorp, content_transformer(tolower))
  incorp <- tm_map(incorp, removePunctuation)
  incorp <- tm_map(incorp, removeNumbers)
  return(incorp$content[[1]][[1]])
}

predict2 <- function(inString) {
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
  ngrams <- kevTokenizer(cleanText(inString))
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- triGrams[list(lastngram[1])]$o1
  prediction <- c(prediction, c("the","to","and","a","of"))
  prediction <- prediction[!is.na(prediction)]
  return(prediction[1:5])
}

predict3 <- function(inString) {
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
  ngrams <- kevTokenizer(cleanText(inString))
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- triGrams[list(lastngram[1], lastngram[2])]$o1
  if(is.na(prediction)) { return(predict2(inString))}
  if(length(prediction) <5) {prediction <- c(prediction, predict2(inString))}
  return(prediction[1:5])
}
predict4 <- function(inString) {
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
  ngrams <- kevTokenizer(cleanText(inString))
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- fourGrams[list(lastngram[1], lastngram[2], lastngram[3])]$o1
  if(is.na(prediction)) { return(predict3(inString)) }
  if(length(prediction) < 5) {prediction <- c(prediction, predict3(inString))}
  return(prediction[1:5])
}

predict <- function(inString) {
  inStringLength <- length(unlist(strsplit(inString, " ")))
  if(inStringLength == 0) {
    return(c("I","The","I'm","You","Thanks"))
  }
  if(inStringLength == 1) {
    return(predict2(inString))
  }
  
  if(inStringLength == 2) {
    return(predict3(inString))
  }
  
  if(inStringLength > 2) {
    return(predict4(inString))
  }
}

shinyServer(function(input, output) {
  output$text1 <- renderText({ predict(input$inputText) })
  output$bigrams <- renderPlot({
    clean <- cleanText(input$inputText)
    words <- unlist(strsplit(clean, " "))
    if(length(words) >= 1) {
      lastword <- tolower(words[length(words)])
      results <- biGrams[list(lastword)]
      if(!is.na(results$o1[1])) {
        results$o1 <- factor(results$o1, levels = arrange(results, n)$o1)
        biplot <- ggplot(results, aes(x=o1, y=n)) + geom_bar(stat="identity", fill=herokuLight, color=herokuPurple) +
          theme_bw() + coord_flip() +
          labs(title="Frequent words based on last word in the input text", x="Prediction", y="Count")
        return(biplot)  
      }
    }
  })
  output$trigrams <- renderPlot({
    clean <- cleanText(input$inputText)
    words <- unlist(strsplit(clean, " "))
    if(length(words) >=2) {
      lastwords <- tolower(words[(length(words)-1):length(words)])
      print(lastwords)
      results <- triGrams[list(lastwords[1], lastwords[2])]
      if(!is.na(results$o1[1])) {
        results$o1 <- factor(results$o1, levels = arrange(results, n)$o1)
        triplot <- ggplot(results, aes(x=o1, y=n)) + geom_bar(stat="identity", fill=herokuLight, color=herokuPurple) +
          theme_bw() + coord_flip() +
          labs(title="Frequent words based on last two words in the input text", x="Prediction", y="Count")
        return(triplot)
      }
      
    }
  })
  
  output$fourgrams <- renderPlot({
    clean <- cleanText(input$inputText)
    words <- unlist(strsplit(clean, " "))
    if(length(words) >=3) {
      lastwords <- tolower(words[(length(words)-2):length(words)])
      results <- fourGrams[list(lastwords[1], lastwords[2], lastwords[3])]
      if(!is.na(results$o1[1])) {
        results$o1 <- factor(results$o1, levels = arrange(results, n)$o1)
        triplot <- ggplot(results, aes(x=o1, y=n)) + geom_bar(stat="identity", fill=herokuLight, color=herokuPurple) +
          theme_bw() + coord_flip() +
          labs(title="Frequent words based on last three words in the input text", x="Prediction", y="Count")
        return(triplot)
      }
    }
  })
  
})