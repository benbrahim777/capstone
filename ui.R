library(shiny)

# Define a UI for the application
shinyUI(fluidPage(
  titlePanel("Predictive Text Application"),
  p("Enter a message into the text box below and the application will attempt to predict the next word."),
  textInput("inputText", label= h3("Input text")),
  h3("Suggested words"),
  textOutput("text1"),
  HTML("<hr/>"),
  h3("Explanation"),
  p("The input text is broken up into 3-grams and then the last 3-gram in the input text is checked against a data.table of 4-grams. The last word for the 4-grams is returned."),
  p("The program tries to return 5 suggestions. If there are not five words returned then it will break the input text into bigrams and return the results from a table of trigrams"),
  p("As a last resort, the app will return the five most frequently seen words in the english language"),
  plotOutput("bigrams"),
  plotOutput("trigrams"),
  plotOutput("fourgrams")
))