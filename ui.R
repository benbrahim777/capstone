library(shiny)

# Define a UI for the application
shinyUI(fluidPage(
  titlePanel("Predictive Text Application"),
  p("Enter a message into the text box below and the application will attempt to predict the next word."),
  textInput("inputText", label= h3("Input text")),
  h3("Suggested words"),
  textOutput("text1")
))