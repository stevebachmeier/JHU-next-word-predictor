library(shiny)

shinyServer(function(input, output) {
  source("f_nlpPredictor_3grams.R")
  

  nlpPredictor(input$box1)

  

})
