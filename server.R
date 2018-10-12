library(shiny)

shinyServer(function(input, output) {
  source("f_nlpPredictor.R")
  

  nlpPredictor(input$box1)

  

})
