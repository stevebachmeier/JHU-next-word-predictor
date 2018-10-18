library(shiny)

shinyServer(function(input, output) {
  source("f_nlpPredictor_3grams.R")
  
  output$predictions <- renderPrint({
    nlpPredictor(input$box1)
  })
  
  # if (!is.null(input$box1)) {
  #   output$predictions <- renderPrint({
  #     nlpPredictor(input$box1)
  #   })
  # }
  
})
