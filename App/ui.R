library(shiny)

shinyUI(fluidPage(#theme = "bootstrap.css",
  titlePanel("Next Word Predictor"),
      
    # Main panel
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Main", 
                           h2("Type away:"),
                           textInput("box1", label=NULL, placeholder="Start typing:", width="100%"),
                           # submitButton("Submit"),
                           # actionButton(label="Submit", inputId=1),
                           br(),
                           h2("Suggestions:"),
                           p("Shown below are the top three suggestions from the model and their respective scores."),
                           # textOutput("predictions")),
                           # renderPrint("predictions")),
                           # verbatimTextOutput("predictions")),
                           htmlOutput("predictions")),
                  
                  tabPanel("Help", 
                           h2("Background"),
                           p("This app takes user text input and provides three recommendations for the next word."),
                           
                           h2("Instructions"),
                           p("1. Start typing (in English) into the input box."), 
                           p("2. Three next word predictions will be provided."),
                           
                           h2("Approach"),
                           p("This app uses a Stupid Backoff model (reference 'Large Language Models in Machine Translation,' 
                              Brants et al, Proceedings of the 2007 Joint Conference on Empirical Methods in Natural Language Processing and 
                              Computational Natural Language Learning, pp. 858-867, Prague, June 2007) using n-grams derived from three 
                              corpora provided by SwiftKey: twitter, blogs, and news. All are primarily in English."),
                           p("First, the last three words submitted by the user are searched for in the 4-grams from a sample corpus (which
                             combines samples from all three provided corpora) and any matches are stored with their corresponding scores. Then,
                             we back off to the 3-grams and search for the last two words submitted and calculate those scores with a 
                             knockdown factor of 0.4. We back off again and search the 2-grams with a knockdown factor of 0.4^2 and finally the
                             1-grams with a knockdown factor of 0.4^3. The scores are then all added up for each prediction and the top three
                             words are provided."),
                           p("In the event that the user would like a prediction based on less than three words, the model simply starts with
                             the appropriate n-gram list."),
                           p("Note that three predictions will always be provided. In cases where there are very few or no n-gram matches,
                             the model simply provides the most commonly used words from the sample corpus. This is why three suggestions are
                             provided even before the user begins typing.")
                           )
      )
    )
  )
)
