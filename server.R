# server.R

library(shiny)
library(plyr)          # data ordering
library(reshape)
oracle <- readRDS("data/three_gram_oracle.rds")

shinyServer(

  function(input, output) {
    
    output$text1 <- renderText({ 
      arrange(oracle[ which(oracle$leading_Bigram == input$stringInput & oracle$count > 1) , ] , -count)[1,4]         
    })
    
    # sorted columns are colored now because CSS are attached to them
    output$matchTable = renderDataTable({
      arrange(oracle[ which(oracle$leading_Bigram == input$stringInput ) , ], -count)[1:input$sliderGuesses,]
    }, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))

  }
)
