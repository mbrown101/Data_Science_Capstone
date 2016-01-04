# server.R
options(shiny.maxRequestSize=30*1024^2)
library(shiny)
library(plyr)          # data ordering
library(reshape)
oracle3gram <- readRDS("data/three_gram_oracle.rds")

shinyServer(

  function(input, output) {
    
    output$textWords <- renderText({
      paste(unlist(strsplit(input$stringInput , " ")) , "")
    })
    
    output$text1 <- renderText({ 
      if (length(unlist(strsplit(input$stringInput , " "))) == 2){
      arrange(oracle3gram[ which(oracle3gram$leading_Bigram == input$stringInput & oracle3gram$count > 1) , ] , -count)[1,4]         
    } else {
      "dog"
    }
   })
    
    output$matchTable = renderDataTable({
      arrange(oracle3gram[ which(oracle3gram$leading_Bigram == input$stringInput ) , ], -count)[1:input$sliderGuesses,]
    }, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))

  }
)
