# server.R
options(shiny.maxRequestSize=30*1024^2)
library(shiny)
library(plyr)          # data ordering
library(reshape)

oracle2gram <- readRDS("data/project.data.2df.rds")
oracle3gram <- readRDS("data/project.data.3df.rds")
oracle4gram <- readRDS("data/project.data.4df.rds")
oracle5gram <- readRDS("data/project.data.5df.rds")

shinyServer(function(input, output) {
  
  output$textWords <- renderText({
    tolower(input$stringInput)
  })  ## This is the input redirecte to output to the UI

  
  ### Form strings to search against
  last_1 <- reactive({ tail(strsplit(trimws(input$stringInput),split=" ")[[1]],1) })   # list of last 1 term
  last_2 <- reactive({ tail(strsplit(trimws(input$stringInput),split=" ")[[1]],2) })   # list of last 2 terms
  last_3 <- reactive({ tail(strsplit(trimws(input$stringInput),split=" ")[[1]],3) })
  last_4 <- reactive({ tail(strsplit(trimws(input$stringInput),split=" ")[[1]],4) })
  
  
  output$w_1 <- renderText({ last_1() })
  output$w_2 <- renderText({ last_2() })
  output$w_3 <- renderText({ last_3() })
  output$w_4 <- renderText({ last_4() })
  
  
  ### Word guesses

    output$gram2 <- renderText({ 
      if ( length(last_1()) == 1  ){ 
         arrange(oracle2gram[ which(oracle2gram$leadingGram == tolower( trimws( paste(last_1() , collapse = ' ')))) , ], -count)[1,3] 
      } else {"input length != 1"}
    }) 
        
  
    output$gram3 <- renderText({ 
      if ( length(last_2()) == 2 ) {
         arrange(oracle3gram[ which( oracle3gram$leadingGram == tolower( trimws( paste(last_2() , collapse = ' ')))) , ] , -count)[1,3]         
      } else {"Absoultly Nothing to see there"}
    })
    
    
    output$gram4 <- renderText({ 
      if ( length(last_3()) == 3 ){
         arrange(oracle4gram[ which( oracle4gram$leadingGram == tolower( trimws( paste(last_3() , collapse = ' ')))) , ] , -count)[1,4]         
      } else {"Totally Nothing to see there"}
    })
    
    output$gram5 <- renderText({ 
      if ( length(last_4()) == 4  ){
          arrange(oracle5gram[ which( oracle5gram$leadingGram == tolower( trimws( paste(last_4() , collapse = ' ')))) , ] , -count)[1,4]         
      } else {"Ludicracy Nothing to see there"}
    })
    
    
    
    
    
    logical2 <- reactive({ 
      if ( length(last_1()) == 1 ){
        arrange(oracle2gram[ which( oracle2gram$leadingGram == tolower( trimws( paste(last_1() , collapse = ' ')))) , ] , -count)[1,3]         
      } 
    })
    
    logical3 <- reactive({ 
      if ( length(last_2()) == 2 ){
        arrange(oracle3gram[ which( oracle3gram$leadingGram == tolower( trimws( paste(last_2() , collapse = ' ')))) , ] , -count)[1,3]         
      } 
    })
    
    logical4 <- reactive({ 
      if ( length(last_3()) == 3 ){
        arrange(oracle4gram[ which( oracle4gram$leadingGram == tolower( trimws( paste(last_3() , collapse = ' ')))) , ] , -count)[1,4]         
      }  
    })
    
    logical5 <- reactive({ 
      if ( length(last_4()) == 4  ){
        arrange(oracle5gram[ which( oracle5gram$leadingGram == tolower( trimws( paste(last_4() , collapse = ' ')))) , ] , -count)[1,4]         
      } 
    })
    
    output$mesh <- reactive({
       ifelse( !is.null(logical5()) , logical5() , 
               ifelse( !is.null(logical4()) , logical4() , 
                     ifelse( !is.null(logical3()) , logical3() , 
                            ifelse( !is.null(logical2()) , logical2() , 'no matches found')))) })
                

    
 #### Table outputs  

    ### Table mesh
    
    
    output$tabmesh <- renderDataTable({  
      if( !is.null(tab5()) ) {tab5()}
          else { if(!is.null(tab4())) {tab4()}
               else { if(!is.null(tab3())) {tab3()}
                    else { if(!is.null(tab2())) {tab2()} }}}
    })
    
    
    tab2 <- reactive({
      if ( length(last_1()) == 1 ){
        arrange(oracle2gram[ which( oracle2gram$leadingGram == tolower( trimws( paste(last_1() , collapse = ' ')))) , ] , -count)[1:input$sliderGuesses,]        
      } else{'go away, you bother me'}
    })#, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE)) 
  
  
    tab3 <- reactive({
      if ( length(last_2()) == 2 ){
        arrange(oracle3gram[ which( oracle3gram$leadingGram == tolower( trimws( paste(last_2() , collapse = ' ')))) , ] , -count)[1:input$sliderGuesses,]        
      } 
    })#, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE)) 

    
    tab4 <- reactive({
      if ( length(last_3()) == 3 ){
        arrange(oracle4gram[ which( oracle4gram$leadingGram == tolower( trimws( paste(last_3() , collapse = ' ')))) , ] , -count)[1:input$sliderGuesses,]        
      } 
    })#, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE)) 
   
 
    tab5 <- reactive({
      if ( length(last_4()) == 4  ){
        arrange(oracle5gram[ which( oracle5gram$leadingGram == tolower( trimws( paste(last_4() , collapse = ' ')))) , ] , -count)[1:input$sliderGuesses,]        
      } 
    })#, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE)) 
    
##### test table
    #output$tabmesh2 <- renderDataTable({ tab2() })
    
        
#### Single tables    
    output$matchTable2 = renderDataTable({ 
      if ( length(last_1()) == 1 ){
        arrange(oracle2gram[ which(oracle2gram$leadingGram == tolower(trimws(paste(last_1() , collapse = ' '))) ) , ], -count)[1:input$sliderGuesses,]
      }}, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))    
    
    
    output$matchTable3 = renderDataTable({ 
      if ( length(last_2()) == 2 ){
        arrange(oracle3gram[ which(oracle3gram$leadingGram == tolower(trimws(paste(last_2() , collapse = ' '))) ) , ], -count)[1:input$sliderGuesses,]
        }}, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))    
   
    output$matchTable4 = renderDataTable({ 
      if ( length(last_3()) == 3 ){
        arrange(oracle4gram[ which(oracle4gram$leadingGram == tolower(trimws(paste(last_3() , collapse = ' '))) ) , ], -count)[1:input$sliderGuesses,]
      }}, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))    
    

    output$matchTable5 = renderDataTable({ 
      if (length(last_4()) == 4 ){
        arrange(oracle5gram[ which(oracle5gram$leadingGram == tolower(trimws(paste(last_4() , collapse = ' '))) ) , ], -count)[1:input$sliderGuesses,]
      }}, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))    
    
    
})  # close function input / output
