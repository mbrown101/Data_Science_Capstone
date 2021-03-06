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
    c("The most probable word following the string --" , tolower(input$stringInput) , " -- is:")
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
         arrange(oracle2gram[ which(oracle2gram$leading_Gram == tolower( trimws( paste(last_1() , collapse = ' ')))) , ], -frequency)[1,2] 
      } else {"input length != 1"}
    }) 
        
  
    output$gram3 <- renderText({ 
      if ( length(last_2()) == 2 ) {
         arrange(oracle3gram[ which( oracle3gram$leading_Gram == tolower( trimws( paste(last_2() , collapse = ' ')))) , ] , -frequency)[1,2]         
      } else {"Absoultly Nothing to see there"}
    })
    
    
    output$gram4 <- renderText({ 
      if ( length(last_3()) == 3 ){
         arrange(oracle4gram[ which( oracle4gram$leading_Gram == tolower( trimws( paste(last_3() , collapse = ' ')))) , ] , -frequency)[1,2]         
      } else {"Totally Nothing to see there"}
    })
    
    output$gram5 <- renderText({ 
      if ( length(last_4()) == 4  ){
          arrange(oracle5gram[ which( oracle5gram$leading_Gram == tolower( trimws( paste(last_4() , collapse = ' ')))) , ] , -frequency)[1,2]         
      } else {"Ludicracy Nothing to see there"}
    })
    
    
    
    
    logical2 <- reactive({ 
      if ( length(last_1()) == 1 ){
        arrange(oracle2gram[ which( oracle2gram$leading_Gram == tolower( trimws( paste(last_1() , collapse = ' ')))) , ] , -frequency)[1,2]         
      } else {NA}
    })
    
    logical3 <- reactive({ 
      if ( length(last_2()) == 2 ){
        arrange(oracle3gram[ which( oracle3gram$leadingGram == tolower( trimws( paste(last_2() , collapse = ' ')))) , ] , -frequency)[1,2]         
      } else {NA}
    })
    
    logical4 <- reactive({ 
      if ( length(last_3()) == 3 ){
        arrange(oracle4gram[ which( oracle4gram$leading_Gram == tolower( trimws( paste(last_3() , collapse = ' ')))) , ] , -frequency)[1,2]         
      }  else {NA}
    })
    
    logical5 <- reactive({ 
      if ( length(last_4()) == 4  ){
        arrange(oracle5gram[ which( oracle5gram$leading_Gram == tolower( trimws( paste(last_4() , collapse = ' ')))) , ] , -frequency)[1,2]         
      } else {NA}
    })
    
    output$mesh <- reactive({
       ifelse( !is.na(logical5()) , logical5() , 
               ifelse( !is.na(logical4()) , logical4() , 
                     ifelse( !is.na(logical3()) , logical3() , 
                            ifelse( !is.na(logical2()) , logical2() , 'no matches found')))) })
                

    
 #### Table outputs  

    ### Table mesh
    
    
    output$tabmesh <- renderDataTable({  
      if( !is.na(tab5()) ) {tab5()}
          else { if(!is.na(tab4())) {tab4()}
               else { if(!is.na(tab3())) {tab3()}
                    else { if(!is.na(tab2())) {tab2()} }}}
    } , options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))  
    
    
    tab2 <- reactive({
      if ( length(last_1()) == 1 ){
        arrange(oracle2gram[ which( oracle2gram$leading_Gram == tolower( trimws( paste(last_1() , collapse = ' ')))) , ] , -frequency)[1:input$sliderGuesses,]        
      }  else {NA}
    })
  
  
    tab3 <- reactive({
      if ( length(last_2()) == 2 ){
        arrange(oracle3gram[ which( oracle3gram$leading_Gram == tolower( trimws( paste(last_2() , collapse = ' ')))) , ] , -frequency)[1:input$sliderGuesses,]        
      }  else {NA}
    }) 

    
    tab4 <- reactive({
      if ( length(last_3()) == 3 ){
        arrange(oracle4gram[ which( oracle4gram$leading_Gram == tolower( trimws( paste(last_3() , collapse = ' ')))) , ] , -frequency)[1:input$sliderGuesses,]        
      }  else {NA}
    }) 
   
 
    tab5 <- reactive({
      if ( length(last_4()) == 4  ){
        arrange(oracle5gram[ which( oracle5gram$leading_Gram == tolower( trimws( paste(last_4() , collapse = ' ')))) , ] , -frequency)[1:input$sliderGuesses,]        
      }  else {NA}
    }) 
    
##### test table
    #output$tabmesh2 <- renderDataTable({ tab2() })
    
        
#### Single tables    
#    output$matchTable2 = renderDataTable({ 
#      if ( length(last_1()) == 1 ){
#        arrange(oracle2gram[ which(oracle2gram$leadingGram == tolower(trimws(paste(last_1() , collapse = ' '))) ) , ], -count)[1:input$sliderGuesses,]
#      }}, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))    
    
    
#    output$matchTable3 = renderDataTable({ 
#      if ( length(last_2()) == 2 ){
#        arrange(oracle3gram[ which(oracle3gram$leadingGram == tolower(trimws(paste(last_2() , collapse = ' '))) ) , ], -count)[1:input$sliderGuesses,]
#        }}, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))    
   
#    output$matchTable4 = renderDataTable({ 
#      if ( length(last_3()) == 3 ){
#        arrange(oracle4gram[ which(oracle4gram$leadingGram == tolower(trimws(paste(last_3() , collapse = ' '))) ) , ], -count)[1:input$sliderGuesses,]
#      }}, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))    
    

#    output$matchTable5 = renderDataTable({ 
#      if (length(last_4()) == 4 ){
#        arrange(oracle5gram[ which(oracle5gram$leadingGram == tolower(trimws(paste(last_4() , collapse = ' '))) ) , ], -count)[1:input$sliderGuesses,]
#      }}, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))    
    
    
})  # close function input / output
