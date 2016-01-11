# server.R
options(shiny.maxRequestSize=30*1024^2)
library(shiny)
library(plyr)          # data ordering
library(reshape)
library(stringr)

oracle3gram <- readRDS("data/project.data.3df.rds")
oracle2gram <- readRDS("data/project.data.2df.rds")
oracle4gram <- readRDS("data/project.data.4df.rds")
oracle5gram <- readRDS("data/project.data.5df.rds")

#rows2 <- paste("rows in 2 gram =" , nrow(oracle2gram))
rows3 <- paste("rows in 3 gram =" , nrow(oracle3gram))

shinyServer(function(input, output) {
  
# newdata <- mtcars[order(mpg, cyl),]
  
  #### rows in oracles    
  #output$text2Gram <- renderText({return(rows2)})
  output$text3Gram <- renderText({return(rows3)})
  
  output$textWords <- renderText({
    tolower(input$stringInput)
    #paste(unlist(strsplit(input$stringInput , " ")) , "")
  })  ## This is the input redirecte to output to the UI

  
  ### Form strings to search against
  #termTail1 <- function() return( trimws( tail( unlist( strsplit( input$stringInput , ' ') ) , 1) ) )
  #termTail2 <- function() return( trimws(paste(tail(unlist(strsplit(input$stringInput , ' ')) , 2) , ' ')) )
  #termTail3 <- function() return( trimws(paste(tail(unlist(strsplit(input$stringInput , ' ')) , 3) , ' ')) )
  #termTail4 <- function() return( trimws(paste(tail(unlist(strsplit(input$stringInput , ' ')) , 4) , ' ')) )
  
  termTail1 <- function() return( ifelse( !is.null(input$stringInput)  , word( trimws(input$stringInput) , -1) , NULL ) )
  termTail2 <- function() return( ifelse( !is.null(input$stringInput)*!is.null(termTail1())*!is.na(word( input$stringInput , -2)) , paste( word( input$stringInput , -2) , termTail1() ) , NULL ) )
  termTail3 <- function() return( ifelse( !is.null(input$stringInput)*!is.null(termTail2())*!is.na(word( input$stringInput , -3)) , paste( word( input$stringInput , -3) , termTail2() ) , NULL ) )
  termTail4 <- function() return( ifelse( !is.null(input$stringInput)*!is.na(word( input$stringInput , -4)) , paste( word( input$stringInput , -4) , termTail3() ) , NULL ) )
  
  output$formed1G <- renderText({ termTail1() })
  output$formed2G <- renderText({ termTail2() })
  output$formed3G <- renderText({ termTail3() })
  output$formed4G <- renderText({ termTail4() })
  
  wordGuess2gram <- function(stringIn) return( 
      arrange(oracle3gram[ which( oracle3gram$leadingGram == tolower( trimws( stringIn ))), ] , -count)[1,4]         
      )
  

  
  ### Word guess
 # output$text1 <- renderText({ 
#    if (length(unlist(strsplit(input$stringInput , " "))) == 2){
#      arrange(oracle3gram[ which(oracle3gram$leadingGram == tolower(trimws(input$stringInput))) , ] , -count)[1,4]         
#    } else {"Nothing to see there"}
#  })
  

    output$gram2 <- renderText({ 
      if ( stri_stats_latex(input$stringInput)[4] > 0 & !is.null(input$stringInput)  ){
        arrange(oracle2gram[ which( oracle2gram$leadingGram == tolower( trimws( termTail1() )) ) , ] , -count)[1,4]         
      } else {"REally - Nothing to see there"}
    }) 
    
        
    output$gram3 <- renderText({ 
      if (stri_stats_latex(input$stringInput)[4] > 1 & !is.null(input$stringInput) ){
        arrange(oracle3gram[ which( oracle3gram$leadingGram == tolower( trimws( termTail2() )) ) , ] , -count)[1,4]         
      } else {"Absoultly Nothing to see there"}
    })
  
    
    output$gram4 <- renderText({ 
      if (stri_stats_latex(input$stringInput)[4] > 2 & !is.null(input$stringInput) ){
        arrange(oracle4gram[ which( oracle4gram$leadingGram == tolower( trimws( termTail3() )) ) , ] , -count)[1,4]         
      } else {"Totally Nothing to see there"}
    })
    
    
    output$gram5 <- renderText({ 
      if (stri_stats_latex(input$stringInput)[4] > 3 & !is.null(input$stringInput) ){
        arrange(oracle5gram[ which( oracle5gram$leadingGram == tolower( trimws( termTail4() )) ) , ] , -count)[1,4]         
      } else {"Ludicracy Nothing to see there"}
    })
    
    
    output$text2 <- renderText({ 
      if (length(unlist(strsplit(termTail2() , " "))) == 2){
        arrange(oracle3gram[ which( oracle3gram$leadingGram == tolower( trimws( termTail2() )) ) , ] , -count)[1,4]         
      } else {"Nothing to see here"}
    })
    
    

    output$matchTable2 = renderDataTable({ 
      if (stri_stats_latex(input$stringInput)[4] > 0 & !is.null(input$stringInput)){
        arrange(oracle2gram[ which(oracle2gram$leadingGram == tolower(trimws(termTail1())) ) , ], -count)[1:input$sliderGuesses,]
      }}, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))    
    
    
    output$matchTable3 = renderDataTable({ 
      if (stri_stats_latex(input$stringInput)[4] > 1 & !is.null(input$stringInput)){
        arrange(oracle3gram[ which(oracle3gram$leadingGram == tolower(trimws(termTail2())) ) , ], -count)[1:input$sliderGuesses,]
        }}, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))    
   
    output$matchTable4 = renderDataTable({ 
      if (stri_stats_latex(input$stringInput)[4] > 2 & !is.null(input$stringInput)){
        arrange(oracle4gram[ which(oracle4gram$leadingGram == tolower(trimws(termTail3())) ) , ], -count)[1:input$sliderGuesses,]
      }}, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))    
    

    output$matchTable5 = renderDataTable({ 
      if (stri_stats_latex(input$stringInput)[4] > 3 & !is.null(input$stringInput)){
        arrange(oracle5gram[ which(oracle5gram$leadingGram == tolower(trimws(termTail4())) ) , ], -count)[1:input$sliderGuesses,]
      }}, options = list(orderClasses = TRUE , searching = FALSE , paging = FALSE))    
    
    

    
    
})  # close function input / output
