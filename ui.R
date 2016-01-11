# ui.R

shinyUI(fluidPage(
    titlePanel("Coursera Data Science Project"),

    sidebarLayout(
        sidebarPanel(
        
             helpText("This project returns the most probable word following the words input in the text box below. "),
            
             tags$br(),
             
             textInput(inputId = "stringInput", 
                       label = "Enter string to guess next word",
                       value = "", 
                       width = NULL),
             
             tags$br(),
             
             sliderInput("sliderGuesses", 
                          label = "Probabilistic matches to be returned in the summary table",
                          min = 1, max = 10, value = 3 , step = 1)
             
             
             ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel('Results',
                 
                 h4("The most probable word following the string:"),
                 textOutput("textWords"),

                 #textOutput("text2Gram"),
                 #textOutput("text3Gram"),
        
                 
                 #h4("/////terms Below /////"),
                    textOutput("formed1G"),
                    textOutput("formed2G"),
                    textOutput("formed3G"),
                    textOutput("formed4G"),
                 #h4("/////terms above /////"),
                 
                 tags$br(),
                 h4("is:"),
                 

                 h5(textOutput("text1") , align = 'center' , style = "color:red"),
                 h5(textOutput("text2") , align = 'center' , style = "color:red"),
                 h5(textOutput("text3") , align = 'center' , style = "color:red"),
                 
                 textOutput("list2items"),
                 tags$br(),
                 tags$hr(color="grey" , WIDTH="80%" , height="10" ), 
                 
                 h4("Summary table:"),
                 dataTableOutput("matchTable")),
        
        tabPanel('Instructions',
                 h3("The most probable next word is:")),
        
        tabPanel('Design',
                 h3("The most probable next word is:"))
      )
      
    )
    )
))
