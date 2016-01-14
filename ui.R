# ui.R



shinyUI(fluidPage(theme = "bootstrap.css",
    titlePanel("Coursera Data Science Project"),

    sidebarLayout(
        sidebarPanel(
        
             helpText("This project returns the most probable word following the words input in the text box below. Words should be separated by a single space."),
            
             tags$br(),
             
             textInput(inputId = "stringInput", 
                       label = "Enter string to guess next word",
                       value = "", 
                       width = NULL),
             
             tags$br(),
             
             sliderInput("sliderGuesses", 
                          label = "Number of matches returned in the summary table",
                          min = 1, max = 8, value = 4 , step = 1)
             
             
             ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel('Results',
                 
                 h4("The most probable next word is:"),
                 #textOutput("textWords"),

                 #textOutput("text2Gram"),
                 #textOutput("text3Gram"),
        
                 
                 #h4("/////terms Below /////"),
                    #textOutput("formed1G"),
                    #textOutput("formed2G"),
                    #textOutput("formed3G"),
                    #textOutput("formed4G"),
                    #textOutput("tailTest"),
                 #h4("/////terms above /////"),
                 
                 #h4("/////words Below /////"),
                 #textOutput("w_1"),
                 #textOutput("w_2"),
                 #textOutput("w_3"),
                 #textOutput("w_4"),
                 #h4("/////words above /////"),
                 
                 
                 
                 tags$br(),
                 tags$br(),
                 #h4("the most probable next word is:"),
                 
                 #h5(textOutput("gram2") , align = 'center' , style = "color:red"),
                 #h5(textOutput("gram3") , align = 'center' , style = "color:red"),
                 #h5(textOutput("gram4") , align = 'center' , style = "color:red"),
                 #h5(textOutput("gram5") , align = 'center' , style = "color:red"),
                 h3(textOutput("mesh") , align = 'center' , style = "color:red"),
               
      
                 #h4("length of tail_2:"),
                 #textOutput("length2tail"),
                 tags$br(),
                 tags$br(),
                 tags$br(),
                 tags$hr(color="grey" , WIDTH="100%" , height="10" ), 

                 
                 h4("Summary table:"),

                 #dataTableOutput("matchTable2"),
                 #dataTableOutput("matchTable3"),
                 #dataTableOutput("matchTable4"),
                 #dataTableOutput("matchTable5")
                 dataTableOutput("tabmesh")),
        
        tabPanel('Instructions',
                 tags$br(),
                 h5("This project returns the most probable word following the words input in the text box below. Words should be separated by a single space.")),
        
        tabPanel('Design',
                 tags$br(),
                 h5("This shiny app uses a randomly sampled corpus of three input texts.  2, 3, 4 and 5-grams are then formed.  Each of the grams are split into a trailing gram (the last term of the gram) and a leading gram (all the words to the left of the last word.)  "))
      )
      
    )
    )
))


