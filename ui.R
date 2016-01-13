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
                    #textOutput("formed1G"),
                    #textOutput("formed2G"),
                    #textOutput("formed3G"),
                    #textOutput("formed4G"),
                    #textOutput("tailTest"),
                 #h4("/////terms above /////"),
                 
                 h4("/////words Below /////"),
                 textOutput("w_1"),
                 textOutput("w_2"),
                 textOutput("w_3"),
                 textOutput("w_4"),
                 h4("/////words above /////"),
                 
                 
                 
                 tags$br(),
                 h4("lookups are:"),
                 
                 h5(textOutput("gram2") , align = 'center' , style = "color:red"),
                 h5(textOutput("gram3") , align = 'center' , style = "color:red"),
                 h5(textOutput("gram4") , align = 'center' , style = "color:red"),
                 h5(textOutput("gram5") , align = 'center' , style = "color:red"),
                 h5(textOutput("mesh") , align = 'center' , style = "color:red"),
               
      
                 h4("length of tail_2:"),
                 textOutput("length2tail"),
                 tags$br(),
                 tags$hr(color="grey" , WIDTH="80%" , height="10" ), 
                 
                 h4("mesh tables:"),
                 dataTableOutput("tabmesh"),
                 dataTableOutput("tabmesh2"),
                 
                 h4("Summary tables:"),
                 dataTableOutput("matchTable2"),
                 dataTableOutput("matchTable3"),
                 dataTableOutput("matchTable4"),
                 dataTableOutput("matchTable5")),
        
        tabPanel('Instructions',
                 h3("The most probable next word is:")),
        
        tabPanel('Design',
                 h3("The most probable next word is:"))
      )
      
    )
    )
))
