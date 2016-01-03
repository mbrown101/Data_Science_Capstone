# ui.R

shinyUI(fluidPage(
    titlePanel("Coursera Data Science Project"),

    sidebarLayout(
        sidebarPanel(
        
             helpText("This project seeks to estimate the next work in a string based on n-gram analysis."),
      
             textInput(inputId = "stringInput", 
                       label = "Enter string to guess next word",
                       value = "", 
                       width = NULL),
             
             sliderInput("sliderGuesses", 
                          label = "Guesses to be returned",
                          min = 1, max = 10, value = 3 , step = 1)
             ),
    
    mainPanel(
        h3("The most probable next word is:"),
        h5(textOutput("text1") , align = 'center' , style = "color:red"),
        h2(" "),   
        h2(" "),
        h2(" "),

        
        dataTableOutput("matchTable")
    )
    )
))
