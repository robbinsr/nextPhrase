
shinyUI(fluidPage(
  
  
  sidebarLayout(
    sidebarPanel(
      tags$h3("Data Source:"),
      #uiOutput("num_2_ui"),
      radioButtons("data_source", "", c("Blogs"='blogs', "News"="news","Twitter"="twitter"), selected="twitter"),width=3
      
    ),
    mainPanel(
      HTML("<br>"),
      tags$h3("Next Word for Next Phrase:"),
      tabsetPanel(
        tabPanel("Instructions",
                 HTML("<br>"),
                 tags$h4("Instructions:"),
 
                 tags$ol(
                   HTML("<br>"),
                   
                   tags$li("Choose whether you want to enter 1, 2, 3, or 4 words by selecting a tab."), 
                   tags$li("Choose whether you would like sequences of words completed by using data from blogs, news, or Twitter tweets."), 
                   tags$li("Enter a word or words into the text box on the tabbed page you chose above. No need to hit enter or a submit button.")
                 )),
                 tabPanel("One Word", sidebarPanel(textInput("bword", "Enter one word:", "the"),width=4), mainPanel(uiOutput('biGrams'))),
                 tabPanel("Two Words", sidebarPanel(textInput("twords", "Enter two Words:", "the best"),width=4), mainPanel(uiOutput('triGrams'))),
                 tabPanel("Three Words", sidebarPanel(textInput("qwords", "Enter three words:", "the best of"),
                                                      width=4), mainPanel(uiOutput('quadGrams'))),
                 tabPanel("Four Words", sidebarPanel(textInput("pwords", "Enter four words:", "the best of the"),
                                                     width=4), mainPanel(uiOutput('pentaGrams'))),
                 tabPanel("Purpose", HTML("<br>"),tags$h4("Purpose:"), 
                          HTML("<br>"),
                          tags$ol(
                            
                            
                            tags$li("This app shows how an algorithm and associated data might serve as the basis for a new SwiftKey user interface."), 
                            tags$li("The new user interface would present three alternative phrases, based upon a user\'s choices of previous words."), 
                            tags$li("Words that have not been selected by a user would be predicted based upon their frequency likelihood given the words already selected.")
                          )
                           
                          
                          
                          )
                )
                )
            )
            )
            )  