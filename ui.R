library(shiny)
library(shinythemes)
library(DT)
library(datasets)

shinyUI(
  fluidPage(
    theme = shinytheme("flatly"), titlePanel("Your Words"),
    sidebarLayout(
      sidebarPanel("", textInput(inputId = "words_in", label = "Enter up to eight words", value = "the best of the"), radioButtons("data_source", "Data Source:",
                                                                                                           c("Blogs" = "blogs",
                                                                                                             "News" = "news",
                                                                                                             "Twitter" = "twitter"))),
      mainPanel(tags$div(tags$h2("Your Words + Prospective Next Word")), 
        tabsetPanel(
          tabPanel("one prediction",
                   fluidRow(
                     column(12,
                            textOutput('line')
                     ) #close column
                   ) # close fluid row

          ) # close tab panel
        ,
          tabPanel("more than one prediction",
             fluidRow(
               column(12,
                      DT::dataTableOutput('table')
                ) #close column
              ) # close fluid row
            ) # close tab panel

          ) # close tab set panel
        ) # close main panel
      ) # close sidebar layout
    ) # close fluid page
  ) #close shiny ui