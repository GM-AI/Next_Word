#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(conflicted)
conflict_prefer("box", "shinydashboard")

header <- dashboardHeader(
  title = "GM Next Word App"
)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
  fluidRow(
    box(textAreaInput("input", "Enter text here", "Life is good", width = NULL), width = NULL,background = "black")
    #box(verbatimTextOutput("value"))
  ),
  fluidRow(
     
     box(title="Predicted by \"Fast one\"", verbatimTextOutput("predicted1"),width = 6,  height = 250),
    box(title="Other predictions",verbatimTextOutput("head1", placeholder = TRUE),width = 6, height = 250)
     
   ),
  fluidRow(
    
    box(title="Predicted by \"Smart one\"", verbatimTextOutput("predicted2"),width = 6,  height = 250),
    box(title="Other predictions",verbatimTextOutput("head2", placeholder = TRUE),width = 6, height = 250)
    
  ),
  fluidRow(
    tabBox(
      id = "Tabs", height = "180px",
      tabPanel("Instructions", "1. Enter at least 2 words to get predictions",br(),"2. Use predicted word or check other predictions"),
      tabPanel("How it works", "Predictions are made by using models build from provided capstone text data set and additional text data from Leipzig Corpora Collection by the Institute of Computer Science at Leipzig University.", 
               br(), "After data initial exploration text datasets were tokenised to sentences, cleaned and and converted to ngrams. Ater getting ngrams (1 to 4) they were converted tranformed to the document feature matrix, 
               that was colapsed to get ngram frequency dataset. To be ablet to account for unseen words Good Turing smoothing was applied to get adjusted ngram chance.", br(), "To draw predictions with \"Fast one\"  simple back-off is used.",
                br(),"To draw predictions  with \"Smart one\" Katz's back-off is used."),
      tabPanel("About project", "Final projet by G.M. for Data Science Capstone by Johns Hopkins University"),
      width = NULL
    )  

  )
)


dashboardPage(header, sidebar, body)