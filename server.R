#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)

df.uni <- readRDS("unigram.rds")
df.bi <- readRDS("bigram.rds")
df.tri <- readRDS("trigram.rds")
df.quad <- readRDS("quadgram.rds")

source("predict.R", local = TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  result <- reactive({
    predict.function(input$userText)
  })
  output$prediction <- renderPrint(result()[1])
  output$text <- renderText(result()[2])

  
})
