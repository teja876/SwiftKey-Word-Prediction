#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("SwiftKey Word Prediction"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       helpText("Enter the text to start the next word prediction"),
       textInput("userText", "Enter the Text here", value = ""),
       br(),
       br()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       h2("Predicted next key word"),
       verbatimTextOutput("prediction"),
       strong("Predicted by:"),
       tags$style(type='text/css', '#text1 {background-color: rgba(255,255,0,0.40); color: blue;}'), 
       textOutput('text')
    )
  )
))
