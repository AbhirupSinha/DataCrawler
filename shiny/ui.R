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
  titlePanel("Data Crawler"),
  
  # Sidebar with input fields
  sidebarLayout(
    sidebarPanel
    (
    fileInput("inputfile","Input Filename"),
    #textInput("outputfile","Output Pathname"),
    h3("Social Media to Crawl"),
    checkboxInput("fb","Facebook",TRUE),
    checkboxInput("tw","Twitter",TRUE),
    checkboxInput("rd","Reddit",TRUE),
    checkboxInput("gp","Google Plus",TRUE),
    actionButton("goButton","Start Crawling")
    ),
    # Show a plot of the generated distribution
    mainPanel(
       h5(textOutput("prog")),
       h4(textOutput("textFB")),
       h4(textOutput("textTW")),
       h4(textOutput("textRD")),
       h4(textOutput("textGP")),
       downloadButton("dl","Download Outputs")
       #plotOutput("plot")
  )
  )
))
