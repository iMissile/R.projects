library(shiny)
library(ggmap)
shinyUI(fluidPage(
  titlePanel("Shiny Map"),
  sidebarLayout (
    sidebarPanel(
      textInput("address", label=h3("Location"),
                value="" ),          
      submitButton("Search"),
      sliderInput("zoom", label = h3("zoom"),
                  min=5, max=20, value=10)
    ),
    
    mainPanel(
      plotOutput("map", width="800px", height="800px")
    )  
  )))