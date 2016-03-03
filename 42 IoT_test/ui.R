library(shiny)

shinyUI(fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(
    sidebarPanel( "sidebar panel"),
    
    # Show a plot of the traffic load
    mainPanel("main panel")
  )
))
