library(shiny)

source("IoT_funcs.R")

shinyUI(fluidPage(
  titlePanel("Данные по использованию интернет-канала"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("lastDay", label = "За последние сутки"),
      helpText(""),
      actionButton("lastWeek", label = "За последнюю неделю")
      
    ),
    
    # Show a plot of the traffic load
    mainPanel(
      plotOutput("loadPlot")
  )
)))
