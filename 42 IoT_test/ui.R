library(shiny)

# две кнопки описаны в [Pattern 3 - Dueling buttons](http://shiny.rstudio.com/articles/action-buttons.html)

source("IoT_funcs.R")

shinyUI(fluidPage(
  titlePanel("Данные по использованию интернет-канала"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("lastDay", label = "За последние сутки"),
      # helpText(""),
      p(""),
      actionButton("lastWeek", label = "За последнюю неделю")
    ),
    
    # Show a plot of the traffic load
    mainPanel(
      plotOutput("loadPlot"),
      textOutput("text1")
  )
)))
