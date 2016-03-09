library(shiny)
library(leaflet)

# две кнопки описаны в [Pattern 3 - Dueling buttons](http://shiny.rstudio.com/articles/action-buttons.html)

source("IoT_funcs.R")

shinyUI(fluidPage(
  titlePanel("Данные по использованию интернет-канала"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("lastDay", label = "За последние сутки"),
      # helpText(""),
      p(""),
      actionButton("lastWeek", label = "За последнюю неделю"),
      p(""),
      textOutput("text1")
    ),
    
    # Show a plot of the traffic load
    mainPanel(
      fluidRow(
      column(11, plotOutput("loadPlot")),
      #
      column(1, plotOutput("loadPlot2"))
      ),
      leafletOutput("mymap")
    )  
      # https://rstudio.github.io/leaflet/shiny.html
      
  )
))
