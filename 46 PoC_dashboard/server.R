# обязательно в UTF-8
library(shiny)
library(shinydashboard)
#library(ggplot2)
#library(ggmap)


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)