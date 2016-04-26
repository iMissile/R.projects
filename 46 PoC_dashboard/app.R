# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8
library(shiny)
library(shinydashboard)
library(shinythemes)
library(magrittr)
library(leaflet)
library(readr) #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/

# загрузим данные
ifilename <- ".\\data\\tsensors.csv"
# подгружаем данные по сенсорам
mydata <- read_delim(ifilename, delim = ",", quote = "\"",
                     col_names = TRUE,
                     locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"), # таймзону, в принципе, можно установить здесь
                     # col_types = list(date = col_datetime(format = "%d.%m.%Y %H:%M")), 
                     progress = interactive()
) # http://barryrowlingson.github.io/hadleyverse/#5


ui <- dashboardPage(
  skin = "green",
  # theme = shinytheme("cerulean"),
  dashboardHeader(title = "Мониторинг"),
  
  dashboardSidebar(
    sliderInput("obs", "Slider input:", 1, 100, 50)
  ),
  
  dashboardBody(
    fluidRow(
      box(column(12, plotOutput("loadPlot"))),
      #
      box(column(12, leafletOutput("mymap")))
    ),
    box(dataTableOutput("dtable"), width = 12)
  )
)

server <- function(input, output) { 
  output$loadPlot <- renderPlot({
      hist(rnorm(input$obs), col = 'darkgray', border = 'white')
    })
  
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>% #setup map
    addTiles() %>% #add open street map data
    setView(lng = 37.58, lat = 55.76, zoom = 15)
  })
}

shinyApp(ui, server)
