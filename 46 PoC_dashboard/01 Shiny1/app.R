# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8
library(shiny)
library(magrittr)
library(leaflet)
library(readr) #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/

# загрузим данные
ifilename <- "..\\.\\data\\tsensors.csv"
# подгружаем данные по сенсорам
mydata <- read_delim(ifilename, delim = ",", quote = "\"",
                     col_names = TRUE,
                     locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"), # таймзону, в принципе, можно установить здесь
                     # col_types = list(date = col_datetime(format = "%d.%m.%Y %H:%M")), 
                     progress = interactive()
) # http://barryrowlingson.github.io/hadleyverse/#5


ui <- fluidPage(
  titlePanel("Agricola Dashboard"), 
  sidebarLayout(),
  mainPanel()
)

server <- function(input, output) { 
}

shinyApp(ui = ui, server = server)
