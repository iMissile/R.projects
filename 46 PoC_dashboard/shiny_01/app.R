# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8
library(shiny)
library(magrittr)
library(leaflet)
library(readr) #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/
library(ggmap)
library(DT)


# ================ первичная загрузка данных =========================
ifilename <- "..\\.\\data\\test_data.csv"
# подгружаем данные по сенсорам
mydata <- read_delim(ifilename, delim = ",", quote = "\"",
                     col_names = TRUE,
                     locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"), # таймзону, в принципе, можно установить здесь
                     # col_types = list(date = col_datetime(format = "%d.%m.%Y %H:%M")), 
                     progress = interactive()
) # http://barryrowlingson.github.io/hadleyverse/#5

mydata$value <- round(mydata$value, 1)

# ================================================================

ui <- fluidPage(titlePanel("Контроль полива полей"),
                sidebarLayout(
                  sidebarPanel(
                    radioButtons(
                      "objectInput",
                      "Поле",
                      choices = c("Картофель 1", "Капуста 1"),
                      selected = "Картофель 1"
                    ),
                    selectInput("daysDepth", "Глубина истории (дни)",
                                choices = c(1, 3, 7)),
                    width = 3
                  ),
                  
                  mainPanel(fluidRow(
                    column(6, plotOutput('map_plot')),
                    column(6, plotOutput('data_plot'))
                  ),
                  
                  fluidRow(DT::dataTableOutput('data_tbl')))
                ))




server <- function(input, output) {
  output$data_plot <- renderPlot({
    # на выходе должен получиться ggplot
    plot(rnorm(input$daysDepth))
  })
  
  output$map_plot1 <- renderPlot({
    # i <- input$daysDepth
    
    getMap <- get_map(
      enc2utf8("Москва, Зоологическая 2"),
      language = "ru-RU",
      # source = "stamen",
      # maptype = "watercolor", 
      maptype = "terrain",
      zoom = 16
    )
    
    ggmap(getMap, extent="panel")
  })
  
  output$data_tbl <- DT::renderDataTable(
    mydata, options = list(lengthChange = FALSE))
  
  
}

shinyApp(ui = ui, server = server)
