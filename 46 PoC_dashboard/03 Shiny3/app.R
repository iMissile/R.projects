# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8
library(shiny)
library(magrittr)
#library(leaflet)
library(readr) #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/
library(ggmap)
library(DT)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(ggthemes)
library(ggmap)
library(RColorBrewer)
library(gtable)
library(grid) 


source("..\\common_funcs.R") # сюда выносим все вычислительные и рисовательные функции

# ================ первичная загрузка данных =========================
raw.df <- reload_raw_data()

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
                    selectInput(
                      "daysDepth",
                      "Глубина истории (дни)",
                      choices = c(1, 3, 7),
                      selected = 3
                    ),
                    strong("Время последнего обновления"),
                    textOutput("time_updated"),
                    width = 2 # обязательно ширины надо взаимно балансировать!!!!
                  ),
                  
                  mainPanel(
                    fluidRow(column(6, plotOutput('map_plot')),
                             column(6, plotOutput('data_plot'))),
                    width = 10 # обязательно ширины надо взаимно балансировать!!!!
                  )
                ))


server <- function(input, output, session) {
  
  # создаем инстанс текущих данных
  rvars <- reactiveValues(work.df = raw.df # data.frame -- подмножество для анализа и отображения
                          ) 
  # Anything that calls autoInvalidate will automatically invalidate every 5 seconds.
  # See:   http://shiny.rstudio.com/reference/shiny/latest/reactiveTimer.html
  autoInvalidate <- reactiveTimer(5000, session)

  observe({
    # Invalidate and re-execute this reactive expression every time the
    # timer fires.
    autoInvalidate()
    
    # подгрузим данные
    raw.df <- reload_raw_data()
    
    # отобразили время последнего обновления
    output$time_updated <- renderText({ 
      paste0(Sys.time())
    })
    
    
  })
    
  observeEvent(input$daysDepth, {
    # делаем выборку данных на заданную глубину
    rvars$work.df <- raw.df %>%
      filter(timegroup < lubridate::now()) %>%
      filter(timegroup > lubridate::now() - days(input$daysDepth))
  })
  
  output$data_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    # делаем выборку данных

    p1 <- plot_ts_data(rvars$work.df)
    
    grid.arrange(p1, p1, ncol = 1)
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
  
  # временно отключим для ускорения
  # output$data_tbl <- DT::renderDataTable(
  output$data_tbl1 <- DT::renderDataTable(
    rvars$work.df, options = list(lengthChange = FALSE))
  
  
}

shinyApp(ui = ui, server = server)
