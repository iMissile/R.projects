# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8
library(shiny)
library(magrittr)
#library(leaflet)
library(readr) #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/
library(ggmap)
# library(DT)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(ggthemes)
library(ggmap)
library(RColorBrewer)
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()


source("..\\common_funcs.R") # сюда выносим все вычислительные и рисовательные функции

# ================ первичная загрузка данных =========================
raw_field.df <- load_field_data()
raw_weather.df <- load_weather_data()

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
                    p(),
                    fluidRow(column(6, plotOutput('weather_plot')),
                             column(6, plotOutput('temp_plot'))),
                    width = 10 # обязательно ширины надо взаимно балансировать!!!!
                  )
                ))


server <- function(input, output, session) {
  
  # создаем инстанс текущих данных
  # data.frame -- подмножество для анализа и отображения
  rvars <- reactiveValues(work_field.df = raw_field.df,
                          work_weather.df = raw_weather.df
                          ) 
  # Anything that calls autoInvalidate will automatically invalidate every 5 seconds.
  # See:   http://shiny.rstudio.com/reference/shiny/latest/reactiveTimer.html
  autoInvalidate <- reactiveTimer(5000, session)

  observe({
    # Invalidate and re-execute this reactive expression every time the
    # timer fires.
    autoInvalidate()
    
    # подгрузим данные
    raw_field.df <- load_field_data()
    raw_weather.df <- load_weather_data()
    
    # отобразили время последнего обновления
    output$time_updated <- renderText({ 
      paste0(Sys.time())
    })
  })
    
  observeEvent(input$daysDepth, {
    # делаем выборку данных на заданную глубину
    rvars$work_field.df <- raw_field.df %>%
      filter(timegroup < lubridate::now()) %>%
      filter(timegroup > lubridate::now() - days(input$daysDepth))
    
    rvars$work_weather.df <- raw_weather.df %>%
      filter(timegroup > lubridate::now() - days(input$daysDepth))
  })
  
  output$data_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    # делаем выборку данных

    p1 <- plot_ts_data(rvars$work_field.df)
    grid.arrange(p1, p1, ncol = 1)
  })
  
  output$weather_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    # делаем выборку данных по состоянию на текущий момент и по установленной глубине выборки

    plot_weather_data(rvars$work_weather.df)
  })
  
  output$map_plot <- renderPlot({
    p <- NULL
    #p <- draw_field_ggmap()
    p
  })
  
  
}

shinyApp(ui = ui, server = server)
