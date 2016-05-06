# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8
library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/
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
# library(KernSmooth)
library(akima)
# library(rgl)


source("../common_funcs.R") # сюда выносим все вычислительные и рисовательные функции

# ================ первичная загрузка данных =========================
raw_field.df <- load_field_data()
raw_weather.df <- load_weather_data()

# ================================================================
ui <- fluidPage(theme = shinytheme("united"), titlePanel("Контроль полива полей"),
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
                  
                  # http://stackoverflow.com/questions/25340847/control-the-height-in-fluidrow-in-r-shiny
                  # mainPanel(
                  #   fluidRow(class = "myRow1", 
                  #            column(6, div(style = "height:600px;background-color: yellow;", plotOutput('map_plot'))),
                  #            column(6, div(style = "height:600px;background-color: blue;", plotOutput('data_plot')))),
                  #   p(),
                  #   fluidRow(class = "myRow2",
                  #            column(6, div(style = "height:100px;background-color: green;", plotOutput('weather_plot'))),
                  #            column(6, div(style = "height:150px;background-color: red;", plotOutput('temp_plot')))),
                  #   width = 10, # обязательно ширины надо взаимно балансировать!!!!
                  #   tags$head(tags$style(".myRow1{height:650px;}.myRow2{height:350px;background-color: pink;}"))
                  #   )
                  
                  mainPanel(
                    fluidRow(
                             column(5, plotOutput('map_plot')), # , height = "300px"
                             column(7, plotOutput('weather_plot'))), # , height = "300px"
                    fluidRow(
                             column(5, plotOutput('temp_plot')),
                             column(7, plotOutput('data_plot'))),
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
    
    print(rvars$work_weather.df)
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
    
    slicetime <- now()
    slicetime <- dmy_hm("29.04.2016 5:00", tz = "Europe/Moscow")
    sensors.df <- raw_field.df %>%
      filter(timestamp <= slicetime) %>%
      group_by(name) %>%
      filter(timestamp == max(timestamp)) %>%
      mutate(delta = round(difftime(slicetime, timestamp, unit = "min"), 0)) %>%
      arrange(name)
    
    # откатегоризируем
    sensors.df <- within(sensors.df, {
      level <- NA
      level[value >= 0 & value <= 33] <- "Low"
      level[value > 33  & value <= 66] <- "Average"
      level[value > 66  & value <= 100] <- "High"
    })
    
    p <- draw_field_ggmap(sensors.df)
    p
  })
  
  
}

shinyApp(ui = ui, server = server)
