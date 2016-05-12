# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8

# задаем фиксированный порт для shiny (http://shiny.rstudio.com/reference/shiny/latest/runApp.html)
options(shiny.port = 7775)

library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(magrittr)
#library(leaflet)
library(readr) #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/
library(ggmap)
# library(DT)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(scales)
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


# source("../common_funcs.R") # сюда выносим все вычислительные и рисовательные функции

# это вместо source
# How to source() .R file saved using UTF-8 encoding?
# http://stackoverflow.com/questions/5031630/how-to-source-r-file-saved-using-utf-8-encoding
eval(parse("../common_funcs.R", encoding="UTF-8"))

# ================ первичная загрузка данных =========================
raw_field.df <- load_field_data()
raw_github_field.df <- load_github_field_data()
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
                    p(),
                    strong("Текущая погода"),
                    plotOutput('cweather_plot', height = "200px"),
                    p(),
                    actionButton("update_btn", "Обновить данные сенсоров"),
                    width = 2 # обязательно ширины надо взаимно балансировать!!!!
                  ),
                  
                  mainPanel(
                    fluidRow(
                             column(5, plotOutput('map_plot1')), # , height = "300px"
                             column(7, plotOutput('data_plot'))), # , height = "300px"
                    fluidRow(
                             column(5, plotOutput('weather_plot')),
                             column(7, plotOutput('temp_plot'))),
                    width = 10 # обязательно ширины надо взаимно балансировать!!!!
                   )
                ))


server <- function(input, output, session) {
  
  # создаем инстанс текущих данных
  # data.frame -- подмножество для анализа и отображения
  rvars <- reactiveValues(work_field.df = NA, #raw_field.df,
                          work_weather.df = NA, #raw_weather.df,
                          work_github_field.df = NA #raw_field.df
                          ) 
  # Anything that calls autoInvalidate will automatically invalidate every 5 seconds.
  # See:  http://shiny.rstudio.com/reference/shiny/latest/reactiveTimer.html
  # Also: http://rpackages.ianhowson.com/cran/shiny/man/reactiveTimer.html
  autoInvalidate <- reactiveTimer(5000, session)

#  observe({
#    rvars$should_update <- rvars$should_update + 1 # поставили флаг на обновление данных
#  })
  
  observe({
    # в одном месте следим и за таймером и за нажатием на кнопку
    # Invalidate and re-execute this reactive expression every time the timer fires.
    autoInvalidate()
    # смотрим, требуется ли обновление данных
    print(paste0(input$update_btn, " - ", Sys.time()))

    # подгрузим данные
    raw_field.df <- load_field_data()
    raw_weather.df <- load_weather_data()

    # берем лабораторные данные с github
    df <- load_github_field_data()
    if (!is.na(df)) { raw_github_field.df <- df}    
    
    # принудительно меняем 
    # отобразили время последнего обновления
    output$time_updated <- renderText({ 
      paste0(Sys.time())
    })
  })
  
  #observeEvent(input$update_btn, {
  #  # нажали на кнопку "обновить исходные данные", напрямик не делаем
  #  rvars$should_update <- rvars$should_update + 1 # поставили флаг на обновление данных
  #})  
  
  observeEvent(input$daysDepth, {
    # делаем выборку данных на заданную глубину
      rvars$work_field.df <- raw_field.df %>%
        filter(timegroup < lubridate::now()) %>%
        filter(timegroup > floor_date(lubridate::now() - days(input$daysDepth), unit = "day"))
      
      rvars$work_weather.df <- raw_weather.df %>%
        filter(timegroup > floor_date(lubridate::now() - days(input$daysDepth), unit = "day"))
      
      rvars$work_github_field.df <- raw_github_field.df %>%
        filter(timestamp < lubridate::now()) %>%
        filter(timestamp > floor_date(lubridate::now() - days(input$daysDepth), unit = "day"))
      # print(paste("rvars$work_weather.df", rvars$work_weather.df))
    
  })
  
  output$data_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    # делаем выборку данных
    p1 <- plot_average_ts_data(rvars$work_field.df)
    grid.arrange(p1, ncol = 1)
  })
  
  output$temp_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    plot_github_ts_data(rvars$work_github_field.df)
    # invalidateLater(5000, session) # обновляем график раз в 5 секунд
  })

    output$weather_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    # делаем выборку данных по состоянию на текущий момент и по установленной глубине выборки
    plot_weather_data(rvars$work_weather.df)
  })
  
  output$cweather_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    p1 <- plot_ts_data_old(rvars$work_field.df)
    p1
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
# http://shiny.rstudio.com/reference/shiny/latest/runApp.html
# app <- shinyApp(ui = ui, server = server)
# runApp(app, port = "8080")
