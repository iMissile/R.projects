# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8

# задаем фиксированный порт для shiny (http://shiny.rstudio.com/reference/shiny/latest/runApp.html)
#options(shiny.host = "127.0.0.1")
# options(shiny.port = 7775)
# options(shiny.trace = TRUE)
# options(shiny.error = browser)
# options(shiny.reactlog = TRUE)

library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(magrittr)
#library(leaflet)
library(DT)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(scales)
library(ggmap)
library(dplyr)
library(tidyr)
library(readr)  #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/
library(reshape2)
library(ggthemes)
library(ggdendro) # для пустой темы
library(wesanderson) # https://github.com/karthik/wesanderson
library(RColorBrewer)
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()
# library(KernSmooth)
library(akima)
library(curl)
library(httr)
library(jsonlite)
library(arules)
library(futile.logger)

# library(rgl)
# настраиваем кастомный логгер
# t <- paste0("iot_", format(now(), "%Y%m%d_%H%M%S"), ".log")
# flog.appender(appender.file(t), name = 'iotlog')
flog.appender(appender.file('iot-dashboard.log'))
flog.threshold(TRACE)
flog.info("PoC dashboard started")

# source("../common_funcs.R") # сюда выносим все вычислительные и рисовательные функции

# это вместо source
# How to source() .R file saved using UTF-8 encoding?
# http://stackoverflow.com/questions/5031630/how-to-source-r-file-saved-using-utf-8-encoding
eval(parse("../common_funcs.R", encoding="UTF-8"))

# ================================================================
ui <- fluidPage(theme = shinytheme("united"), titlePanel("Контроль влажности"),
                sidebarLayout(
                  sidebarPanel(
                    radioButtons(
                      "objectInput",
                      "Поле",
                      choices = c("Картофель", "Капуста"),
                      selected = "Картофель"
                    ),
                    strong("Время последнего обновления"),
                    textOutput("time_updated"),
                    p(),
                    strong("Текущая погода"),
                    plotOutput('cweather_plot', height = "200px"),
                    p(),
                    actionButton("update_btn", "Обновить данные сенсоров"),
                    p(),
                    strong("-------- Debug Zone --------"),
                    checkboxInput(inputId = "sync_graphs",
                                  label = strong("Синхронизация на графиках оси X"),
                                  value = FALSE),
                    checkboxInput(inputId = "expand_y",
                                  label = strong("Расширить ось Y"),
                                  value = FALSE),
                    selectInput(
                      "historyDays",
                      "Глубина истории (дни)",
                      choices = c(0, 1, 3, 5, 7),
                      selected = 0
                    ),
                    selectInput(
                      "predictDays",
                      "Горизонт прогноза (дни)",
                      choices = c(1, 2, 3, 5),
                      selected = 2
                    ),
                    selectInput(
                      "timeBin",
                      "Период группировки (часы)",
                      choices = c(0.5, 1, 2, 3, 4, 6, 12),
                      selected = 0.5
                    ),
                    actionButton("logdata_btn", "Сброс данных в лог"),
                    width = 2 # обязательно ширины надо взаимно балансировать!!!!
                  ),
                  
                  mainPanel(
                    fluidRow(
                             column(5, plotOutput('map_plot1')), # , height = "300px"
                             column(7, plotOutput('temp_plot'))), # , height = "300px"
                    fluidRow(
                             column(5, DT::dataTableOutput('data_tbl1')),
                             column(7, plotOutput('weather_plot'))),
                    width = 10 # обязательно ширины надо взаимно балансировать!!!!
                   )
                ))


server <- function(input, output, session) {
  
  # создаем инстанс текущих данных
  # data.frame -- подмножество для анализа и отображения
  # rvars <- reactiveValues(raw_field.df = get_github_field2_data(),
  #                         raw_weather.df = prepare_raw_weather_data(),
  #                         weather.df = get_weather_df(raw_weather.df), 
  #                         rain.df = calc_rain_per_date(raw_weather.df)) 
  
  rvars <- reactiveValues(raw_field.df = NA,
                          raw_weather.df = NA,
                          weather.df = NA, 
                          rain.df = NA)   # Anything that calls autoInvalidate will automatically invalidate every 5 seconds.
  # See:  http://shiny.rstudio.com/reference/shiny/latest/reactiveTimer.html
  # Also: http://rpackages.ianhowson.com/cran/shiny/man/reactiveTimer.html
  autoInvalidate <- reactiveTimer(1000 * 60, session) # раз в минуту

#  observe({
#    rvars$should_update <- rvars$should_update + 1 # поставили флаг на обновление данных
#  })
  
  observeEvent(input$logdata_btn, {
    flog.info("Сброс глобальных данных")
    flog.info("rvars$raw_field.df")
    flog.info(capture.output(print(head(arrange(rvars$raw_field.df, desc(timestamp)), n = 10))))
    # flog.info("raw_github_field.df")
    # flog.info(capture.output(print(head(arrange(raw_github_field.df, desc(timestamp)), n = 10))))
    flog.info("rvars$weather.df")
    flog.info(capture.output(print(head(arrange(rvars$weather.df, desc(timestamp)), n = 10))))
  })
  
  observe({
    # в одном месте следим и за таймером и за нажатием на кнопку
    # Invalidate and re-execute this reactive expression every time the timer fires.
    autoInvalidate()
    # смотрим, требуется ли обновление данных
    flog.info(paste0("autoInvalidate. ", input$update_btn, " - ", Sys.time()))

    # подгрузим и посчитаем данные по погоде
    # и только, если они хороши, то мы их обновляем для отображения
    temp.df <- prepare_raw_weather_data()
    # NA[[1]] = NA
    if (!is.na(temp.df)[[1]]) {
      rvars$weather.df <- get_weather_df(temp.df)
      rvars$rain.df <- calc_rain_per_date(temp.df)
      # saveRDS(rain.df, "rain.df")
      }

    # берем лабораторные данные с github
    temp.df <- get_github_field2_data()
    # NA[[1]] = NA
    if (!is.na(temp.df)[[1]]) { rvars$raw_field.df <- temp.df } # и только, если они хороши, то мы их обновляем для отображения
    
    # принудительно меняем 
    # отобразили время последнего обновления
    output$time_updated <- renderText({ 
      paste0(Sys.time())
    })
  })
  
  # виджет текущей погоды
  output$cweather_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    invalidateLater(1000 * 60) # обновляем в автономном режиме раз в N минут
    plot_cweather()
    # plot_cweather_scaled()
  })
  
  output$temp_plot <- renderPlot({
    # invalidateLater(5000, session) # обновляем график раз в 5 секунд
    # flog.info(paste0(input$update_btn, ": temp_plot")) # формально используем
    # игнорируем update_btn, используем косвенное обновление, через reactiveValues

    if (is.na(rvars$raw_field.df)[[1]]) rReturn(NULL) # игнорируем первичную инициализацию или ошибки
        
    # параметры select передаются как character vector!!!!!!!!
    # может быть ситуация, когда нет данных от сенсоров. 
    # в этом случае попробуем растянуть данные до последней даты, когда видели показания
    # вперед ставим не 0, иначе округление будет до нижней даты, т.е. до 0:00 текущего дня
    timeframe <- get_timeframe(days_back = as.numeric(input$historyDays),
                               days_forward = ifelse(input$sync_graphs, as.numeric(input$predictDays), 0)) 
    
    flog.info(paste0("sensorts_plot timeframe: ", capture.output(str(timeframe))))
    # на выходе должен получиться ggplot!!!
    plot_github_ts4_data(rvars$raw_field.df, timeframe, as.numeric(input$timeBin), expand_y = input$expand_y)
  })

  output$weather_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    # параметры select передаются как character vector!!!!!!!!
    # browser() 
    if (is.na(rvars$weather.df)[[1]]) return(NULL) # игнорируем первичную инициализацию или ошибки
      
    timeframe = get_timeframe(days_back = as.numeric(input$historyDays),
                              days_forward = as.numeric(input$predictDays))
    
    flog.info(paste0("weather_plot timeframe: ", capture.output(str(timeframe))))
    plot_real_weather2_data(rvars$weather.df, rvars$rain.df, timeframe)
  })
  
  output$data_tbl <- DT::renderDataTable({
    df <- rvars$raw_field.df %>% 
      filter(type == 'MOISTURE') %>%
      select(name, measurement, work.status, timestamp, type) %>% 
      arrange(desc(timestamp))
    # изменим значения на русский
    df$work.status <- ifelse(df$work.status, "Ок", "Неисправен")
    
    DT::datatable(df,
                  # colnames = c('время' = 'timestamp'),
                  colnames = c('# сенсора', 'V', 'статус', 'время', 'тип'), # https://rstudio.github.io/DT/, п.2.4
                  options = list(lengthChange = FALSE, pageLength = 6)) %>%
      formatDate('timestamp', method = "toLocaleString") # см. https://rstudio.github.io/DT/functions.html, https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#Conversion_getter 
    })

  output$map_plot <- renderPlot({
    
    slicetime <- now()
    #slicetime <- dmy_hm("29.04.2016 5:00", tz = "Europe/Moscow")
    # input.df <- raw_field.df.old
    input.df <- rvars$raw_field.df
    
    sensors.df <- prepare_sensors_mapdf(input.df, slicetime)
    
    flog.info("sensors.df")
    flog.info(capture.output(print(sensors.df)))
    gm <- draw_field_ggmap(sensors.df, heatmap = FALSE)
    # benchplot(gm)
    gm
  })

}

shinyApp(ui = ui, server = server)
# http://shiny.rstudio.com/reference/shiny/latest/runApp.html
# app <- shinyApp(ui = ui, server = server)
# runApp(app, port = "8080")
