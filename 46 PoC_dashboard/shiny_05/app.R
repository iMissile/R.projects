# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8

# задаем фиксированный порт для shiny (http://shiny.rstudio.com/reference/shiny/latest/runApp.html)
#options(shiny.host = "127.0.0.1")
# options(shiny.port = 7775)
# options(shiny.trace = TRUE)
# options(shiny.error = browser)

library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(magrittr)
#library(leaflet)
library(readr) #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/
library(DT)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(scales)
library(ggmap)
library(dplyr)
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

# ================ первичная загрузка данных =========================
raw_field.df <- load_field_data()
raw_github_field.df <- load_github_field_data()
raw_weather.df <- load_weather_data()

# ================================================================
ui <- fluidPage(theme = shinytheme("united"), titlePanel("Контроль влажности"),
                sidebarLayout(
                  sidebarPanel(
                    radioButtons(
                      "objectInput",
                      "Поле",
                      choices = c("Картофель 1", "Капуста 1"),
                      selected = "Картофель 1"
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
                    selectInput(
                      "daysDepth",
                      "Глубина истории (дни)",
                      choices = c(1, 3, 7),
                      selected = 7
                    ),
                    selectInput(
                      "timeBin",
                      "Период группировки (часы)",
                      choices = c(1, 2, 3, 4, 6, 12),
                      selected = 4
                    ),
                    width = 2 # обязательно ширины надо взаимно балансировать!!!!
                  ),
                  
                  mainPanel(
                    fluidRow(
                             column(5, plotOutput('map_plot')), # , height = "300px"
                             # column(7, plotOutput('data_plot'))), # , height = "300px"
                             column(7, plotOutput('temp_plot'))), # , height = "300px"
                    fluidRow(
                             # column(5, plotOutput('weather_plot')),
                             column(5, DT::dataTableOutput('data_tbl')),
                             column(7, plotOutput('weather_plot'))),
                    width = 10 # обязательно ширины надо взаимно балансировать!!!!
                   )
                ))


server <- function(input, output, session) {
  
  # создаем инстанс текущих данных
  # data.frame -- подмножество для анализа и отображения
  rvars <- reactiveValues() 
  # Anything that calls autoInvalidate will automatically invalidate every 5 seconds.
  # See:  http://shiny.rstudio.com/reference/shiny/latest/reactiveTimer.html
  # Also: http://rpackages.ianhowson.com/cran/shiny/man/reactiveTimer.html
  autoInvalidate <- reactiveTimer(1000 * 60, session) # раз в минуту

#  observe({
#    rvars$should_update <- rvars$should_update + 1 # поставили флаг на обновление данных
#  })
  
  observe({
    # в одном месте следим и за таймером и за нажатием на кнопку
    # Invalidate and re-execute this reactive expression every time the timer fires.
    autoInvalidate()
    # смотрим, требуется ли обновление данных
    flog.info(paste0("autoInvalidate. ", input$update_btn, " - ", Sys.time()))

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
  
  output$data_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    flog.info(paste0(input$update_btn, ": data_plot")) # формально используем
    p1 <- plot_average_ts_data(raw_field.df, input$daysDepth)
    grid.arrange(p1, ncol = 1)
  })
  
  output$temp_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    flog.info(paste0(input$update_btn, ": temp_plot")) # формально используем
    # параметры select передаются как character vector!!!!!!!!
    plot_github_ts2_data(raw_github_field.df, as.numeric(input$daysDepth), as.numeric(input$timeBin))
    # invalidateLater(5000, session) # обновляем график раз в 5 секунд
  })

  output$weather_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    flog.info(paste0(input$update_btn, ": weather_plot")) # формально используем  
    # параметры select передаются как character vector!!!!!!!!
    plot_weather_data(raw_weather.df, as.numeric(input$daysDepth))
  })
  
  # виджет текущей погоды
  output$cweather_plot <- renderPlot({
    # на выходе должен получиться ggplot!!!
    invalidateLater(1000 * 5) # обновляем в автономном режиме раз в N минут
    plot_cweather()
    # plot_cweather_scaled()
  })
  
  output$data_tbl <- DT::renderDataTable(
    DT::datatable(raw_github_field.df %>% select(-lon, -lat, -location),
    options = list(lengthChange = FALSE, pageLength = 6)) %>%
    formatDate('timestamp', method = "toLocaleString") # см. https://rstudio.github.io/DT/functions.html 
    )

  output$map_plot <- renderPlot({
    
    slicetime <- now()
    #slicetime <- dmy_hm("29.04.2016 5:00", tz = "Europe/Moscow")
    input.df <- raw_field.df
    input.df <- raw_github_field.df
    
    df <- input.df %>%
      filter(timestamp <= slicetime) %>%
      group_by(name) %>%
      filter(timestamp == max(timestamp)) %>%
      mutate(delta = round(as.numeric(difftime(slicetime, timestamp, unit = "min")), 0)) %>%
      arrange(name) %>%
      ungroup() %>%
      # рабочий статус также определяется тем, насколько давно мы видели показания от конкретного сенсора
      mutate(work.status = (delta < 60))
    
    
    # откатегоризируем
    df <- within(df, {
      level <- NA
      level[value >= 0 & value <= 33] <- "Low"
      level[value > 33  & value <= 66] <- "Normal"
      level[value > 66  & value <= 100] <- "High"
    })
    
    # при группировке по Lev по умолчанию, порядок следования строк осуществляется по алфавиту
    # ggplot(sensors.df, aes(x = lat, y = lon, colour = level)) + 
    #  geom_point(size = 4)
    
    # пытаемся изменить группировку
    # http://docs.ggplot2.org/current/aes_group_order.html
    # сделаем из текстовых строк factor и их принудительно отсортируем
    # http://www.ats.ucla.edu/stat/r/modules/factor_variables.htm
    
    # у нас два критерия разделения -- диапазон значений и работоспособность.
    # диапазон измерений -- цвет, работоспособность -- форма
    
    sensors.df <- df %>%
      rename(level.unordered = level) %>%
      # mutate(lev.of = ordered(lev, levels = c('Low', 'Normal', 'High'))) %>%
      mutate(level = factor(level.unordered, levels = c('High', 'Normal', 'Low'))) %>%
      mutate(work.status = work.status & !is.na(level)) # что не попало в категорию также считается нерабочим
    
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
