# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8
rm(list=ls()) # очистим все переменные

library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)
library(stringi)
library(Cairo)
library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyBS)
library(shinyjs)
library(config)
library(DBI)
library(RPostgreSQL)
library(futile.logger)
library(tictoc)

## ---------
# library(RColorBrewer)
# library(gtable)
# library(grid) # для grid.newpage()
# library(gridExtra) # для grid.arrange()
# library(curl)
# library(httr)
# library(jsonlite)
# library(arules)
# library(htmltools)

options(shiny.usecairo=TRUE)
options(shiny.reactlog=TRUE)

eval(parse("funcs.R", encoding="UTF-8"))

# ================================================================
ui <- 
  navbarPage("DVT IoT",
  title=HTML('<div><a href="http://devoteam.com/"><img src="./img/devoteam_176px.png" width="80%"></a></div>'),
  # windowTitle="CC4L",
  collapsible=TRUE,
  id="tsp",
  theme=shinytheme("flatly"),
  # shinythemes::themeSelector(),
  
  # includeCSS("styles.css"),
  
  tabPanel("Поле", value="field"),
  tabPanel("About", value="about"),
  
  
  # titlePanel("Контроль орошения полей"),
  # ----------------
  conditionalPanel(
    "input.tsp=='field'",
    fluidRow(
      tags$style(type='text/css', '#cweather_text {white-space:pre;}'),
      # tags$style(type='text/css', 'div {background-color: #000с00;}'), 
      
      column(6, h2("Типовая форма"), h3(textOutput("cweather_text", inline=TRUE))),
      column(6,
             fluidRow(
               column(4, selectInput("predict_days", "Горизонт прогноза (дни)",
                                     choices = c(1, 2, 3, 5), selected = 2)),
               column(4, selectInput("time_bin", "Группировка (часы)",
                                     choices = c(0.5, 1, 2, 3, 4, 6, 12), selected = 1))
               )
             )
      ),
    
    fluidRow(
        column(6, textOutput('info_text')), # 
        column(6, plotOutput('weather_plot', height = "300px")) # , height = "300px"
      ),
    
    fluidRow(
      column(2, uiOutput("choose_segment")),
      column(2, uiOutput("choose_region")),
      column(2, dateRangeInput("date_range",
                               label = paste('Date range input 2: range is limited,',
                                             'dd/mm/yy, language: fr, week starts on day 1 (Monday),',
                                             'separator is "-", start view is year'),
                               start = Sys.Date() - 3, end = Sys.Date() + 3,
                               min = Sys.Date() - 10, max = Sys.Date() + 10,
                               separator = " - ", format = "dd/mm/yy",
                               startview = "month", language = 'ru', weekstart=1)
      ), 
      column(2, selectInput("history_days", "Глубина истории (дни)", 
                            choices = c(0, 1, 3, 5, 7), selected = 5)),
    )    
  )
)

# ================================================================
server <- function(input, output, session) {
  # статические переменные ------------------------------------------------
  log_name <- "app.log"
  
  flog.appender(appender.tee(log_name))
  flog.threshold(TRACE)
  flog.info("App started")

  # пока подгружаем 1 раз
  raw_df <- {
    system.time(df <- readRDS("./data/tvstream4.rds"))
    flog.info(paste0("Loaded ", nrow(df), " rows"))
    
    df
  }  
  
  # реактивные переменные -------------------
  
  cur_df <- reactive({
    req(raw_df)
  })
  
  msg <- reactiveVal("")

  field_df <- reactive({

  })  
  
  raw_weather_df <- reactive({
  })  
  
  rain_df <- reactive({
  })  

  weather_df <- reactive({
  })  
  
  output$cweather_plot <- renderPlot({
  })

  output$cweather_text <- renderText({
  })

  output$temp_plot <- renderPlot({
  })

  output$info_text <- renderPlot({
  })
  
  output$data_tbl <- DT::renderDataTable({
    })


  # служебный вывод ---------------------  
  output$info_text <- renderText({
    msg()
  })

  # динамический выбор типа технологии передачи данных (сегмента) ---------
  output$choose_segment <- renderUI({
    # выделим уникальные типы для выбранного множества данных
    data <- cur_df() %>% distinct(segment) %>% arrange(segment)
    # names(data) <- NULL
    flog.info(paste0("Dynamic segment list ",  capture.output(str(data))))
    
    msg(capture.output(dput(data)))
    
    # создадим элемент
    selectInput("segment_filter", "Сегмент", choices=as.list(data))
  })

    # динамический выбор региона ---------
  output$choose_region <- renderUI({
    # выделим уникальные типы для выбранного множества данных
    data <- cur_df() %>% distinct(region) %>% arrange(region)
    # names(data) <- NULL
    flog.info(paste0("Dynamic region list ",  capture.output(str(data))))
    
    msg(capture.output(dput(data)))
    
    # создадим элемент
    selectInput("region_filter", "Регион", choices=as.list(data))
  })
  
}

shinyApp(ui = ui, server = server)
