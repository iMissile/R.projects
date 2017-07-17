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
  
  tabPanel("МТС", value="field"),
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
      column(2, uiOutput("choose_segment")),
      column(2, uiOutput("choose_region")),
      column(2, dateRangeInput("in_date_range",
                               label = "Диапазон дат",
                               start = Sys.Date() - 3, end = Sys.Date(),
                               # min = Sys.Date() - 10, 
                               max = Sys.Date(),
                               separator = " - ", format = "dd/mm/yy",
                               startview = "month", language = 'ru', weekstart=1)
      ), 
      column(2, selectInput("history_depth", "Глубина истории", 
                            choices = c("1 месяц"=30, "2 недели"=14,
                                        "1 неделя"=7, "3 дня"=3), selected=30))
    ),
    fluidRow(
      column(6, textOutput('info_text')), # 
      column(6, plotOutput('weather_plot', height = "300px")) # , height = "300px"
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
    flog.info(paste0("Time range [", min(df$timestamp), "; ", max(df$timestamp), "]"))
    
    as_tibble(df)
  }  
  
  # реактивные переменные -------------------
  
  cur_df <- reactive({
    # browser()
    flog.info(paste0("Applied time filter [", input$in_date_range[1], "; ", input$in_date_range[2], "]"))
    t <- req(raw_df) %>%
      mutate(date=anydate(timestamp))
    
    t %<>%
      filter(input$in_date_range[1] < date  & date < input$in_date_range[2])
    
    # browser()

    t
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

  
  # динамическое управление диапазоном дат ---------
  observeEvent(input$history_depth, {
    # browser();
    # почему-то $history_depth получаем как строку
    date <- Sys.Date()-as.numeric(input$history_depth)
    flog.info(paste0("Start date changed to  ", date))
    updateDateRangeInput(session, "in_date_range", start=date)
   }
  )
  
  # служебный вывод ---------------------  
  output$info_text <- renderText({
    msg()
  })

  # динамический выбор типа технологии передачи данных (сегмента) ---------
  output$choose_segment <- renderUI({
    # выделим уникальные типы для выбранного множества данных (вектор)
    data <- cur_df() %>% distinct(segment) %>% arrange(segment) %>% pull(segment)
    # browser()
    # names(data) <- NULL
    flog.info(paste0("Dynamic segment list ",  capture.output(str(data))))
    
    msg(capture.output(dput(data)))
    
    # создадим элемент
    selectInput("segment_filter", 
                paste0("Сегмент (", length(data), ")"), 
                choices=data)
  })

    # динамический выбор региона ---------
  output$choose_region <- renderUI({
    # выделим уникальные типы для выбранного множества данных
    data <- cur_df() %>% distinct(region) %>% arrange(region) %>% pull(region)
    # names(data) <- NULL
    flog.info(paste0("Dynamic region list ",  capture.output(str(data))))
    
    msg(capture.output(dput(data)))
    
    # создадим элемент
    selectInput("region_filter", 
                paste0("Регион (", length(data), ")"), 
                choices=data)
  })
  
}

shinyApp(ui = ui, server = server)
