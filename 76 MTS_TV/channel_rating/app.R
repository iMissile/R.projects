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
library(futile.logger)
library(anytime)
library(tictoc)
library(digest)

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
      column(6, h2("Заполнитель"))
      ),
    fluidRow(
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
                                        "1 неделя"=7, "3 дня"=3), selected=3)),
      column(2, uiOutput("choose_segment")),
      column(2, uiOutput("choose_region")),
      column(2, selectInput("min_watch_time", "Мин. время просмотра, сек",
                            choices = c(5, 10, 20, 30), selected = 10)),
      column(2, selectInput("max_watch_time", "Макс. время просмотра, час",
                            choices = c(1, 2, 4, 8), selected = 2))
    ),
    #tags$style(type='text/css', "#in_date_range { position: absolute; top: 50%; transform: translateY(-80%); }"),
    fluidRow(
      column(12, div(DT::dataTableOutput('stat_table')), style="font-size: 90%")
    ),
    fluidRow(
      column(6, textOutput('info_text'))
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
    req(raw_df) %>%
      mutate(date=anydate(timestamp)) %>%
      filter(input$in_date_range[1] < date  & date < input$in_date_range[2])
  })
  
  msg <- reactiveVal("")

  
  output$stat_table <- DT::renderDataTable({
    # https://rstudio.github.io/DT/functions.html
    DT::datatable(req(cur_df()),
                  rownames=FALSE,
                  filter = 'bottom',
                  options=list(pageLength=7, lengthMenu=c(5, 7, 10, 15),
                               order=list(list(3, 'desc')))) %>%
      DT::formatDate("timestamp", method="toLocaleString")
    }
    )
    
  
  
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
                choices=data, multiple=TRUE)
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
