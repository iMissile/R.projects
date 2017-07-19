# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8
rm(list=ls()) # очистим все переменные
gc()

library(tidyverse)
library(lubridate)
library(forcats)
library(readxl)
library(magrittr)
library(stringi)
library(futile.logger)
library(Cairo)
library(RColorBrewer)
library(extrafont)
library(hrbrthemes)
library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyBS)
library(shinyjs)
library(config)
library(anytime)
library(tictoc)
library(digest)
#library(rJava)
#library(ReporteRsjars)
library(ReporteRs)


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

# очистим все warnings():
assign("last.warning", NULL, envir = baseenv())

# ================================================================
ui <- 
  navbarPage("DVT IoT",
  # title=HTML('<div><a href="http://devoteam.com/"><img src="./img/devoteam_176px.png" width="80%"></a></div>'),
  title = "Статистика телесмотрения",
  tabPanel("Рейтинг каналов", value="channel_rating"),
  tabPanel("About", value="about"),
  # windowTitle="CC4L",
  # collapsible=TRUE,
  id="tsp",
  theme=shinytheme("flatly"),
  # shinythemes::themeSelector(),
  # includeCSS("styles.css"),

  # http://stackoverflow.com/questions/25387844/right-align-elements-in-shiny-mainpanel/25390164
  tags$head(tags$style(".rightAlign{float:right;}")), 
  
  
  
  # titlePanel("Статистика телесмотрения"),
  # ----------------
  conditionalPanel(
    condition = "input.tsp == 'channel_rating'",
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
      column(1, selectInput("history_depth", "История", 
                            choices = c("1 месяц"=30, "2 недели"=14,
                                        "1 неделя"=7, "3 дня"=3), selected=3)),
      column(1, selectInput("min_watch_time", "Мин. время",
                            choices = c("5 сек"=5, "10 сек"=10, 
                                        "20 сек"=20, "30 сек"=30), selected = 10)),
      column(1, selectInput("max_watch_time", "Макс. время",
                            choices = c("1 час"=1, "2 часа"=2, 
                                        "3 часа"=3, "4 часа"=4), selected = 2)),
      column(2, uiOutput("choose_segment")),
      column(2, uiOutput("choose_region"))
    ),
    #tags$style(type='text/css', "#in_date_range { position: absolute; top: 50%; transform: translateY(-80%); }"),
    tabsetPanel(
      id = "panel_id",
      selected="table_tab",
      tabPanel("Таблица", value = "table_tab",
               fluidRow(
                 p(),
                 column(12, div(DT::dataTableOutput('stat_table')), style="font-size: 90%")
                 )),
      tabPanel("График", value = "graph_tab",
               fluidRow(
                 p(),
                 column(12, div(plotOutput('stat_plot')), style="font-size: 90%")
               ))
      ),
    p(),
    fluidRow(
      column(8, {}),
      column(2, downloadButton("csv_download_btn", label="Download CSV", class = 'rightAlign')),
      column(2, downloadButton("word_download_btn", label="Download Word", class = 'rightAlign'))
    )
    #,
    #fluidRow(
    #  column(6, textOutput('info_text'))
    #)
    
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
  
  # реактивные переменные -------------------
  
  raw_df <- reactive({
    system.time(df <- readRDS("./data/tvstream4.rds"))
    flog.info(paste0("Loaded ", nrow(df), " rows"))
    flog.info(paste0("Time range [", min(df$timestamp), "; ", max(df$timestamp), "]"))
    
    as_tibble(df)
  })  

  cur_df <- reactive({
    # browser()
    # t <- input$min_watch_time
    flog.info(paste0("Applied time filter [", input$in_date_range[1], "; ", input$in_date_range[2], "]"))
    req(raw_df()) %>%
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
                # choices=data, multiple=TRUE)
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

  # обработчики кнопок выгрузки файлов --------------------------------------------------
  output$csv_download_btn <- downloadHandler(
    filename = function() {
      paste0("channel_rating_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # browser()
      cur_df() %>% arrange(desc(timestamp)) %>%
        # write_csv(file)
        # сделаем вывод в формате, принимаемым Excel
        write.table(file, na="NA", append=FALSE, col.names=TRUE, row.names=FALSE, sep=";")
    }
  )
  
  output$word_download_btn <- downloadHandler(
    filename = function() {
      name <- paste0("channel_rating_report-", Sys.Date(), ".docx", sep="")
      flog.info(paste0("Word report: '", name, "'"))
      name
    },
    content = function(file) {
      doc <- cur_df() %>% 
        arrange(desc(timestamp)) %>%
        gen_word_report(template_fname="./TV_report_template.docx")

      writeDoc(doc, file)
    }
  )  
  
}

shinyApp(ui = ui, server = server)
