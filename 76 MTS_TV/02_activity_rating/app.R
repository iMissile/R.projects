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
library(DBI)
library(RPostgreSQL)
library(config)
library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
library(config)
library(anytime)
library(tictoc)
library(digest)
library(officer)

options(shiny.usecairo=TRUE)
options(shiny.reactlog=TRUE)
options(spinner.type=4)

source("clickhouse.R")
eval(parse("funcs.R", encoding="UTF-8"))

# очистим все warnings():
assign("last.warning", NULL, envir = baseenv())

# ================================================================
ui <- 
  navbarPage("DVT IoT",
  # title=HTML('<div><a href="http://devoteam.com/"><img src="./img/devoteam_176px.png" width="80%"></a></div>'),
  title = "Статистика телесмотрения",
  tabPanel("Рейтинг пользовательской активности", value="general_panel"),
  tabPanel("About", value="about"),
  # windowTitle="CC4L",
  # collapsible=TRUE,
  id="tsp",
  # theme=shinytheme("flatly"),
  theme=shinytheme("yeti"),
  # shinythemes::themeSelector(),
  # includeCSS("styles.css"),

  # http://stackoverflow.com/questions/25387844/right-align-elements-in-shiny-mainpanel/25390164
  tags$head(tags$style(".rightAlign{float:right;}")), 
  
  
  
  # titlePanel("Статистика телесмотрения"),
  # ----------------
  conditionalPanel(
    # general panel -----------------------
    condition = "input.tsp == 'general_panel'",
    fluidRow(
      tags$style(type='text/css', '#cweather_text {white-space:pre;}')
      # tags$style(type='text/css', 'div {background-color: #000с00;}'), 
      
      #column(6, h2("Типовая форма"), h3(textOutput("cweather_text", inline=TRUE))),
      #column(6, h2("Заполнитель"))
      ),
    fluidRow(
      column(2, dateRangeInput("in_date_range",
                               label = "Диапазон дат",
                               # start = Sys.Date() - 3, end = Sys.Date(),
                               # на время отладки
                               start  = "2017-06-28",
                               end    = "2017-06-30",
                               # min = Sys.Date() - 10, 
                               max = Sys.Date(),
                               separator = " - ", format = "dd/mm/yy",
                               startview = "month", language = 'ru', weekstart=1)
      ), 
      column(1, selectInput("history_depth", "История", 
                            choices = c("1 месяц"=30, "2 недели"=14,
                                        "1 неделя"=7, "3 дня"=3, "1 день"=1), selected=1)),
      #column(1, selectInput("min_watch_time", "Мин. время",
      #                      choices = c("5 сек"=5, "10 сек"=10, 
      #                                  "20 сек"=20, "30 сек"=30), selected = 10)),
      #column(1, selectInput("max_watch_time", "Макс. время",
      #                      choices = c("1 час"=1, "2 часа"=2, 
      #                                  "3 часа"=3, "4 часа"=4), selected = 2)),
      column(6, uiOutput("choose_region")),
      column(2, selectInput("segment_filter", "Сегмент",
                            choices = c("Все"="all",
                                        "DVB-C"="DVB-C", 
                                        "IPTV"="IPTV", 
                                        "DVB-S"="DVB-S"), selected="all"))
    ),
    fluidRow(
      column(12, actionButton("process_btn", "Применить", class = 'rightAlign'))
    ),

    #tags$style(type='text/css', "#in_date_range { position: absolute; top: 50%; transform: translateY(-80%); }"),
    tabsetPanel(
      id = "panel_id",
      selected="table_tab",
      tabPanel("Таблица", value = "table_tab",
               fluidRow(
                 p(),
                 column(12, div(withSpinner(DT::dataTableOutput('stat_table'))), style="font-size: 90%")
               ),
               p(),
               fluidRow(
                 column(8, {}),
                 column(2, downloadButton("csv_download_btn", label="Экспорт (Excel)", class = 'rightAlign')),
                 column(2, downloadButton("word_download_btn", label="Экспорт (Word)", class = 'rightAlign'))
               )
      ),
      tabPanel("График", value = "graph_tab",
               fluidRow(
                 p(),
                 #column(11, {}),
                 column(12, div(selectInput("top_num", "Кол-во в ТОП:", 
                                        choices=c(3, 5, 7, 10, 20), 
                                         selected=5), 
                               class='rightAlign'))
               ),
               fluidRow(
                 p(),
                 column(6, div(plotOutput('top10_duration_plot', height="500px"))),
                 column(6, div(plotOutput('top10_stb_plot', height="500px")))
               )
               )
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

  # создание параметров оформления для различных видов графиков (screen\publish) ------
  font_sizes <- list(
    "screen"=list(base_size=20, axis_title_size=18, subtitle_size=15),
    "word_A4"=list(base_size=16, axis_title_size=14, subtitle_size=13)
  )
  
  # подгрузим таблицу преобразования транслита в русские названия городов
  cities_df <- {
    flog.info("Loading cities translit table")
    # подгрузим ограниченный список городов
    city_subset <- read_csv("region.csv")
    
    con <- dbConnect(clickhouse(), host="10.0.0.44", port=8123L, user="default", password="")
    
    df <- req(dbGetQuery(con, "SELECT * FROM regnames")  %>%
                mutate_if(is.character, `Encoding<-`, "UTF-8") %>%
                filter(translit %in% pull(city_subset)))
    flog.info(paste0("Cities translit table loaded ", nrow(df), " rows"))
    dbDisconnect(con)
    df
  }
  
  
  # реактивные переменные -------------------
  raw_df <- reactive({
    input$process_btn # обновлять будем вручную
    isolate({
      con <- dbConnect(clickhouse(), host="10.0.0.44", port=8123L, user="default", password="")
      # regions <- c("Moskva", "Barnaul")
      regions <- input$region_filter
      # browser()
    
      # r <- buildReq(begin=today(), end=today()+days(1), regions)
      flog.info(paste0("Applied time filter [", input$in_date_range[1], "; ", input$in_date_range[2], "]"))
      flog.info(paste0("Applied region filter [", regions, "]"))
      r <- buildReq(begin=input$in_date_range[1], end=input$in_date_range[2], 
                    regions=regions, segment=input$segment_filter)
    })
    
    # browser()

    tic()
    temp_df <- dbGetQuery(con, r) %>%
      as_tibble()

    flog.info(paste0("Query: ", capture.output(toc())))
    # system.time(df <- readRDS("./data/tvstream4.rds"))
    flog.info(paste0("Loaded ", nrow(temp_df), " rows"))
    
    # browser()
    df <- temp_df %>%
      # время смотрения, мин
      mutate(total_duration=round(as.numeric(total_duration), 0)) %>%
      # 3. % уникальных приставок
      mutate(stb_ratio=round(unique_stb/total_unique_stb, 3)) %>%
      left_join(cities_df, by=c("region"="translit")) %>%
      select(-region) %>%
      select(region=russian, everything())
    
    # browser()
    dbDisconnect(con)
    as_tibble(df)
  })  

  cur_df <- reactive({
    req(raw_df()) %>%
      # filter(segment==input$segment_filter) %>%
      select(region, everything())
  })
  
  msg <- reactiveVal("")

  # таблица с выборкой по каналам ----------------------------
  output$stat_table <- DT::renderDataTable({
    # https://rstudio.github.io/DT/functions.html
    # https://stackoverflow.com/questions/39970097/tooltip-or-popover-in-shiny-datatables-for-row-names
    colnames_with_tooltip <- tribble(
      ~colname, ~collabel,
      "регион", "подсказка по региону",
      "кол-во уник. STB", "подсказка по колонке 2",
      "всего уник. STB", "подсказка по колонке 3",
      "суммарное время, мин",	"подсказка по колонке 4",
      "кол-во просмотров","подсказка по колонке 5",
      "% уник. STB", "подсказка по колонке 6")

    # browser()
    DT::datatable({req(cur_df()); colNamesToRus(cur_df())},
                  # colnames=c('Канал'='channelId', 'Сегмент'='segment', 'Регион'='region', 'Дата'='date'),
                  rownames=FALSE,
                  filter = 'bottom',
                  # только после жесткой фиксации колонок
                  container = htmltools::withTags(table(class = 'display',
                    thead(
                      tr(apply(colnames_with_tooltip, 1,
                               function(x) th(title=x[2], x[1])))
                    )
                  )),
                  options=list(dom='fltip', pageLength=7, lengthMenu=c(5, 7, 10, 15),
                               order=list(list(3, 'desc')))) %>%
      DT::formatPercentage("% уник. STB", 2)
    })
  
  # график Топ10 каналов по суммарному времени просмотра -------------
  output$top10_duration_plot <-renderPlot({
    plotTop10Duration(cur_df(), publish_set=font_sizes[["screen"]], 
                      ntop=as.integer(input$top_num))
  })
  
  # график Топ10 каналов по количеству уникальных приставок --------------
  output$top10_stb_plot <-renderPlot({
    plotTop10STB(cur_df(), publish_set=font_sizes[["screen"]], 
                 ntop=as.integer(input$top_num))
  })  

  # динамическое управление диапазоном дат ---------
  observeEvent(input$history_depth, {
    
    # почему-то $history_depth получаем как строку
    date <- Sys.Date()-as.numeric(input$history_depth)
    flog.info(paste0("Start date changed to  ", date))
    # updateDateRangeInput(session, "in_date_range", start=date)
   }
  )
  
  # служебный вывод ---------------------  
  output$info_text <- renderText({
    msg()
  })


  # динамический выбор региона ---------
  output$choose_region <- renderUI({
    
    data <- as.list(cities_df$translit)
    names(data) <- cities_df$russian
    
    # browser()
    
    # создадим элемент
    selectInput("region_filter", 
                paste0("Регион (", length(data), ")"),
                multiple=TRUE,
                choices=data, width = "100%")
  })

  # обработчики кнопок выгрузки файлов --------------------------------------------------
  # выгрузка таблицы в CSV -----------------------  
  output$csv_download_btn <- downloadHandler(
    filename = function() {
      paste0("user_activity_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      cur_df() %>%
        # сделаем вывод в формате, принимаемым Excel
        write.table(file, na="NA", append=FALSE, col.names=TRUE, row.names=FALSE, sep=";")
    }
  )
  
  # выгрузка таблицы в Word -----------------------
  output$word_download_btn <- downloadHandler(
    filename = function() {
      name <- paste0("user_activity_report-", Sys.Date(), ".docx", sep="")
      flog.info(paste0("Word report: '", name, "'"))
      name
    },
    content = function(file) {
      doc <- cur_df() %>% select(-total_unique_stb) %>%
        gen_word_report(publish_set=font_sizes[["word_A4"]])
      print(doc, target=file)  
    }
  )  
  
}

shinyApp(ui = ui, server = server)
