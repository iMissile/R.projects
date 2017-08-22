# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8
rm(list=ls()) # очистим все переменные
gc()

library(tidyverse)
library(lubridate)
library(scales)
library(forcats)
library(readxl)
library(magrittr)
library(stringi)
library(stringr)
library(futile.logger)
library(jsonlite)
library(Cairo)
library(RColorBrewer)
library(extrafont)
library(hrbrthemes)
library(DBI)
library(RPostgreSQL)
library(config)
library(shiny)
library(shinyjqui)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
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

# определяем окружение в котором запускаемся
if (Sys.info()["sysname"] == "Linux") {
  # CTI стенд
  Sys.setenv("R_CONFIG_ACTIVE"="cti-prod")
  # Sys.setenv("R_CONFIG_ACTIVE"="cti-demo")
}else{
  # MT стенд
  Sys.setenv("R_CONFIG_ACTIVE"="media-tel-prod")
  Sys.setenv("R_CONFIG_ACTIVE"="media-tel-demo")
}   

# ================================================================
ui <- 
  navbarPage(
  # title=HTML('<div><a href="http://devoteam.com/"><img src="./img/devoteam_176px.png" width="80%"></a></div>'),
  title = "Статистика телесмотрения",
  tabPanel("Рейтинг каналов", value="general_panel"),
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
                               label="Диапазон дат",
                               start=Sys.Date()-1, end=Sys.Date(),
                               # на время отладки
                               # start="2017-06-28", end="2017-06-30",
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
      column(6, {}),
      column(2, selectInput("select_config", "Конфигурация",
                            choices = c("cti-prod","cti-demo","demia-tel-prod", "media-tel-demo"), 
                            selected=Sys.getenv("R_CONFIG_ACTIVE"))),
      column(2, actionButton("set_test_dates_btn", "Вкл. демо дату", class = 'rightAlign')),
      column(2, actionButton("process_btn", "Применить", class = 'rightAlign'))
      ),
    # https://stackoverflow.com/questions/28960189/bottom-align-a-button-in-r-shiny
    tags$style(type='text/css', "#set_test_dates_btn {margin-top: 25px;}"),
    tags$style(type='text/css', "#process_btn {margin-top: 25px;}"),

    #tags$style(type='text/css', "#in_date_range { position: absolute; top: 50%; transform: translateY(-80%); }"),
    tabsetPanel(
      id = "main_panel",
      selected="table_tab",
      tabPanel("Таблица", value="table_tab",
               fluidRow(
                 p(),
                 column(12, div(withSpinner(DT::dataTableOutput("stat_table"))), style="font-size: 90%")
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
                 jqui_sortabled(
                   div(id='top10_plots',
                 column(6, div(withSpinner(plotOutput('top10_duration_plot', height="500px")))),
                 column(6, div(withSpinner(plotOutput('top10_stb_plot', height="500px"))))
                       )))
               )
      )
    #,
    #fluidRow(
    #  column(6, textOutput('info_text'))
    #)
    
  ),
  shinyjs::useShinyjs()  # Include shinyjs
)


# ================================================================
server <- function(input, output, session) {

  setBookmarkExclude(c("stat_table")) # таблицу восстановить мы не можем и не должны
  
  # статические переменные ------------------------------------------------
  log_name <- "app.log"
  
  flog.appender(appender.tee(log_name))
  flog.threshold(TRACE)
  flog.info(paste0("App started in '", Sys.getenv("R_CONFIG_ACTIVE"), "' environment"))

  # создание параметров оформления для различных видов графиков (screen\publish) ------
  font_sizes <- list(
    "screen"=list(base_size=20, axis_title_size=18, subtitle_size=15),
    "word_A4"=list(base_size=14, axis_title_size=12, subtitle_size=11)
  )
  
  # Sys.getenv("R_CONFIG_ACTIVE")
  ch_db <- config::get("clickhouse") # достаем параметры подключения
  # создаем коннект к инстансу CH -----------
  conn <- dbConnect(clickhouse(), host=ch_db$host, port=ch_db$port, user=ch_db$user, password=ch_db$password)

  # подгрузим таблицу преобразования транслита в русские названия городов -------
  cities_df <- {
    flog.info("Loading cities translit table")
    # подгрузим ограниченный список городов
    city_subset <- read_csv("region.csv")
    
    df <- req(dbGetQuery(conn, "SELECT * FROM regnames")  %>%
                mutate_if(is.character, `Encoding<-`, "UTF-8") %>%
                filter(translit %in% pull(city_subset)))
    flog.info(paste0("Cities translit table loaded ", nrow(df), " rows"))
    # dbDisconnect(con)
    df
  }
  
  # словарь для преобразований имен полей из английских в русские
  # имена колонок -- группы и агрегаты из запроса
  # сливаем модельные данные
  dict_df <- {
    df0 <- jsonlite::fromJSON("data_dict.json", simplifyDataFrame=TRUE)
    
    # на всякий случай защитимся от случая, когда вообще не определено поле internal_name
    if (!"internal_name" %in% names(df0)) df0$internal_name <- NA
    dict_df <- df0 %>%
      as_tibble() %>%
      # если есть поле в БД, а внутреннее представление не задано, то прозрачно транслируем
      mutate(internal_name={map2_chr(.$db_field, .$internal_name, ~if_else(!is.na(.x) & is.na(.y), .x, .y))})
  }  
  
  # подгрузим таблицу преобразования идентификатора канала в русское название ----
  progs_df <- jsonlite::fromJSON("./channels.json", simplifyDataFrame=TRUE) %>% 
    select(channelId, channelName=name)
  
  # реактивные переменные -------------------
  raw_df <- reactive({
    input$process_btn # обновлять будем вручную
    isolate({
      # r <- buildReq(begin=today(), end=today()+days(1), regions)
      # flog.info(paste0("Applied time filter [", input$in_date_range[1], "; ", input$in_date_range[2], "]"))
      # flog.info(paste0("Applied region filter [", regions, "]"))
      r <- buildReq(ch_db$table, begin=input$in_date_range[1], end=input$in_date_range[2], 
                    region=input$region_filter, segment=input$segment_filter)
      flog.info(paste0("DB request: ", r))
    })
    
    # browser()
    tic()
    temp_df <- dbGetQuery(conn, r) %>%
      as_tibble()

    flog.info(paste0("Query: ", capture.output(toc())))
    flog.info(paste0("Table: ", capture.output(head(temp_df, 2))))
    # system.time(df <- readRDS("./data/tvstream4.rds"))
    flog.info(paste0("Loaded ", nrow(temp_df), " rows"))
    
    # !!! исправляем непонятно чей косяк: если вложенный select дает 0 строк, то его имя транслируется как NULL
    # browser()
    names(temp_df)[[3]] <- "total_unique_stb"
    
    # browser()
    df <- temp_df %>%
      # время смотрения, мин
      mutate(channel_duration=round(as.numeric(channel_duration), 0)) %>%
      # 6. Среднее время просмотра, мин
      mutate(mean_duration=round(channel_duration/watch_events, 0)) %>%
      # 3. % уникальных приставок
      mutate(stb_ratio=round(unique_stb/total_unique_stb, 3)) %>%
      # 5. % времени просмотра
      mutate(watch_ratio=round(channel_duration/sum(channel_duration), 5)) %>%
      # 7. Среднее суммарное время просмотра одной приставкой за период, мин
      mutate(duration_per_stb=round(channel_duration/unique_stb, 0))
      # %>% mutate_at(vars(mean_duration, ratio_per_stb, watch_ratio, duration_per_stb), funs(round), digits=1)

    # browser()
    # dbDisconnect(con)
    as_tibble(df)
  })  

  cur_df <- reactive({
    req(raw_df()) %>%
      mutate_at(vars(channelId), as.character) %>%
      left_join(progs_df, by=c("channelId")) %>%
      # санация
      mutate(channelName=if_else(is.na(channelName), 
                                 str_c("_", channelId, "_"), 
                                 channelName)) %>%
      select(channelName, channelId, everything())
  })
  
  msg <- reactiveVal("")

  # таблица с выборкой по каналам ----------------------------
  output$stat_table <- DT::renderDataTable({
    df <- req(cur_df()) %>%
      select(-channelId)
    
    # сделаем мэпинг русских имен колонок и подсказок
    colnames_df <- tibble(internal_name=names(df)) %>%
      left_join(dict_df, by=c("internal_name"))
      # поля строго заданы, санация сознательно не делается. Если что не так, надо справочник править

    # https://stackoverflow.com/questions/39970097/tooltip-or-popover-in-shiny-datatables-for-row-names
    colheader <- htmltools::withTags(
      table(class = 'display',
            thead(
              tr(colnames_df %>%
                   {purrr::map2(.$col_label, .$human_name_rus, ~th(title=.x, .y))})
              )))
    
    # browser()
    # https://rstudio.github.io/DT/functions.html
    DT::datatable(df,
                  rownames=FALSE,
                  filter='bottom',
                  # только после жесткой фиксации колонок
                  container=colheader,
                  options=list(dom='fltip', pageLength=7, lengthMenu=c(5, 7, 10, 15),
                               order=list(list(3, 'desc')))) %>%
      DT::formatPercentage("watch_ratio", 2) %>%
      DT::formatPercentage("stb_ratio", 2)
    })
  
  # график Топ10 каналов по суммарному времени просмотра -------------
  output$top10_duration_plot <-renderPlot({
    shiny::validate(
      need(!is.null(cur_df()), "NULL value can't be renederd"),
      need(nrow(cur_df())>0, "0 rows -- nothing to draw") 
    )
    plotTop10Duration(cur_df(), publish_set=font_sizes[["screen"]])
  })
  
  # график Топ10 каналов по количеству уникальных приставок --------------
  output$top10_stb_plot <-renderPlot({
    shiny::validate(
      need(!is.null(cur_df()), "NULL value can't be renederd"),
      need(nrow(cur_df())>0, "0 rows -- nothing to draw") 
    )
    plotTop10STB(cur_df(), publish_set=font_sizes[["screen"]])
  })  

  # динамическое управление диапазоном дат ---------
  observeEvent(input$history_depth, {
    # $history_depth получаем как строку
    date <- Sys.Date()-as.numeric(input$history_depth)
    flog.info(paste0("Start date changed to  ", date))
    # updateDateRangeInput(session, "in_date_range", start=date)
    }
  )

  # фиксим даты на демо диапазон ---------  
  observeEvent(input$set_test_dates_btn, {
    updateDateRangeInput(session, "in_date_range", start="2017-05-28", end="2017-05-30")
    }
  )
  
  # управляем визуализацией кнопок выгрузки ----- 
  observe({
    # browser()
    if(!is.null(cur_df()) & nrow(cur_df())>0) {
      shinyjs::enable("csv_download_btn")
      shinyjs::enable("word_download_btn")
    } else {
      shinyjs::disable("csv_download_btn")
      shinyjs::disable("word_download_btn")
    }
  })  
  
  # служебный вывод ---------------------  
  output$info_text <- renderText({
    msg()
  })

  # динамический выбор региона ---------
  output$choose_region <- renderUI({
    
    data <- as.list(cities_df$translit)
    names(data) <- cities_df$russian
    # browser()
    selectInput("region_filter", 
                paste0("Регион (", length(data), ")"),
                multiple=TRUE,
                choices=data, width = "100%")
  })

  # обработчики кнопок выгрузки файлов --------------------------------------------------
  # выгрузка таблицы в CSV -----------------------  
  output$csv_download_btn <- downloadHandler(
    filename = function() {
      paste0("channel_rating_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      cur_df() %>%
        # сделаем вывод в формате, принимаемым Excel
        write.table(file, na="NA", append=FALSE, col.names=TRUE, 
                    row.names=FALSE, sep=";", fileEncoding="windows-1251")
    }
  )
  
  # выгрузка таблицы в Word -----------------------
  output$word_download_btn <- downloadHandler(
    filename = function() {
      name <- paste0("channel_rating_report-", Sys.Date(), ".docx", sep="")
      flog.info(paste0("Word report: '", name, "'"))
      name
    },
    content = function(file) {
      # browser();
      doc <- cur_df() %>% # select(-total_unique_stb) %>% # пока убираем, чтобы была консистентная подстановка
        gen_word_report(publish_set=font_sizes[["word_A4"]], dict=dict_df)
      print(doc, target=file)  
    }
  )  
  onBookmark(function(state) {
    #state$values$event_filter <- input$event_filter
    #state$values$intensity <- input$intensity
  })
  
  onRestored(function(state){
    # input-ы сохраняются штатным образом, главное их восстановить. 
    # восстанавливаем не из values, а из input
    # browser()
    updateSelectInput(session, "event_filter", selected=state$input$event_filter)
    updateSelectInput(session, "prefix_filter", selected=state$input$prefix_filter)
    updateSelectInput(session, "serial_mask", selected=state$input$serial_mask)
    # shinyjs::delay(800, {})
    updateSliderInput(session, "duration_range", value=state$input$duration_range)
    updateDateRangeInput(session, "in_date_range",
                         start=state$input$in_date_range[1], 
                         end=state$input$in_date_range[2])
  }) 
  
  # динамичекое обновление url в location bar
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  
  onBookmarked(function(url) {
    updateQueryString(url)
  })   
}

shinyApp(ui = ui, server = server)
