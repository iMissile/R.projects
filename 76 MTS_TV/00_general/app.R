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
  tabPanel("Форма запросов", value="general_panel"),
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
  
  conditionalPanel(
    # general panel -----------------------
    condition = "input.tsp == 'general_panel'",
    fluidRow(
      tags$style(type='text/css', '#cweather_text {white-space:pre;}')
      # tags$style(type='text/css', 'div {background-color: #000с00;}'), 
      
      #column(6, h2("Типовая форма"), h3(textOutput("cweather_text", inline=TRUE))),
      #column(6, h2("Заполнитель"))
      ),
    h3("Фильтры"),
    fluidRow(
      column(2, dateRangeInput("in_date_range",
                               label="Диапазон дат",
                               # start=Sys.Date()-1, end=Sys.Date(),
                               # при демонстрации на пилотных данных
                               start="2017-03-01", end="2017-03-02",
                               # min = Sys.Date() - 10, 
                               # max = Sys.Date(),
                               separator = " - ", format = "dd/mm/yy",
                               startview = "month", language = 'ru', weekstart=1)
      ), 
      #column(1, selectInput("history_depth", "История", 
      #                      choices = c("1 месяц"=30, "2 недели"=14,
      #                                  "1 неделя"=7, "3 дня"=3, "1 день"=1), selected=1)),
      column(2, textInput("serial_mask", "Фрагмент S/N", value = "", placeholder=NULL)),
      column(8, sliderInput("duration_range", "Длительность, мин", 
                            min=0, max=5*60, value=c(5, 2*60), width="100%"))
      # column(1, selectInput("min_watch_time", "Мин. время",
      #                       choices = c("30 сек"=30, "1 мин"=1*60, "2 мин"=2*60, 
      #                                   "3 мин"=3*60, "5 мин"=5*60), selected = 1*60)),
      # column(1, selectInput("max_watch_time", "Макс. время",
      #                       choices = c("1 час"=1*60*60, "2 часа"=2*60*60, 
      #                                   "3 часа"=3*60*60, "4 часа"=4*60*60), selected=2*60*60))
    ),
    fluidRow(
      column(6, uiOutput("choose_region")),
      column(6, uiOutput("choose_channel"))
    ),
    p(),
    fluidRow(
      column(6, selectInput("prefix_filter", "Префикс",
                            choices=c("001 - DVB-S: Dune Lite"="*001", 
                                      "002 - DVB-S: Dune Lite+"="*002",
                                      "003 - IPTV: Huawei"="*003", 
                                      "004 - DVB-S: Huawei"="*004",
                                      "006 - DVB-C: Huawei"="*006", 
                                      "007 - IPTV: ZTE"="*007",
                                      "008 - IPTV: EKT"="*008", 
                                      "009 - DVB-C: EKT"="*009"), 
                            multiple=TRUE, width="100%")),
      column(6, selectInput("event_filter", "Тип переключения канала",
                            choices=c("CHPLUS", "INIT", "DIGIT", "PREVIOUS_CHANNEL",
                                      "CHMINUS", "FULLSCREEN_EPG", "ENTER", "PVR", 
                                      "REMINDER", "MAIN_MENU", "FILE", "VOD", "CATCH_UP"), 
                            multiple=TRUE, width="100%"))
    ),
    h3("Агрегаты"),
    # блок элементов группировки и агрегации, формируемых динамически
    fluidRow(
      column(2, uiOutput("choose_group1")),
      column(2, uiOutput("choose_group2")),
      column(2, uiOutput("choose_group3")),
      # https://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices
      column(6, uiOutput("choose_vars"))
    ),
    fluidRow(
      column(10, actionButton("process_btn", "Применить", class = 'rightAlign')),
      column(1, div(bookmarkButton(), class = 'rightAlign')),
      column(1, downloadButton("csv_download_btn", label="Экспорт (Excel)", class = 'rightAlign'))
      
    ),
    h3("Выборка"),
    fluidRow(
      column(12, verbatimTextOutput('info_text'))
    ),
    #tags$style(type='text/css', "#in_date_range { position: absolute; top: 50%; transform: translateY(-80%); }"),
    tabsetPanel(
      id = "panel_id",
      selected="table_tab",
      tabPanel("Таблица", value = "table_tab",
               fluidRow(
                 p(),
                 # column(12, div(withSpinner(DT::dataTableOutput('stat_table'))), style="font-size: 90%")
                 column(12, div(DT::dataTableOutput('stat_table')), style="font-size: 90%")
               )
               )
      )
    
  ),
  shinyjs::useShinyjs()  # Include shinyjs
)


# ================================================================
server <- function(input, output, session) {
  # статические переменные ------------------------------------------------
  log_name <- "app.log"
  
  flog.appender(appender.tee(log_name))
  flog.threshold(TRACE)
  flog.info("App started")

  shinyjs::hide("csv_download_btn")
  
  # создание параметров оформления для различных видов графиков (screen\publish) ------
  font_sizes <- list(
    "screen"=list(base_size=20, axis_title_size=18, subtitle_size=15),
    "word_A4"=list(base_size=14, axis_title_size=12, subtitle_size=11)
  )
  
  # создаем коннект к инстансу CH -----------
  if (Sys.info()["sysname"] == "Linux") {
    # CTI стенд
    con <- dbConnect(clickhouse(), host="172.16.33.74", port=8123L, user="default", password="")
  }else{
    # MT стенд
    con <- dbConnect(clickhouse(), host="10.0.0.44", port=8123L, user="default", password="")
  }      
  
  # подгрузим таблицу преобразования транслита в русские названия городов -------
  cities_df <- {
    flog.info("Loading cities translit table")
    # подгрузим ограниченный список городов
    city_subset <- read_csv("region.csv")
    
    df <- req(dbGetQuery(con, "SELECT * FROM regnames")  %>%
                mutate_if(is.character, `Encoding<-`, "UTF-8") %>%
                filter(translit %in% pull(city_subset)))
    flog.info(paste0("Cities translit table loaded ", nrow(df), " rows"))
    # dbDisconnect(con)
    df
  }

  # подгрузим таблицу преобразования идентификатора канала в русское название ----
  progs_df <- jsonlite::fromJSON("./channels.json", simplifyDataFrame=TRUE) %>% 
    select(channelId, channelName=name) %>%
    as_tibble()

  # создаем tidy модель данных
  data_model_df <- {
    df0 <- jsonlite::fromJSON("datamodel.json", simplifyDataFrame=TRUE) %>%
      as_tibble() %>%
      # создаем внутреннее представление (раньше представление БД могло быть синтетикой)
      mutate(internal_field=ifelse(any(names(.) %in% 'internal_field'), 
                                   internal_field, as.character(NA))) %>%
      mutate(select_string={map2_chr(.$internal_field, .$ch_field,
                                     ~if_else(is.na(.x), .y, stri_join(.y, " AS ", .x)))}) %>%
      mutate(internal_field={map2_chr(.$internal_field, .$ch_field,
                                      ~if_else(is.na(.x), .y, .x))})  
  }
  # модель для переменных под агрегат
  var_model_df <- {    
    # построим модель переменных для вычисления
    df <- data_model_df %>%
      # в переменные берем только то, что подпадает под агрегаты
      filter(!is.na(aggr_ops)) %>%
      separate_rows(aggr_ops) %>%
      mutate(id=row_number()) %>%
      # переводим алиасы функций агрегации в различные узлы (экран, CH)
      mutate(x=aggr_ops,
             ext_aggr_opts=case_when(
               x=="min" ~ "мин, min",
               x=="max" ~ "макс, max",
               x=="mean" ~ "ср, avg",
               x=="sum" ~ "сумма, sum",
               x=="unique" ~ "уник, uniq",           
               x=="count" ~ ", count",
               TRUE ~ "UNKNOWN"
             )) %>%
      # разнесем на отдельные колонки
      separate(ext_aggr_opts, into=c("visual_aggr_func", "ch_aggr_func")) %>%
      select(-x) %>%
      # mutate_at(vars(visual_aggr_func), funs(na_if(., ""))) %>%
      mutate(visual_var_name={map2_chr(.$human_name_rus, .$visual_aggr_func,
                                     ~if_else(.y=="", .x, stri_join(.x, ": ", .y)))}) %>%
      mutate(ch_query_name=str_c(ch_aggr_func, "(", internal_field, ")"))
    
    # browser()
    df
  }
  # модель переменных для группировки
  group_model_df <- {    
    # построим модель переменных для вычисления
    df <- data_model_df %>%
      # в переменные берем только то, что подпадает под агрегаты
      filter(can_be_grouped) %>%
      # mutate(visual_group_name=str_c("_", internal_field, "_")) %>%
      mutate(visual_group_name=human_name_rus) %>%
      select(select_string, internal_field, visual_group_name) %>%
      mutate(id=row_number()) %>%
      # добавим пустую строку, позволяющую не выбирать агрегат
      add_row(id=0, visual_group_name="нет") %>%
      arrange(id) 

    df
  }
  # словарь для преобразований имен полей из английских в русские
  # имена колонок -- группы и агрегаты из запроса
  # сливаем модельные данные
  dic_df <- dplyr::union(var_model_df %>% select(name_enu=ch_query_name, name_rus=visual_var_name),
                         group_model_df %>% select(name_enu=internal_field, name_rus=visual_group_name)
  )
  

  # browser()
  # реактивные переменные -------------------
  msg <- reactiveVal("")
  sql_request <- reactiveVal("")
    
  raw_df <- reactive({
    r <- req(sql_request())
    flog.info(paste0("DB request: ", r))

    tic()
    df <- dbGetQuery(con, r) %>%
      as_tibble()
    flog.info(paste0("Query: ", capture.output(toc())))
    flog.info(paste0("Table: ", capture.output(head(df, 2))))
    flog.info(paste0("Loaded ", nrow(df), " rows"))

    df
  })  

  cur_df <- reactive({
    req(raw_df())
  })
  
  # таблица с выборкой по каналам ----------------------------
  output$stat_table <- DT::renderDataTable({
    df <- req(cur_df())

    # делаем русские имена колонок в выводе
    colnames_df <- tibble(name_enu=names(df)) %>%
      left_join(dic_df, by=c("name_enu")) %>%
      # санация
      mutate(name_rus={map2_chr(.$name_rus, .$name_enu, 
                                ~if_else(is.na(.x), .y, .x))})

    # https://stackoverflow.com/questions/39970097/tooltip-or-popover-in-shiny-datatables-for-row-names
    colheader <- htmltools::withTags(
     table(class = 'display',
           thead(
             tr(colnames_df %>%
                  {purrr::map2(.$name_enu, .$name_rus, ~th(title=.x, .y))})
             )))
    # 
    # browser()
    # https://rstudio.github.io/DT/functions.html
    DT::datatable(df,
                  rownames=FALSE,
                  filter='bottom',
                  container=colheader,
                  options=list(dom='fltip', pageLength=7, lengthMenu=c(5, 7, 10, 15))
                  )
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
  observeEvent(cur_df(), {
    # msg(capture.output(str(session)))
    # browser()
    if(!is.null(raw_df()) & nrow(raw_df())>0) {
      shinyjs::show("csv_download_btn")
    } else {
      shinyjs::hide("csv_download_btn")
    }
  })  
  
  # служебный вывод ---------------------  
  output$info_text <- renderText({
    msg()
  })

  # динамический выбор списка доступных агрегатов переменных в запрос ---------
  output$choose_vars <- renderUI({
    # сюда не должны попадать переменные, которые выбраны в группировках
    # исключаем поля, которые указаны в 1-ой, 2-ой и 3-ей группировках
    req(input$group_var1, input$group_var2, input$group_var3)
    # собираем переменные групировки (в SELECT & GROUP BY)
    idx <- map_int(1:3, ~as.integer(input[[stri_join("group_var", .x)]]))
    exclude_var <- left_join(as_tibble(idx), group_model_df, by=c("value"="id")) %>% drop_na() %>% pull(internal_field)
    # browser()
    df <- var_model_df %>%
      filter(!(internal_field %in% exclude_var))
    data <- setNames(as.list(df$id), df$visual_var_name)

    selectizeInput("selected_vars", "Агрегатные функции", choices=data, 
                   selected=NULL, multiple=TRUE, width="100%")
    })
  
  # динамический выбор группировки 1-го уровня в запрос ---------
  output$choose_group1 <- renderUI({
    flog.info("Recreating 'Group 1' control")
    df <- group_model_df
    data <- setNames(as.list(df$id), df$visual_group_name)
    selectInput("group_var1", "Группировка 1", choices=data)
  })

  # динамический выбор группировки 2-го уровня в запрос ---------
  output$choose_group2 <- renderUI({
    req(input$group_var1)
    flog.info("Recreating 'Group 2' control")
    # если выше нет группировки, то и ниже ее быть не может
    if(input$group_var1=="0"){
      data <- c("нет"="0")
    } else {
      # исключаем поле, которое указано в 1-ой группировке
      df <- group_model_df %>%
        filter(id != input$group_var1)
      data <- setNames(as.list(df$id), df$visual_group_name)
    }
    selectInput("group_var2", "Группировка 2", choices=data)
  })

  # динамический выбор группировки 3-го уровня в запрос ---------
  output$choose_group3 <- renderUI({
    # исключаем поле, которое указано в 1-ой или 2-ой группировке
    req(input$group_var1, input$group_var2)
    flog.info("Recreating 'Group 3' control")
    # если выше нет группировки, то и ниже ее быть не может
    if(input$group_var2=="0"){
      data <- c("нет"="0")
    } else {
      # исключаем поле, которое указано в 1-ой или 2-ой группировке
      df <- group_model_df %>%
        filter(id != input$group_var1 & id != input$group_var2)
      data <- setNames(as.list(df$id), df$visual_group_name)
    }    
    selectInput("group_var3", "Группировка 3", choices=data, selected=NULL)
  })

  # динамический выбор региона ---------
  output$choose_region <- renderUI({
    data <- as.list(cities_df$translit)
    names(data) <- cities_df$russian
    selectInput("region_filter", 
                paste0("Регион (", length(data), ")"),
                multiple=TRUE,
                choices=data, width = "100%")
  })

  # динамический выбор канала ---------
  output$choose_channel <- renderUI({
    data <- as.list(progs_df$channelId)
    names(data) <- progs_df$channelId

    # создадим элемент
    selectInput("channel_filter", 
                paste0("Канал [ID] (", length(data), ")"),
                multiple=TRUE,
                choices=data, width = "100%")
  })  
  
  # генерируем SQL запрос ----------------
  observeEvent(input$process_btn, {
    # генерируем SQL запрос

    # определяем, есть ли переменные в select
    # has_select <- !all(purrr::map_lgl(list(input$group_var1, input$group_var2,
    #                                        input$group_var3, input$selected_vars), 
    #                                   is.null))
    
    # собираем переменные групировки (в SELECT & GROUP BY)
    idx <- map_int(1:3, ~as.integer(input[[stri_join("group_var", .x)]]))
    df <- left_join(as_tibble(idx), group_model_df, by=c("value"="id")) %>% drop_na()
    group_vars <- stri_join(df$internal_field, collapse=", ")
    
    # если переменных нет, то имеем character(0)
    
    # browser()
    has_select_var <- any(length(group_vars) > 0, !is.null(input$selected_vars))

    limit_string <- ""    
    if(has_select_var){
      # построим список переменных в часть SELECT
      vars_string <- var_model_df %>%
        filter(id %in% input$selected_vars) %>%
        # {purrr::map2_chr(.$ch_query_name, .$col_name, ~str_c(.x, " AS ", .y))} %>%
        {purrr::map_chr(.$ch_query_name, ~str_c(.x, " "))} %>%
        stri_join(sep="", collapse=", ")
      # объединим с групповыми переменными
      # browser()
      select_string <- stri_join(group_vars, vars_string, sep=", ", collapse="", ignore_null=TRUE)
    } else {
      # не выбрана ни одна переменная
      select_string <- " * "
      limit_string <- "LIMIT 2000"
    }
    
    # browser()
    where_string <- paste0(paste0(" date >= '", input$in_date_range[1], "' AND date <= '", input$in_date_range[2], "' "),
                           paste0("AND duration >= ", input$duration_range[1]*60, " AND duration <= ", input$duration_range[2]*60, " "),
                           buildReqFilter("region", input$region_filter, add=TRUE),
                           buildReqFilter("prefix", input$prefix_filter, add=TRUE),
                           buildReqFilter("channelId", input$channel_filter, add=TRUE),
                           buildReqFilter("switchEvent", input$event_filter, add=TRUE),
                           ifelse(input$serial_mask=="", "", paste0(" AND like(serial, '%", input$serial_mask, "%') "))
    ) 
    
    from_where_string <- paste0(" FROM view_simstates ",
                                "WHERE ", where_string)
    
    text <- paste0("SELECT ", select_string, 
                   from_where_string,
                   ifelse(length(group_vars)>0, paste0(" GROUP BY ", group_vars), ""),
                   limit_string, ";")
    
    sql_request(text)
    # msg(from_where_string)
    msg(text)
  })
  
  # выгрузка таблицы в CSV -----------------------  
  output$csv_download_btn <- downloadHandler(
    filename = function() {
      paste0("slice_", format(Sys.time(), "%F_%H-%M-%S"), ".csv", sep="")
    },
    content = function(file) {
      df <- cur_df()
      # необходимо сделать русские названия колонок
      names(df) <- tibble(name_enu=names(df)) %>%
        left_join(dic_df, by=c("name_enu")) %>% 
        # санация
        mutate(name_rus={map2_chr(.$name_rus, .$name_enu, 
                                  ~if_else(is.na(.x), .y, .x))}) %>% 
        pull(name_rus)

      df %>%
        # сделаем вывод в формате, принимаемым Excel
        write.table(file, na="NA", append=FALSE, col.names=TRUE, 
                    row.names=FALSE, sep=";", fileEncoding="windows-1251")
    }
  )
}

shinyApp(ui=ui, server=server, enableBookmarking="url")
