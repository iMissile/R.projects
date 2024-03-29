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
library(formattable)
library(anytime)
library(tictoc)
library(digest)
library(officer)
library(openxlsx)
library(assertive)
library(assertr)

options(shiny.usecairo=TRUE)
options(shiny.reactlog=TRUE)
options(spinner.type=4)

source("clickhouse.R")
eval(parse("funcs.R", encoding="UTF-8"))

# очистим все warnings():
assign("last.warning", NULL, envir = baseenv())

# определяем окружение в котором запускаемся
Sys.setenv("R_CONFIG_ACTIVE"="media-tel")
Sys.setenv("R_CONFIG_ACTIVE"="cti")

# ================================================================
ui <- 
  navbarPage(
  # title=HTML('<div><a href="http://devoteam.com/"><img src="./img/devoteam_176px.png" width="80%"></a></div>'),
  title="Статистика телесмотрения",
  tabPanel("Форма запросов", value="general_panel"),
  tabPanel("Настройки", value="config_panel"),
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
    condition = "input.tsp=='general_panel'",
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
                               separator = " - ", format = "dd/mm/yyyy",
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
      column(6, selectInput("event_filter", "Тип переключения канала", choices=NULL,
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
      column(8, actionButton("reset_btn", "Сброс", class = 'rightAlign')),
      # column(8, tags$a(href="javascript:history.go(0)", 
      #                    popify(tags$i(class="fa fa-refresh fa-5x"),
      #                         title="Reload", 
      #                         content="Рестарт сессии",
      #                         placement="right"))      
      # ),
      column(1, actionButton("set_today_dates_btn", "На сегодня")),
      column(1, actionButton("process_btn", "Применить", width="100%")),
      #column(1, div(bookmarkButton(label="Закладка..."), class = 'rightAlign')),
      column(1, downloadButton("csv_download_btn", label="Сохр. CSV")),
      column(1, downloadButton("xls_download_btn", label="Сохр. Excel"))
      
    ),
    h3("Выборка"),
    fluidRow(
      column(12, verbatimTextOutput('info_text')),
      # https://stackoverflow.com/questions/23233497/outputting-multiple-lines-of-text-with-rendertext-in-r-shiny
      tags$style(type="text/css", "#info_text {white-space: pre-wrap;}")
    ),
    #tags$style(type='text/css', "#in_date_range { position: absolute; top: 50%; transform: translateY(-80%); }"),
    tabsetPanel(
      id="panel_id",
      selected="table_tab",
      tabPanel("Таблица", value="table_tab",
               fluidRow(
                 p(),
                 # column(12, div(withSpinner(DT::dataTableOutput('stat_table'))), style="font-size: 90%")
                 column(12, div(DT::dataTableOutput("stat_table")), style="font-size: 90%")
               )
      )
    )
  ),
  conditionalPanel(
    # config panel -----------------------
    condition = "input.tsp=='config_panel'",
    fluidRow(
      column(2, checkboxInput("debug_cbx", label="Диагностический вывод", value = FALSE, width = NULL))
    )
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
  
  # префикс имени файла при генерации выгрузок
  fname_prefix <- "slice_"

  shinyjs::hide("csv_download_btn")
  shinyjs::hide("xls_download_btn")
  
  # выставим даты на первоначальный диапазон данных
  updateDateRangeInput(session, "in_date_range", 
                       start=config::get("initial_values")$begin_time_range, 
                       end=config::get("initial_values")$end_time_range)
  
  # Sys.getenv("R_CONFIG_ACTIVE")
  ch_db <- config::get("clickhouse") # достаем параметры подключения
  # создаем коннект к инстансу CH -----------
  conn <- dbConnect(clickhouse(), host=ch_db$host, port=ch_db$port, user=ch_db$user, password=ch_db$password)

  # заполним список доступных к выбору таблиц
  updateSelectInput(session, "select_ch_table", choices=unique(c(ch_db$table, "states")), selected=ch_db$table)

  # подгрузим таблицу преобразования транслита в русские названия городов -------
  city_translit_df <- {
    flog.info("Loading cities translit table")
    df <- req(dbGetQuery(conn, "SELECT * FROM regnames")) %>%
      verify(has_all_names("date", "russian", "translit")) %>%
      mutate_if(is.character, `Encoding<-`, "UTF-8") %>%
      # для каждой строки перевода возьмем самое старшее значение
      group_by(translit) %>%
      filter(date==max(date)) %>%
      ungroup() %>% 
      select(-date)
    flog.info(paste0("Cities translit table loaded ", nrow(df), " rows"))
    # dbDisconnect(con)
    df
  }

  # подгрузим таблицу преобразования транслита в русские названия городов -------
  cities_df <- {
    # подгрузим ограниченный список городов
    city_subset <- read_csv("region.csv", col_names=FALSE)
    
    df <- city_translit_df %>%
      filter(translit %in% pull(city_subset))
    # browser()
    df
  }

  # подгрузим таблицу перевода тэгов, используется для формирования запросов (enu) и отображения (rus)  -------
  # tag, enu, rus
  tag_dict_df <- read_excel("tag_dict.xlsx") %>%
    verify(has_all_names("tag", "enu", "rus"))
  # заполним список доступных к выбору событий
  updateSelectInput(session, "event_filter", 
                    choices=tag_dict_df %>% filter(tag=="switchevent") %>% {setNames(.$enu, .$rus)}, 
                    selected=NULL)

  # подгрузим таблицу преобразования идентификатора канала в русское название ----
  progs_df <- jsonlite::fromJSON("channels.json", simplifyDataFrame=TRUE) %>% 
    select(channelId, channelName=name) %>%
    as_tibble()

  # создаем tidy модель данных
  data_model_df <- jsonlite::fromJSON("datamodel.json", simplifyDataFrame=TRUE) %>%
      as_tibble()

  # модель для переменных под агрегат
  var_model_df <- {    
    # построим модель переменных для вычисления
    df1 <- data_model_df %>%
      # в переменные берем только то, что подпадает под агрегаты
      filter(!is.na(aggr_ops)) %>%
      # 1-ая декомпозиция: по агрегатам
      separate_rows(aggr_ops, sep="[,;[:space:]]+") %>%
      # 2-ая декомпозиция: по вычислению долей
      separate(aggr_ops, into=c("aggr_ops", "ratio_type"), sep="[:[:space:]]+") %>%  
      # переводим алиасы функций агрегации в различные узлы (экран, CH)
      mutate(x=aggr_ops,
             ext_aggr_opts=case_when(
               x=="min" ~ "мин, min",
               x=="max" ~ "макс, max",
               x=="mean" ~ "ср, avg",
               x=="sum" ~ "сумма, sum",
               x=="unique" ~ "уник, uniq",           
               x=="count" ~ "всего, count",
               TRUE ~ "UNKNOWN"
             )) %>%
      # разнесем на отдельные колонки
      separate(ext_aggr_opts, into=c("visual_aggr_func", "ch_aggr_func")) %>%
      select(-x, -col_label) %>%
      mutate(ch_query_name=str_c(ch_aggr_func, "(", db_field, ")")) %>%
      mutate(internal_name=ch_query_name)
    
    # добавим дробные отношения как самостоятельные агрегатные переменные
    df2 <- df1 %>%
      filter(!is.na(ratio_type)) %>%
      mutate(can_be_grouped=FALSE) %>%
      mutate_at(vars(visual_aggr_func), ~str_c(.x, ", % от общего")) %>%
      mutate(internal_name=str_c(ch_aggr_func, db_field, "ratio", sep="_"))
    # mutate(internal_name=stri_join(ch_aggr_func, db_field, "ratio", sep="_", collapse=NULL))
    # mutate(internal_name={map2_chr(.$internal_name, .$db_field, ~if_else(is.na(.x), .y, .x))})
    
    
    # объединим все в единую модель
    res_df <- df1 %>%
      mutate(ratio_type=as.character(NA)) %>%
      bind_rows(df2) %>%
      # select(-src) %>%
      mutate(visual_var_name={map2_chr(.$human_name_rus, .$visual_aggr_func,
                                       ~if_else(.y=="", .x, stri_join(.x, ": ", .y)))}) %>%
      mutate(id=row_number())
    
    # browser()
    res_df
  }
  
  # модель переменных для группировки
  group_model_df <- {    
    # построим модель переменных для вычисления
    df <- data_model_df %>%
      filter(can_be_grouped) %>%
      mutate(visual_group_name=human_name_rus) %>%
      select(internal_name=db_field, visual_group_name) %>%
      mutate(id=row_number()) %>%
      # добавим пустую строку, позволяющую не выбирать агрегат
      add_row(id=0, visual_group_name="нет") %>%
      arrange(id)

    df
  }
  
  dict_df <- {
    # словарь для преобразований имен полей из английских в русские
    # имена колонок -- группы и агрегаты из запроса
    # но есть еще и просто переменные для переименования, у них флаг can_be_grouped=FALSE, aggr_ops=NA
    df <- data_model_df %>%
      filter(!can_be_grouped & is.na(aggr_ops))
    # сливаем модельные данные
    dplyr::union(var_model_df %>% select(name_enu=internal_name, name_rus=visual_var_name),
                 group_model_df %>% select(name_enu=internal_name, name_rus=visual_group_name)) %>%
                 dplyr::union(df %>% select(name_enu=db_field, name_rus=human_name_rus))
  }
  

  # browser()
  # реактивные переменные -------------------
  msg <- reactiveVal("")
  sql_request <- reactiveVal("")
    
  raw_df <- reactive({
    r <- req(sql_request())
    flog.info(paste0("DB request: ", r))

    tic()
    df <- dbGetQuery(conn, r) %>%
      as_tibble() %>%
      mutate_if(is.character, `Encoding<-`, "UTF-8")
    
    flog.info(paste0("Query: ", capture.output(toc())))
    flog.info(paste0("Table: ", capture.output(head(df, 2))))
    flog.info(paste0("Loaded ", nrow(df), " rows"))

    # t <- df %>% head(5) %>% select_if(function(x){cat(" <- "); str(x); cat(" ->\n"); TRUE})
    # для переменных, содержащих %-ную долю, надо применить округление
    # составим список имен колонок для которых мы ожидаем получить долевую часть
    ratio_vars <- var_model_df %>%
      filter(!is.na(ratio_type)) %>%
      filter(internal_name %in% names(df)) %>%
      pull(internal_name)
    # для всех остальных агрегатных числовых переменных применяем округление до 1
    non_ratio_vars <- var_model_df %>%
      filter(is.na(ratio_type)) %>%
      filter(internal_name %in% names(df)) %>%
      pull(internal_name)
    # browser()

    # составим список оставшихся переменных
    other_vars <- names(df) %>% {.[!(. %in% var_model_df$internal_name)]}
    
    res_df <- df %>% 
      mutate_at(vars(one_of(ratio_vars)), ~(round(.x, 4))) %>%
      # добавим маркер форматирования для вывода в excel
      mutate_at(vars(one_of(ratio_vars)), `class<-`, "percentage") %>%
      mutate_at(vars(one_of(non_ratio_vars)), ~(round(.x, 0))) %>%
      # прогоним по оставшимся переменным
      mutate_at(vars(other_vars), ~{if(is.numeric(.x)) round(.x, 0) else .x})

    res_df
  })  

  cur_df <- reactive({
    df <- raw_df()    
    shiny::validate(
      need(df, "data.frame structure must be defined"),
      # при пустом значении tibble решает, что тип переменной -- logi
      need(nrow(df)>0, "В рамках установленных ограничений данные полностью отсутствуют")
    )

    # сделаем русские имена городов, если таковые есть в выборке
    if("region" %in% names(df)){
      df %<>% 
        # транслируем города в соотв. с ПОЛНОЙ таблицей транслита
        left_join(city_translit_df, by=c("region"="translit")) %>%
        # санация
        mutate(region=if_else(is.na(russian), str_c("<<", region, ">>"), russian)) %>%
        # select(region, -russian, everything())
        select(-russian)
    }

    # переведем события, если таковые есть в выборке
    if("switchevent" %in% names(df)){
      df %<>%
        # транслируем событие переключения
        left_join(filter(tag_dict_df, tag=="switchevent"), by=c("switchevent"="enu")) %>%
        # санация, для уже определенной колонки ее позиция остается неизменной
        mutate(switchevent=if_else(is.na(rus), switchevent, rus)) %>% 
        select(-tag, -rus)
    }
    
    df
  })
  
  # таблица с выборкой по каналам ----------------------------
  output$stat_table <- DT::renderDataTable({
    df <- req(cur_df())
    
    # browser()
    # делаем русские имена колонок в выводе
    colnames_df <- tibble(name_enu=names(df)) %>%
      left_join(dict_df, by=c("name_enu")) %>%
      # санация
      mutate(name_rus={map2_chr(.$name_rus, .$name_enu, 
                                ~if_else(is.na(.x), .y, .x))})
    
    # добавим форматный вывод для %-ных долей
    ratio_vars <- var_model_df %>%
      filter(!is.na(ratio_type)) %>%
      filter(internal_name %in% names(df)) %>%
      pull(internal_name)
    
    # browser()
    # форматирование можно применять только если у нас есть список колонок
    attributes(ratio_vars) <- NULL
    fmt_df <- if(identical(ratio_vars, character(0))){
      formattable(df)
      } else {
        formattable(df, list(area(col=ratio_vars) ~ 
                             dvt_color_bar("lightblue", function(x) x/100)))
      }
    # color_bar("lightblue", function(x) formattable::normalize(x, 0, 100))))

    # https://stackoverflow.com/questions/39970097/tooltip-or-popover-in-shiny-datatables-for-row-names
    # в конструкторе в качестве подсказки отображается англоязычный эквивалент колонки, но не исходная метаподсказка
    colheader <- htmltools::withTags(
     table(class = 'display',
           thead(
             tr(colnames_df %>%
                  {purrr::map2(.$name_enu, .$name_rus, ~th(title=.x, .y))})
             )))
    # browser()
    # https://rstudio.github.io/DT/functions.html
    DT::datatable(df,
                  # formattable превращает числа в строки, тем самым нарушая возможности сортровки
                  # as.datatable(fmt_df, # для сохранения параметров formattable
                  class='cell-border stripe',
                  rownames=FALSE,
                  filter='bottom',
                  container=colheader,
                  options=list(dom='fltip', 
                               pageLength=7, 
                               lengthMenu=c(5, 7, 10, 15, 50),
                               scrollCollapse=TRUE,
                               scrollX=TRUE)
                  ) %>%
      DT::formatPercentage(ratio_vars, digits=2)
    })
  
  # управляем отображением дебаг информации --------
  observeEvent(input$debug_cbx, {
    shinyjs::toggle("info_text")
  })
  
  # ресет фильтров ---------
  observeEvent(input$reset_btn, {
    # shinyjs::
    reset("in_date_range")
    reset("serial_mask")
    reset("duration_range")
    reset("prefix_filter")
    reset("event_filter")
    reset("region_filter")
    reset("channel_filter")
    reset("debug_cbx")
    reset("group_var1")
    reset("selected_vars")
  })
   
  # динамическое управление диапазоном дат ---------
  observeEvent(input$history_depth, {
    # $history_depth получаем как строку
    date <- Sys.Date()-as.numeric(input$history_depth)
    flog.info(paste0("Start date changed to ", date))
    # updateDateRangeInput(session, "in_date_range", start=date)
    }
  )

  # фиксим даты на демо диапазон ---------  
  observeEvent(input$set_test_dates_btn, {
    start <- "2017-03-01"
    end <- "2017-03-02"
    updateDateRangeInput(session, "in_date_range", start=start, end=end)
  })

  # фиксим даты на сегодняшнюю дату ---------  
  observeEvent(input$set_today_dates_btn, {
    end <- Sys.Date()
    start <- end - days(2)
    updateDateRangeInput(session, "in_date_range", start=start, end=end)
  })
  
    
  # управляем визуализацией кнопок выгрузки ----- 
  observeEvent(cur_df(), {
    # msg(capture.output(str(session)))
    # browser()
    if(!is.null(raw_df()) & nrow(raw_df())>0) {
      shinyjs::show("csv_download_btn")
      shinyjs::show("xls_download_btn")
    } else {
      shinyjs::hide("csv_download_btn")
      shinyjs::hide("xls_download_btn")
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
    exclude_var <- left_join(as_tibble(idx), group_model_df, by=c("value"="id")) %>% 
      drop_na() %>% 
      pull(internal_name)
    # browser()
    df <- var_model_df %>%
      filter(!(internal_name %in% exclude_var))
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
    data <- cities_df %>% {setNames(object=.$translit, .$russian)}
    selectInput("region_filter", 
                paste0("Регион (", length(data), ")"),
                multiple=TRUE, choices=data, width = "100%")
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
    # сначала проверим и пофиксим даты в элементе управления на демо диапазон 
    # в случае принудительного ручного обнуления полей дат
      # сначала верхнюю дату
    bd <- input$in_date_range[1] # format: Date
    ed <- input$in_date_range[2] # format: Date
    if(is.na(ed)) ed <- ymd("2017-03-02")
    if(is.na(bd) || bd>ed) bd <- ed-days(1)
    updateDateRangeInput(session, "in_date_range", start=bd, end=ed)
    
    # собираем общие условия в соотв. с фильтрами
    where_string <- paste0(paste0(" date >= '", input$in_date_range[1], "' AND date <= '", input$in_date_range[2], "' "),
                           paste0("AND duration >= ", input$duration_range[1]*60, " AND duration <= ", input$duration_range[2]*60, " "),
                           buildReqFilter("region", input$region_filter, add=TRUE),
                           buildReqFilter("prefix", input$prefix_filter, add=TRUE),
                           buildReqFilter("channelId", input$channel_filter, add=TRUE),
                           buildReqFilter("switchevent", input$event_filter, add=TRUE),
                           ifelse(input$serial_mask=="", "", paste0(" AND like(serial, '%", input$serial_mask, "%') "))
    ) 
    
    from_where_string <- str_c(" FROM", ch_db$table, 
                                "WHERE ", where_string, sep=" ")
    
    # определяем, есть ли переменные в select
    # has_select <- !all(purrr::map_lgl(list(input$group_var1, input$group_var2,
    #                                        input$group_var3, input$selected_vars), 
    #                                   is.null))
    
    # собираем переменные групировки (в SELECT & GROUP BY)
    idx <- map_int(1:3, ~as.integer(input[[stri_join("group_var", .x)]]))
    df0 <- left_join(as_tibble(idx), group_model_df, by=c("value"="id")) %>% drop_na()
    group_vars <- stri_join(df0$internal_name, collapse=", ")
    
    # если переменных нет, то имеем character(0)
    # это важно для последующего слияния.... иначе получится "" вместо *
    has_select_var <- any(length(group_vars) > 0, !is.null(input$selected_vars))

    limit_string <- ""    
    if(has_select_var){
      # 1. для единообразной обработки построим динамический SELECT для %-ных параметров
      df <- var_model_df %>%
        mutate(ch_query_name=if_else(is.na(ratio_type), ch_query_name, 
                          str_c(ch_query_name, " / ( SELECT", ch_query_name, from_where_string, 
                                ") AS", internal_name, sep=" ")))
      # 2. построим список переменных в часть SELECT
      vars_string <- df %>%
        filter(id %in% input$selected_vars) %>%
        pull(ch_query_name) %>%
        # purrr::map_chr(~str_c(.x, "")) %>%
        stri_join(sep="", collapse=", ")
      # browser()
      
      # 3. объединим с групповыми переменными
      select_string <- stri_join(group_vars, vars_string, sep=", ", collapse="", ignore_null=TRUE)
    } else {
      # не выбрана ни одна переменная
      select_string <- " * "
      limit_string <- "LIMIT 2000"
    }
    
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
        left_join(dict_df, by=c("name_enu")) %>% 
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

  # выгрузка таблицы в XLS -----------------------  
  output$xls_download_btn <- downloadHandler(
    filename = function() {
      paste0(fname_prefix, "", format(Sys.time(), "%F_%H-%M-%S"), ".xlsx", sep="")
    },
    content = function(file) {
      df <- cur_df()
      # необходимо сделать русские названия колонок
      names(df) <- tibble(name_enu=names(df)) %>%
        left_join(dict_df, by=c("name_enu")) %>% 
        # санация
        mutate(name_rus={map2_chr(.$name_rus, .$name_enu, 
                                  ~if_else(is.na(.x), .y, .x))}) %>% 
        pull(name_rus)
      # %-ное форматирование долевых колонок мы установили при формировании источника
      # browser()
      # write.xlsx(df, file=file, asTable=TRUE, colWidths="auto")
      n <-length(df)
      wb <- createWorkbook()
      addWorksheet(wb, "Выгрузка", zoom=85)
      writeDataTable(wb, sheet=1, x=df)
      setColWidths(wb, sheet=1, cols=1:n, widths=30)
      addWorksheet(wb, "Описание")
      saveWorkbook(wb, file, overwrite=TRUE)
    }
  )
  
  # обработчки сохранения состояний статических элементов
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

shinyApp(ui=ui, server=server, enableBookmarking="url")
