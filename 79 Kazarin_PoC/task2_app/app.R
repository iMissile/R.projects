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

eval(parse("funcs.R", encoding="UTF-8"))

# очистим все warnings():
assign("last.warning", NULL, envir = baseenv())

# ================================================================
ui <- 
  navbarPage(
  # title=HTML('<div><a href="http://devoteam.com/"><img src="./img/devoteam_176px.png" width="80%"></a></div>'),
  title = "Сметная стоимость объектов",
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
      column(2, fileInput('project_plan', 'Выбор .xlsx файла с планом',
                          #accept=c('application/vnd.ms-excel', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')),
                          accept = c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'))
      ),
      column(2, dateRangeInput("in_date_range",
                               label="Диапазон дат",
                               start=Sys.Date()-1, end=Sys.Date(),
                               # на время отладки
                               # start="2017-06-28", end="2017-06-30",
                               # min = Sys.Date() - 10, 
                               max = Sys.Date(),
                               separator=" - ", format="dd/mm/yyyy",
                               startview="month", language='ru', weekstart=1)
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
      column(4, {}),
      column(2, selectInput("select_ch_table", "Таблица", choices=NULL)),
      column(2, actionButton("set_today_btn", "На сегодня", class='rightAlign')),
      column(2, actionButton("set_test_dates_btn", "На демо дату", class='rightAlign')),
      column(2, actionButton("process_btn", "Применить", class = 'rightAlign'))
    ),
    # https://stackoverflow.com/questions/28960189/bottom-align-a-button-in-r-shiny
    tags$style(type='text/css', "#set_today_btn {margin-top: 25px;}"),
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
               ),
               fluidRow(
                 #column(10, plotOutput("subset_plot")),
                 column(12, textOutput("info_text"))
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
                 jqui_sortabled(
                   div(id='top10_plots',
                       column(6, div(withSpinner(plotOutput('top10_duration_plot', height="500px")))),
                       column(6, div(withSpinner(plotOutput('top10_stb_plot', height="500px"))))
                       )))
               )
      ),
    # https://github.com/daattali/shinyjs/issues/121
    div(id="slice_panel_div", 
    tabsetPanel(
      id = "slice_panel",
      selected="ts_tab",
      tabPanel("Временной график", value="ts_tab",
               fluidRow(
                 column(10, plotOutput("subset_plot")),
                 column(2, selectInput("y_ts_plot", "Параметр по Y:",
                                       c("кол-во уник. STB"="unique_stb",
                                         "всего уник. STB"="total_unique_stb",
                                         "суммарное время"="total_duration",
                                         "кол-во просмотров"="watch_events",
                                         "% уник. STB"="stb_ratio")),
                            selectInput("type_ts_plot", "Тип графика:",
                                        c("гистограмма"="barplot",
                                          "линейная"="lineplot"))
                        )
               )
      ),
      tabPanel("Аналитика по выборке", value = "topn_tab",
               fluidRow(
                 p(),
                 column(12, div(selectInput("slice_top", "Кол-во в ТОП:",
                                            choices=c(3, 5, 7, 10, 20),
                                            selected=5),
                                class='rightAlign'))
               )
      )
    )
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
  flog.info(paste0("App started"))

  shinyjs::hide("slice_panel_div")
  
  # создание параметров оформления для различных видов графиков (screen\publish) ------
  font_sizes <- list(
    "screen"=list(base_size=20, axis_title_size=18, subtitle_size=15),
    "word_A4"=list(base_size=14, axis_title_size=12, subtitle_size=11)
  )

  # browser()
  # реактивные переменные -------------------
  pplan <- reactive({
    # в shiny версии 1.0.5 сохраняется расширение загруженного файла
    # fname <- paste(req(input$project_plan$datapath), ".xlsx", sep="")
    # file.copy(input$pplan$datapath, fname)
    # normalizePath(fname)
  })

  raw_df <- reactive({
    fname <- req(input$project_plan$datapath)
    flog.info(paste0("Loading '", fname, "'"))
    read_excel(fname) %>%
      select(1:19) # отрезали примечания
  })  

  clean_df <- reactive({
    req(raw_df())
    
    cleanNames(raw_df()) %>% rebaseCost()
  })
  
  
  # формируем time-series детализацию по аналогии с 3-им отчетом -------------------
  detail_df <- reactive({
  })
  
  msg <- reactiveVal("")

  # таблица с выборкой по регионам ----------------------------
  output$stat_table <- DT::renderDataTable({
    df <- req(clean_df())

    # https://rstudio.github.io/DT/functions.html
    DT::datatable(df,
                  rownames=FALSE,
                  filter='bottom',
                  selection=list(mode="multiple", target="row"),
                  # selection=list(mode="single", target="row"),
                  # selection="single",
                  options=list(dom='fltip', pageLength=7, lengthMenu=c(5, 7, 10, 15),
                               order=list(list(3, 'desc'))))
    })
  
  # вывод time-series графика по выбранному элементу ---------------------  
  output$subset_plot <- renderPlot({
    shiny::validate(
      need(!is.null(detail_df()), "NULL value can't be renederd"),
      need(nrow(detail_df())>0, "0 rows -- nothing to draw") 
    )
    plotRegionHistory(detail_df(), input$y_ts_plot, input$type_ts_plot, publish_set=font_sizes[["screen"]])
  })
    
    
  # график Топ10 каналов по суммарному времени просмотра -------------
    
  output$duration_plot <- renderPlot({
    shiny::validate(
      need(!is.null(cur_df()), "NULL value can't be renederd"),
      need(nrow(cur_df())>0, "0 rows -- nothing to draw")
      )

    plotTop10Duration(cur_df(), publish_set=font_sizes[["screen"]], 
                      ntop=as.integer(input$top_num))
    })    
 
  
  # график Топ10 каналов по суммарному времени просмотра -------------
  output$top10_duration_plot <- renderPlot({
    shiny::validate(
      need(!is.null(cur_df()), "NULL value can't be renederd"),
      need(nrow(cur_df())>0, "0 rows -- nothing to draw") 
    )
    
    plotTop10Duration(cur_df(), publish_set=font_sizes[["screen"]], 
                      ntop=as.integer(input$top_num))
  })
  
  # график Топ10 каналов по количеству уникальных приставок --------------
  output$top10_stb_plot <-renderPlot({
    shiny::validate(
      need(!is.null(cur_df()), "NULL value can't be renederd"),
      need(nrow(cur_df())>0, "0 rows -- nothing to draw") 
    )
    plotTop10STB(cur_df(), publish_set=font_sizes[["screen"]], 
                 ntop=as.integer(input$top_num))
  })  

  observe({
    # управляем визуализацией табсета с детализированным графиком -----
    if(is.null(input$stat_table_rows_selected)) {
      shinyjs::hide("slice_panel_div")
    } else {
      shinyjs::show("slice_panel_div")
    }
  })  
  
  # служебный вывод ---------------------  
  output$info_text <- renderText({
    msg()
    pplan()
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
        write.table(file, na="NA", append=FALSE, col.names=TRUE, 
                    row.names=FALSE, sep=";", fileEncoding="windows-1251")
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

# =================================
# msg()
# row с 1, col с 0 
# m <- req(input$stat_table_cells_selected)
# m <- req(input$stat_table_rows_selected)

# s <- input$stat_table_rows_all
# paste(m, s)
# df <- cur_df()
# значение ячейки

# колонка с 0 + удаленный регион на английском
# df[[m[[1]], m[[2]]+2]]
# имя колонки
# colnames(df)[[m[[2]]+2]]

# browser()
# flog.info(paste0("Selected row num is ", m, ". Data row: ", capture.output(str(df[m, ]))))
# собираем select для получения time-series данных
# m
