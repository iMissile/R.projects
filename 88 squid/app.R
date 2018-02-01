# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8
rm(list=ls()) # очистим все переменные
gc()

library(tidyverse)
library(lubridate)
library(glue)
library(scales)
library(forcats)
library(readxl)
library(magrittr)
library(stringi)
#library(stringr)
library(futile.logger)
library(jsonlite)
library(Cairo)
library(RColorBrewer)
library(extrafont)
library(hrbrthemes)
#library(DBI)
#library(RPostgreSQL)
#library(config)
library(shiny)
library(shinyjqui)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(anytime)
library(tictoc)
#library(digest)
library(officer)
library(openxlsx)
library(assertr)
library(checkmate)

library(dvtdspack)
# devtools::install_github("imissile/dvtdspack")

dvtdspack::initRCfontInCairo()

options(shiny.usecairo=TRUE)
options(shiny.reactlog=TRUE)
options(spinner.type=4)

eval(base::parse("funcs.R", encoding="UTF-8"))
# очистим все warnings():
assign("last.warning", NULL, envir = baseenv())

# ================================================================
ui <- 
  navbarPage(
  # title=HTML('<div><a href="http://devoteam.com/"><img src="./img/devoteam_176px.png" width="80%"></a></div>'),
  title="Squid статистика",
  tabPanel("Статистика", value="general_panel"),
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
    condition="input.tsp=='general_panel'",
    fluidRow(
      tags$style(type='text/css', '#cweather_text {white-space:pre;}')
      # tags$style(type='text/css', 'div {background-color: #000с00;}'), 
      
      #column(6, h2("Типовая форма"), h3(textOutput("cweather_text", inline=TRUE))),
      #column(6, h2("Заполнитель"))
      ),
    fluidRow(
      column(2, dateRangeInput("in_date_range",
                               label="Диапазон дат",
                               # первоначальное значение дат считываем из конфига
                               # start=Sys.Date()-1, end=Sys.Date(),
                               # на время отладки
                               # start="2017-06-28", end="2017-06-30",
                               # min = Sys.Date() - 10, 
                               # max = Sys.Date(),
                               separator=" - ", format="dd/mm/yyyy",
                               startview="month", language='ru', weekstart=1)
      )
    ),
    fluidRow(
      column(8, {}),
      column(2, actionButton("set_today_btn", "На сегодня", class='rightAlign')),
      column(2, actionButton("process_btn", "Применить", class = 'rightAlign'))
      ),
    # https://stackoverflow.com/questions/28960189/bottom-align-a-button-in-r-shiny
    tags$style(type='text/css', "#set_today_btn {margin-top: 25px;}"),
    tags$style(type='text/css', "#set_test_dates_btn {margin-top: 25px;}"),
    tags$style(type='text/css', "#process_btn {margin-top: 25px;}"),

    #tags$style(type='text/css', "#in_date_range { position: absolute; top: 50%; transform: translateY(-80%); }"),
    tabsetPanel(
      id="main_panel",
      selected="table_tab",
      tabPanel("Таблица", value="table_tab",
               mainPanel(
                 fluidRow(
                   p(),
                   column(12, div(withSpinner(DT::dataTableOutput("slice_table"))), 
                          style="font-size: 90%")
                 ), width=10), 
      # ----------------
               sidebarPanel(
                 p(),
                 selectInput("slice_filter", "Представить в разрезе:",
                             choices=c('Все данные'=' ',
                                       'Код вида ОССР'='ossr_type', 
                                       'Код ОССР'='ossr_code', 
                                       'Глава ССР'='ssr_chap',
                                       'Вид затрат'='cost_element')
                 ),
                 # numericInput("obs", "Observations:", 10),
                 width=2)
      ),
      tabPanel("График", value="graph_tab",
               fluidRow(
                 p(),
                 jqui_sortabled(
                   div(id='top10_plots',
                 column(4, div(withSpinner(plotOutput('top10_left_plot', height="500px")))),
                 column(4, div(withSpinner(plotOutput('top10_center_plot', height="500px")))),
                 column(4, div(withSpinner(plotOutput('top10_right_plot', height="500px"))))
                       )))
               )
      )
  ),
 conditionalPanel(
    # config panel -----------------------
    condition = "input.tsp=='config_panel'",
    fluidRow(
      column(2, selectInput("select_ch_table", "Таблица", choices=NULL)),
      column(2, actionButton("set_test_dates_btn", "На демо дату", class='rightAlign')),
      column(2, checkboxInput("debug_cbx", label="Диагностический вывод", value = FALSE, width = NULL))
    ),
    fluidRow(
      column(12, verbatimTextOutput("info_text")),
      # https://stackoverflow.com/questions/23233497/outputting-multiple-lines-of-text-with-rendertext-in-r-shiny
      tags$style(type="text/css", "#info_text {white-space: pre-wrap;}")
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
  flog.info(glue("App started in <{Sys.getenv('R_CONFIG_ACTIVE')}> environment"))

  # реактивные переменные -------------------
  raw_df <- reactive({
    input$process_btn # обновлять будем вручную
    # загрузим лог squid -------
    loadSquidLog("./data/acc.log")
  })  

  squid_df <- reactive({
    req(raw_df()) %>%
      filter(timestamp>now()-days(2))
  })  

  msg <- reactiveVal("")

  # таблица с выборкой по каналам ----------------------------
  output$stat_table <- DT::renderDataTable({
    df <- req(squid_df())
    
    # https://rstudio.github.io/DT/functions.html
    DT::datatable(df,
                  class='cell-border stripe',
                  rownames=FALSE,
                  filter='bottom',
                  # только после жесткой фиксации колонок
                  container=colheader,
                  options=list(dom='fltip', 
                               pageLength=7, 
                               lengthMenu=c(5, 7, 10, 15, 50),
                               language=dataTableRuStrings,
                               order=list(list(3, 'desc')))) # нумерация с 0
    })
  
  # график Топ10 каналов по суммарному времени просмотра -------------
  output$top10_left_plot <- renderPlot({
    df <- req(squid_df()) %>%
      filter(timestamp>now()-days(1))
      plotTopIpDownload(df, subtitle="за последние сутки")
  })

  output$top10_center_plot <- renderPlot({
    df <- req(squid_df()) %>%
      filter(timestamp>now()-minutes(30))
    plotTopIpDownload(df, subtitle="за последние 30 минут")
  })

  output$top10_right_plot <- renderPlot({
    df <- req(squid_df()) %>%
      filter(timestamp>now()-minutes(20))
    plotTopIpDownload(df, subtitle="за последние 5 минут")
  })
  

  # динамическое управление диапазоном дат ---------
  observeEvent(input$history_depth, {
    # $history_depth получаем как строку
    date <- input$in_date_range[2]-days(as.numeric(input$history_depth))
    flog.info(glue("Start date changed to {date}"))
    updateDateRangeInput(session, "in_date_range", start=date)
    },
    ignoreInit=TRUE
  )

  # установка даты на демо диапазон ---------  
  observeEvent(input$set_test_dates_btn, {
    updateDateRangeInput(session, "in_date_range", start="2016-05-01", end="2016-05-08")
  })
  
  # установка даты на сегодня ---------  
  observeEvent(input$set_today_btn, {
    updateDateRangeInput(session, "in_date_range", start=Sys.Date()-1, end=Sys.Date())
  })

  observe({
    # управляем визуализацией кнопок выгрузки -----
    btn_names <- c("csv_download_btn", "xls_download_btn", "word_download_btn")
    if(checkmate::testDataFrame(squid_df(), min.rows=1))
      purrr::walk(btn_names, shinyjs::enable)
    else
      purrr::walk(btn_names, shinyjs::disable)
  })  
  
  # служебный вывод ---------------------  
  output$info_text <- renderText({
    msg()
  })

  # обработчики кнопок выгрузки файлов --------------------------------------------------
  # выгрузка таблицы в CSV -----------------------  
  output$csv_download_btn <- downloadHandler(
    filename = function() {
      glue("{fname_prefix}data-{format(Sys.time(), '%F_%H-%M-%S')}.csv")
    },
    content = function(file) {
      req(squid_df()) %>%
        # сделаем вывод в формате, принимаемым Excel
        write.table(file, na="NA", append=FALSE, col.names=TRUE, 
                    row.names=FALSE, sep=";", fileEncoding="windows-1251")
    }
  )

  # выгрузка таблицы в XLS -----------------------  
  output$xls_download_btn <- downloadHandler(
    filename = function() {
      glue("{fname_prefix}data-{format(Sys.time(), '%F_%H-%M-%S')}.xlsx")
    },
    content = function(file) {
      req(squid_df()) %>%
        genExcelReport(dict=dict_df) %>%
        saveWorkbook(file, overwrite=TRUE)
    }
  )
  
  # выгрузка таблицы в Word -----------------------
  output$word_download_btn <- downloadHandler(
    filename = function() {
      name <- glue("{fname_prefix}report-{format(Sys.time(), '%F_%H-%M-%S')}.docx")
      flog.info(glue("Word report: '{name}'"))
      name
    },
    content = function(file) {
      doc <- req(squid_df()) %>% # select(-total_unique_stb) %>% # пока убираем, чтобы была консистентная подстановка
        genWordReport(dict=dict_df)
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

shinyApp(ui=ui, server=server)
