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
library(DT)
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
  tabPanel("Проектная смета", value="general_panel"),
  tabPanel("Настройки", value="config_panel"),
  id="mainNavbarPage",
  selected="config_panel",
  # collapsible=TRUE,
  # theme=shinytheme("flatly"),
  theme=shinytheme("yeti"),
  # shinythemes::themeSelector(),
  # includeCSS("styles.css"),

  # http://stackoverflow.com/questions/25387844/right-align-elements-in-shiny-mainpanel/25390164
  tags$head(tags$style(".rightAlign{float:right;}")), 

  conditionalPanel( # config panel -----------------------
    condition = "input.mainNavbarPage == 'config_panel'",
    fluidRow(
      tags$style(type='text/css', '#cweather_text {white-space:pre;}')
      # tags$style(type='text/css', 'div {background-color: #000с00;}'), 
      
      #column(6, h2("Типовая форма"), h3(textOutput("cweather_text", inline=TRUE))),
      #column(6, h2("Заполнитель"))
      ),
    fluidRow(
      column(4, fileInput('project_plan', 'Выбор .xlsx файла с планом',
                          #accept=c('application/vnd.ms-excel', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')),
                          accept = c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'))
      )
    )
  ),
  conditionalPanel( # general panel -----------------------
    condition = "input.mainNavbarPage == 'general_panel'",
    # https://stackoverflow.com/questions/28960189/bottom-align-a-button-in-r-shiny
    tags$style(type='text/css', "#set_today_btn {margin-top: 25px;}"),
    tags$style(type='text/css', "#set_test_dates_btn {margin-top: 25px;}"),
    tags$style(type='text/css', "#process_btn {margin-top: 25px;}"),

    #tags$style(type='text/css', "#in_date_range { position: absolute; top: 50%; transform: translateY(-80%); }"),
    tabsetPanel(
      id = "main_panel",
      selected="table_tab",
      tabPanel("Полная сметная стоимость", value="table_tab", # ---------
               fluidRow(
                 p(),
                 column(12, div(withSpinner(DT::dataTableOutput("stat_table"))), style="font-size: 90%")
               ),
               p(),
               fluidRow(
                 # параметр разреза соответствует переменной группировки
                 column(8, {}),
                 column(2, downloadButton("csv_download_btn", label="Экспорт (Excel)", class = 'rightAlign')),
                 column(2, downloadButton("word_download_btn", label="Экспорт (Word)", class = 'rightAlign'))
               ),
               fluidRow(
                 #column(10, plotOutput("subset_plot")),
                 column(12, textOutput("info_text"))
               )               
      ),
      tabPanel("Empty", value="graph_tab", # ----------
               fluidRow(
                 # p()
                 # column(11, {}),
                 # column(12, div(selectInput("top_num", "Кол-во в ТОП:", 
                 #                        choices=c(3, 5, 7, 10, 20), 
                 #                        selected=5), 
                 #                class='rightAlign'))
               ),
               fluidRow(
                 # p(),
                 # jqui_sortabled(
                 #   div(id='top10_plots',
                 #       column(6, div(withSpinner(plotOutput('top10_duration_plot', height="500px")))),
                 #       column(6, div(withSpinner(plotOutput('top10_stb_plot', height="500px"))))
                 #       ))
                 )
               )
      ),
    p(),
    
    # https://github.com/daattali/shinyjs/issues/121
    div(id="slice_panel_div", # drill-down таблица ---------
        mainPanel(
          tabsetPanel(
            id = "slice_panel",
            selected="slice_table_tab",
            tabPanel("Разрез объектов", value="slice_table_tab", # ----------------
                     fluidRow(
                       p(),
                       column(12, div(withSpinner(DT::dataTableOutput("slice_table"))), 
                              style="font-size: 90%")
                     )
            ),
            tabPanel("Графическая структура затрат", value = "slice_graph_tab", # ----------------
                     fluidRow(
                       p(),
                       column(12, div(withSpinner(plotOutput('slice_plot', height="500px"))))
                     )
            )
          ),
          width=10
        ), # ----------------
        sidebarPanel( 
          selectInput("slice_filter", "Представить в разрезе:",
                      choices=c('Все данные'=' ',
                                'Код вида ОССР'='ossr_type', 
                                'Код ОССР'='ossr_code', 
                                'Глава ССР'='ssr_chap')
                      ),
          numericInput("obs", "Observations:", 10),
          width=2
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
    # browser()
    cleanNames(req(raw_df())) %>% rebaseCost()
  })
  
  oks_summary_df <- reactive({
    req(clean_df())
    oks_dict <- tibble(oks_code=c("0001", "0002"), oks_name=c("ДКС.2В", "УКПГ.2В"))
    
    df <- clean_df() %>%
      group_by(oks_code) %>%
      summarise(direct_cost=sum(est_cost), indirect_cost=sum(overcost)) %>%
      left_join(oks_dict) %>%
      select(oks_code, oks_name, everything())
  })
  
  # формируем срез в зависимости от выбранной строки и параметров -------------------
  slice_df <- reactive({
    req(clean_df())
    ids <- req(input$stat_table_rows_selected) # проводим анализ при выборе строки в таблице
    flog.info(paste0("Selected row num is ", ids, ". Data row: ",
                     capture.output(str(oks_summary_df()[ids, ]))))
    # ОКС достаем из выбранной строки
    oks_code <- oks_summary_df()[[ids, "oks_code"]]
    oks_code_val <- enquo(oks_code) # превратили в строку

    df <- clean_df() %>%
      filter(oks_code==!!oks_code_val) %>%
      select(-oks_code, -oks_type, -est_cost_entry) %>%
      rename(direct_cost=est_cost, indirect_cost=overcost) %>%
      # move to end
      select(-indirect_cost, -direct_cost, everything()) %>%
      arrange(desc(direct_cost))
    df
  })
  
  msg <- reactiveVal("")

  # таблица-свертка по ОКС  ----------------------------
  output$stat_table <- DT::renderDataTable({
    df <- req(oks_summary_df())
    # https://rstudio.github.io/DT/functions.html
    DT::datatable(df,
                  class='cell-border stripe',
                  rownames=FALSE,
                  colnames=c('Код ОКС'='oks_code', 'Наименование ОКС'='oks_name',
                             'Прямые затраты'='direct_cost', 'Косвенные затраты'='indirect_cost'),
                  filter='bottom',
                  # selection=list(mode="multiple", target="row"),
                  selection=list(mode="single", target="row"),
                  # selection="single",
                  options=list(dom='fltip', #autoWidth=TRUE, 
                               pageLength=7, lengthMenu=c(5, 7, 10, 15),
                               order=list(list(3, 'desc'))))
    })
  
  # таблица-детализация по ОКС в разрезе ССР  ----------------------------
  output$slice_table <- DT::renderDataTable({
    df <- req(slice_df())
    # browser()
    # https://rstudio.github.io/DT/functions.html
    DT::datatable(df,
                  class='cell-border stripe',
                  rownames=FALSE,
                  colnames=c(# 'Код ОКС'='oks_code', 
                             'Код вида ОССР'='ossr_type', 
                             'Код ОССР'='ossr_code', 
                             'Глава ССР'='ssr_chap',
                             'Наименование ОКС'='sd_name',
                             'Прямые затраты'='direct_cost', 'Косвенные затраты'='indirect_cost'),
                  filter='bottom',
                  # selection=list(mode="multiple", target="row"),
                  selection=list(mode="single", target="row"),
                  # selection="single",
                  options=list(dom='fltip', #autoWidth=TRUE, 
                               pageLength=7, lengthMenu=c(5, 7, 10, 15)))
  })
    
  # график структуры затрат в разрезе ОССР -------------
  output$slice_plot <- renderPlot({
    req(slice_df())
    # shiny::validate(
    #   need(!is.null(cur_df()), "NULL value can't be renederd"),
    #   need(nrow(cur_df())>0, "0 rows -- nothing to draw")
    #   )
    
    # browser()
    plotOSSRslice(slice_df())
  })    

  
  # служебный вывод ---------------------  
  output$info_text <- renderText({
    msg()
    pplan()
  })

  # обработчики событий ------------------------------
  
  # переключаем панель после загрузки
  observeEvent((input$project_plan$datapath), {
    # browser()
    flog.info(paste0("Switching to general panel"))
    updateNavbarPage(session, "mainNavbarPage", selected="general_panel")
    # Run JS code that simply shows a message
    # runjs("var today = new Date(); alert(today);")
    #flog.info(paste0("Switching to general panel again"))
  })  

  observe({
    # управляем визуализацией табсета с детализированным графиком -----
    if(is.null(input$stat_table_rows_selected)) {
      shinyjs::hide("slice_panel_div")
    } else {
      shinyjs::show("slice_panel_div")
    }
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
