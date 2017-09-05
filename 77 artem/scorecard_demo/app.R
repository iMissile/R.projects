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
  title = "Демо Форма",
  tabPanel("Выпуск\\отгрузка", value="general_panel"),
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
      )
    ),
    # https://stackoverflow.com/questions/28960189/bottom-align-a-button-in-r-shiny
    tags$style(type='text/css', "#set_today_btn {margin-top: 25px;}"),
    tags$style(type='text/css', "#set_test_dates_btn {margin-top: 25px;}"),
    tags$style(type='text/css', "#process_btn {margin-top: 25px;}"),

    #tags$style(type='text/css', "#in_date_range { position: absolute; top: 50%; transform: translateY(-80%); }"),
    tabsetPanel(
      id = "main_panel",
      selected="graph_tab",
      tabPanel("none", value="table_tab",
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
      tabPanel("График", value="graph_tab",
               fluidRow(
                 # column(12, {}),
                 # p(),
                 column(12, h3("Выпуск продукции \U21D1, \U21E7"),
                        div(withSpinner(plotOutput('output_plot', height="300px"))))
               )
               )
      ),
    # https://github.com/daattali/shinyjs/issues/121
    div(id="slice_panel_div", 
    tabsetPanel(
      id = "slice_panel",
      selected="slice_table_tab",
      tabPanel("Разрез объектов ССР", value="slice_table_tab",
               fluidRow(
                 column(12, div(withSpinner(DT::dataTableOutput("slice_table"))), style="font-size: 90%")
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

  raw_df <- reactive({
    fname <- req(input$project_plan$datapath)
    flog.info(paste0("Loading '", fname, "'"))
    read_excel(fname) %>%
      mutate_at(vars(docdate), as_date)
  })  

  subset_df <- reactive({
    req(raw_df()) %>% 
      mutate(deparse=stri_replace_all_regex(name, "(.+\\sТК)\\s+(СКБК|ПЗБМ)\\s?(ОБФ\\s\\d)?\\s?(.*)", 
                                            "$1_$2_$3_$4")) %>%
      separate(deparse, into=c("operation", "group", "material", "kpi"), sep="_") %>%
      # выберем только интересующие операции
      filter(operation %in% c("Выпуск ТК", "Склад ТК", "Отгрузка ТК")) %>%
      # и выкинем весь служебный шлак
      select(-id, -unit, -firmcode, -docnum)
  })
    
  score_df <- reactive({
    # выбираем данные на конкретную дату
    c_date <- dmy("10.09.2015") # current date
    
    # надо будет добавить сюда еще Итого
    df0 <- req(subset_df()) %>%
      filter(docdate>=c_date-days(2) & docdate<=c_date) %>%
      filter(material!="" & kpi=="")
    
    # надо будет сформировать и добавить сюда еще "Итого"
    df1 <- df0 %>%
      group_by(docdate) %>%
      summarise(actualvalue=sum(actualvalue), planvalue=sum(planvalue), material="Итого", kpi="")    
    
    bind_rows(df0, df1) %>%
      filter(docdate>=c_date-days(2) & docdate<=c_date) %>%
      filter(material!="" & kpi=="") %>%
      mutate(status=as.character(if_else(is.na(planvalue), TRUE, actualvalue>planvalue))) %>%
      mutate(label=format(actualvalue, big.mark=" "))
  })
  
  
  msg <- reactiveVal("")

  # таблица-свертка по ОКС  ----------------------------
  output$stat_table <- DT::renderDataTable({
    df <- req(score_df())

    # https://rstudio.github.io/DT/functions.html
    DT::datatable(df,
                  class='cell-border stripe',
                  rownames=FALSE,
                  filter='bottom',
                  # selection=list(mode="multiple", target="row"),
                  selection=list(mode="single", target="row"),
                  # selection="single",
                  options=list(dom='fltip', #autoWidth=TRUE, 
                               pageLength=7, lengthMenu=c(5, 7, 10, 15),
                               order=list(list(3, 'desc'))))
    })
  
    
  
  # график структур  -------------
  output$output_plot <- renderPlot({
    plotOutputScore(req(score_df()))
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
