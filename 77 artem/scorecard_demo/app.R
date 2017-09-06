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
  title = "Scoreboard",
  tabPanel("Выпуск\\отгрузка", value="general_panel"),
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

  # ----------------
  conditionalPanel(
    # general panel -----------------------
    condition = "input.mainNavbarPage == 'config_panel'",
    fluidRow(
      column(4, fileInput('project_plan', 'Выбор .xlsx файла с KPI',
                          #accept=c('application/vnd.ms-excel', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'))
                          accept = c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'))
      )
    )
    ),
  # ----------------
  conditionalPanel(
    # general panel -----------------------
    condition = "input.mainNavbarPage == 'general_panel'",
    # https://stackoverflow.com/questions/28960189/bottom-align-a-button-in-r-shiny
    
    fluidRow(
      column(8, {}),
      column(2, 
             dateInput("view_date",
                       # label="Дата",
                       label=NULL,
                       # start=Sys.Date()-1, end=Sys.Date(),
                       # при демонстрации на пилотных данных
                       value="2015-09-10",
                       # min="2015-09-01", 
                       # max="2015-09-30",
                       # min = Sys.Date() - 10, 
                       # max = Sys.Date(),
                       format="dd-mm-yyyy",
                       startview="month", language='ru', weekstart=1)
      ),
      column(1, actionButton("prev_date_btn", "\U21D0 -1 день", width="100%")),
      column(1, actionButton("next_date_btn", "+1 день \U21D2", width="100%"))
    ),
    
    tabsetPanel(
      id = "main_panel",
      selected="graph_tab",
      tabPanel("Выпуск", value="graph_tab",
               fluidRow(
                 column(12, h3("Выпуск продукции \U21D1, \U21E7")),
                 column(10, 
                        div(withSpinner(plotOutput('output_plot', height="600px"))))
               )
      ),
      tabPanel("Метрики", value="table_tab",
               fluidRow(
                 p(),
                 column(12, div(withSpinner(DT::dataTableOutput("stat_table"))), style="font-size: 90%")
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
      # на самом деле % НИ считается из НИ и его можно просто выкинуть
      filter(!stri_detect_regex(name, pattern=".*% НИ$")) %>%
      mutate_at(vars(docdate), as_date)
  })  

  raw_kpi_df <- reactive({
    # немного подрихтованные исходные kpi
    df <- req(raw_df()) %>%
      mutate(ratio=round(actualvalue/planvalue*100, digits=1)) %>%
      group_by(docdate) %>%
      arrange(desc(docdate), ratio)
      
    df
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
    # c_date <- dmy("10.09.2015") # current date
    # browser()
    c_date <- ymd(input$view_date) # current date
    
    # надо будет добавить сюда еще Итого
    df0 <- req(subset_df()) %>%
      filter(docdate>=c_date-days(2) & docdate<=c_date) %>%
      # накопительный итог включаем в общее рассмотрение
      filter(material!="" & kpi %in% c("", "НИ"))
    
    # надо будет сформировать и добавить сюда еще "Итого"
    df1 <- df0 %>%
      group_by(docdate, kpi) %>%
      summarise(actualvalue=sum(actualvalue), planvalue=sum(planvalue), material="Итого") %>%
      ungroup()    
    
    bind_rows(df0, df1) %>%
      mutate(status=as.factor(if_else(is.na(planvalue), TRUE, actualvalue>planvalue))) %>%
      mutate(label_up=format(actualvalue, big.mark=" ")) %>%
      mutate(label_down=stri_join(format(round(actualvalue/planvalue*100, digits=0), big.mark=" "), "%")) %>%
      # unite(material, kpi, col=composed_kpi, sep="-")
      mutate(composed_kpi=if_else(kpi=="", material, stri_join(material, kpi, sep="-"))) %>%
      mutate(opacity=if_else(docdate==c_date, "Mark", "Hide"))
  })
  
  
  msg <- reactiveVal("")

  # таблица-свертка по ОКС  ----------------------------
  output$stat_table <- DT::renderDataTable({
    df <- req(raw_kpi_df())

    # https://rstudio.github.io/DT/functions.html
    DT::datatable(df,
                  class='cell-border stripe',
                  rownames=FALSE,
                  filter='bottom',
                  # selection=list(mode="multiple", target="row"),
                  selection=list(mode="single", target="row"),
                  # selection="single",
                  options=list(dom='fltip', #autoWidth=TRUE, 
                               pageLength=10, lengthMenu=c(5, 7, 10, 15)
                               ))
    })
  
    
  
  # график структур  -------------
  output$output_plot <- renderPlot({
    plotOutputScore(req(score_df()))
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

  # динамическое управление датой ---------
  observeEvent((input$prev_date_btn), {
    date <- input$view_date-days(1)
    # if(date>input$view_date){}
    flog.info(paste0("View date changed to  ", date))
    updateDateInput(session, "view_date", value=date)
  })  
  
  # служебный вывод ---------------------  
  output$info_text <- renderText({
    msg()
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
