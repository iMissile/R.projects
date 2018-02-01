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
      column(8, {}),
      column(2, actionButton("process_btn", "Обновить", class = 'rightAlign'))
      ),
    # https://stackoverflow.com/questions/28960189/bottom-align-a-button-in-r-shiny
    tags$style(type='text/css', "#set_today_btn {margin-top: 25px;}"),
    tags$style(type='text/css', "#set_test_dates_btn {margin-top: 25px;}"),
    tags$style(type='text/css', "#process_btn {margin-top: 25px;}"),

    #tags$style(type='text/css', "#in_date_range { position: absolute; top: 50%; transform: translateY(-80%); }"),
    tabsetPanel(
      id="main_panel",
      selected="graph_tab",
      tabPanel("Таблица", value="table_tab",
               p(),
               mainPanel(
                 fluidRow(
                   column(6, div(withSpinner(DT::dataTableOutput("url_volume_table"))), 
                          style="font-size: 90%"),
                   column(6, div(withSpinner(DT::dataTableOutput("url_host_volume_table"))), 
                          style="font-size: 90%")
                 ), width=10), 
      # ----------------
               sidebarPanel(
                 selectInput("depth_filter", "Глубина данных",
                             choices=c('Last 5 min'=5,
                                       'Last 30 min'=30, 
                                       'Last 24 hr'=24*60)
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
      column(2, actionButton("set_test_dates_btn", "На демо дату", class='rightAlign'))
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
  # статические переменные ------------------------------------------------
  log_name <- "app.log"
  
  flog.appender(appender.tee(log_name))
  flog.threshold(TRACE)
  flog.info(glue("App started in <{Sys.getenv('R_CONFIG_ACTIVE')}> environment"))

  # реактивные переменные -------------------
  raw_df <- reactive({
    input$process_btn # обновлять будем вручную
    # загрузим лог squid -------
    # loadSquidLog("./data/acc.log")
    data <- httr::content(httr::GET("http://10.0.0.246/access.log"))    
    loadSquidLog(data)
  })  

  squid_df <- reactive({
    req(raw_df()) %>%
      filter(timestamp>now()-days(2))
  })

  url_df <- reactive({
    df <- req(squid_df()) %>%
      filter(timestamp>now()-minutes(as.numeric(input$depth_filter))) %>%
      select(host, bytes, url) %>%
      group_by(url) %>%
      summarise(volume=round(sum(bytes)/1024/1024, 1)) %>% # Перевели в Мб
      arrange(desc(volume))
    df
  })

  url_host_df <- reactive(({
    # посчитаем сводку по отдельным HOST
    req(url_df())
    ids <- req(input$url_volume_table_rows_selected) # проводим анализ при выборе строки в таблице
    flog.info(glue("Selected row num is {ids}. Data row: {capture.output(str(url_df()[ids, ]))}"))
    
    url <- url_df()[[ids, "url"]]
    url_val <- enquo(url) # превратили в строку
    df <- squid_df() %>%
      filter(url==!!url_val) %>%
      select(bytes, host) %>%
      group_by(host) %>%
      summarise(volume=round(sum(bytes)/1024/1024, 1)) %>% # Перевели в Мб
      arrange(desc(volume))
    df
  }))  

  msg <- reactiveVal("")

  # таблица с выборкой по каналам ----------------------------
  output$url_volume_table <- DT::renderDataTable({
    df <- req(url_df())

    # https://rstudio.github.io/DT/functions.html
    DT::datatable(df,
                  class='cell-border stripe',
                  rownames=FALSE,
                  filter='bottom',
                  selection=list(mode="single", target="row"),
                  options=list(dom='fltip', 
                               pageLength=7, 
                               lengthMenu=c(5, 7, 10, 15, 50),
                               order=list(list(1, 'desc')))) # нумерация с 0
    })

  # таблица-детализация по ОКС в разрезе ----------------------------
  output$url_host_volume_table <- DT::renderDataTable({
    
    # https://rstudio.github.io/DT/functions.html
    DT::datatable(req(url_host_df()),
                  class='cell-border stripe',
                  rownames=FALSE,
                  filter='bottom',
                  options=list(dom='fltip', #autoWidth=TRUE, 
                               pageLength=7, lengthMenu=c(5, 7, 10, 15)))
  })
  
  # график Топ10  -------------
  output$top10_left_plot <- renderPlot({
    df <- req(squid_df()) %>%
      filter(timestamp>now()-days(1))
    plotTopHostDownload(df, subtitle="за последние сутки")
  })

  output$top10_center_plot <- renderPlot({
    df <- req(squid_df()) %>%
      filter(timestamp>now()-minutes(30))
    plotTopHostDownload(df, subtitle="за последние 30 минут")
  })

  output$top10_right_plot <- renderPlot({
    df <- req(squid_df()) %>%
      filter(timestamp>now()-minutes(20))
    plotTopHostDownload(df, subtitle="за последние 5 минут")
  })
  
  # служебный вывод ---------------------  
  output$info_text <- renderText({
    msg()
  })

}

shinyApp(ui=ui, server=server)
