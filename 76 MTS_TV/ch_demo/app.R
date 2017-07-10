# приложение для проверки цепочки
library(tidyverse)
library(readxl)
library(magrittr)
library(stringi)
library(hrbrthemes)
library(Cairo)
library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyBS)
library(shinyjs)
library(config)
library(DBI)
# library(RPostgreSQL)
library(RODBC)
# library(doParallel)
library(anytime)
library(fasttime)
library(futile.logger)

# eval(parse("funcs.R", encoding="UTF-8"))
eval(parse("clickhouse.R", encoding="UTF-8"))

ui <- fluidPage(
  useShinyjs(), 
  
  titlePanel("Тест цепочки"),
  # Some custom CSS for a smaller font for preformatted text
  tags$head(tags$style(HTML("pre, table.table {font-size: smaller;}"))),
  theme = shinytheme("united"),
  #("slate"),
  
  sidebarLayout(
    sidebarPanel(
      width = 1,
      # обязательно ширины надо взаимно балансировать!!!!
      radioButtons(
        "CH_IP",
        "CH IP",
        choices = c("M-T"="10.0.0.180", "CTI"="172.16.33.74"),
        selected = "10.0.0.180"
      ),

      radioButtons(
        "CH_driver",
        "Драйвер",
        choices = c("ODBC", "HTTP"),
        selected = "ODBC"
      ),
      actionButton("conn_btn", "Подключить"),
      p(""),
      h4(textOutput("info_text", inline = TRUE))
    ),
    
    mainPanel(width = 11, # обязательно ширины надо взаимно балансировать!!!!
              tabsetPanel(
                id = "panel_id",
                selected="states_tab",
                tabPanel("Events", value = "events_tab",
                         fluidRow(
                           p()
                           # column(12, div(DT::dataTableOutput('events_table')), style="font-size: 90%")
                         )),
                tabPanel("States", value = "states_tab",
                         fluidRow(
                           p(),
                           column(8, div(DT::dataTableOutput('states_table')), style="font-size: 90%"),
                           column(4, plotOutput('event_plot'))
                         ))
                
              ),
              fluidRow(
                p(),
                column(8, wellPanel("Лог файл", verbatimTextOutput("log_info"))),
                tags$style(type='text/css', '#log_info {font-size: 80%;}')
              )
    )
    )
  )


server <- function(input, output, session) {

  # статические переменные ------------------------------------------------
  log_name <- "app.log"

  flog.appender(appender.file(log_name))
  flog.threshold(TRACE)
  flog.info("Start App")

  # if (Sys.info()["sysname"] == "Windows") {
  #   # con <- dbConnect(RODBCDBI::ODBC(), dsn='CH_ANSI', believeNRows=FALSE, rows_at_time=1)
  #   con <- dbConnect(clickhouse(), host="172.16.33.74", port=8123L, user="default", password="")
  # }else{
  #   con <- dbConnect(clickhouse(), host="172.16.33.74", port=8123L, user="default", password="")
  # }

  # реактивные переменные ------------------------------------------------
  values <- reactiveValues(info_str="...", req_count=0)

  # db_conn <- eventReactive(input$conn_btn, {
  #   dbConnect(ifelse(input$CH_driver == "ODBC", RODBCDBI::ODBC(), clickhouse()), 
  #             host=ip$CH_IP, port=8123L, user="default", password="")
  # })
  
  # poll переменные ------------------------------------------------
  # если мы хотим еще запускать по кнопке, то придется эмулировать функцию самим
  
  day_events_df <- reactive({
    if (input$conn_btn){
      # нажата кнопка подключения, запускаем процесс
      invalidateLater(5000, session)
      # browser()
      # апдейт делаем в изолированном режиме
      con <- isolate(if(input$CH_driver == "ODBC"){
        dbConnect(RODBCDBI::ODBC(), dsn='CH_ANSI', believeNRows=FALSE, rows_at_time=1)
      }else{
        dbConnect(clickhouse(), host=ip$CH_IP, port=8123L, user="default", password="")
      })

      
      rs <- dbSendQuery(con, "SELECT * FROM states WHERE toDate(begin) >= yesterday() AND begin < now()")
      df <- dbFetch(rs)
      
      if (is.character(df$begin)){
        df %<>% mutate_at(vars(begin, end), anytime, tz="Europe/Moscow", asUTC=FALSE)
      }
      
      flog.info(paste0("load_events returned ",  capture.output(print(tail(df, 2)))))
      flog.info(paste0("usefull length = ",  nrow(df), " events"))
      
      df
    }
  })
  
  # обработчики данных --------------------------------
  
  # таблица состояний ------------------------------
  output$states_table <- DT::renderDataTable(
    # https://rstudio.github.io/DT/functions.html
    DT::datatable(req(day_events_df()),
                  rownames=FALSE,
                  options=list(pageLength=7, lengthMenu=c(5, 7, 10, 15))) %>%
      DT::formatDate("begin", method = "toLocaleString") %>%
      DT::formatDate("end", method = "toLocaleString")
  )

  # таблица событий ------------------------------  
  output$events_table <- DT::renderDataTable(
    DT::datatable(NULL,
                  rownames=FALSE,
                  filter = 'bottom',
                  options=list(pageLength=7, lengthMenu=c(5, 7, 10, 15)))
  )
  
  # информация для справки --------------------------
  output$info_text <- renderText({
    values$info_str
  })
  
  # гистограмма событий --------------------------
  output$event_plot <- renderPlot({
    gp <- ggplot(req(day_events_df()), aes(x=duration)) +
      # theme_bw() +
      theme_ipsum_rc(base_size=14, axis_title_size=12) +
      geom_histogram(binwidth=2)
    
    gp
  })  
  
  # Log file визуализация --------------------------------------------------------
  # This part of the code monitors the file for changes once per
  # 0.5 second (500 milliseconds).
  logReader <- reactiveFileReader(500, session, log_name, readLines)
  
  output$log_info <- renderText({
    # Read the text, and make it a consistent number of lines so
    # that the output box doesn't grow in height.
    text <- logReader() %>% tail(10)
    text[is.na(text)] <- ""
    paste(text, collapse = '\n')
  })  

}

# Run the application 
shinyApp(ui=ui, server=server)

