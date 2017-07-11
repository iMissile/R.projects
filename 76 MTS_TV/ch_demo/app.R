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
# library(RODBC)
# library(doParallel)
library(tictoc)
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
      width = 2,
      # обязательно ширины надо взаимно балансировать!!!!
      p("Справка"),
      h4(textOutput("info_text", inline = TRUE))
    ),
    
    mainPanel(width = 10, # обязательно ширины надо взаимно балансировать!!!!
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
                         )),
                tabPanel("Log as table", value = "logs_tab",
                         fluidRow(
                           p(),
                           column(8, div(DT::dataTableOutput('logs_table')), style="font-size: 90%")
                         ))
              ),
              fluidRow(
                p(),
                column(8, wellPanel("Лог файл", verbatimTextOutput("log_info"))),
                tags$style(type='text/css', '#log_info {font-size: 80%;}')
              ))
  )
)


server <- function(input, output, session) {
  
  # статические переменные ------------------------------------------------
  log_name <- "app.log"
  
  flog.appender(appender.file(log_name))
  flog.threshold(TRACE)
  flog.info("Start App")
  
  # con <- dbConnect(RODBCDBI::ODBC(), dsn='CH_ANSI', believeNRows=FALSE, rows_at_time=1)
  # реквизиты для подключения на удаленном стенде
  con <- dbConnect(clickhouse(), host="172.16.33.74", port=8123L, user="default", password="")
  # реквизиты для подключения на локальном стенде
  # con <- dbConnect(clickhouse(), host="10.0.0.180", port=8123L, user="default", password="")
  
  # реактивные переменные ------------------------------------------------
  values <- reactiveValues(info_str = "...")
  
  
  # poll переменные ------------------------------------------------
  
  check_states <- function(){
    flog.info(paste0("start check_states"))
    rs <- dbSendQuery(con, "SELECT COUNT() FROM states")
    t <- dbFetch(rs)
    ret <- if (is.list(t)) t[[1]] else 0
    # values$info_str <- ret
    flog.info(paste0("check_states returned ", ret))
    
    ret
  }
  
  load_states <- function(){
    flog.info(paste0("start load_states"))
    tic()
    # rs <- dbSendQuery(con, 
                      # "SELECT * FROM states WHERE toDate(begin) >= yesterday() AND begin < now() AND serial='46839447975'")
                      # "SELECT * FROM states WHERE toDate(begin) >= yesterday() AND begin < now()")
    rs <- dbSendQuery(con, 
                      "SELECT * FROM view_states WHERE begin >= toUInt32(yesterday()) AND begin < toUInt32(now())")
    df <- dbFetch(rs)

    msg1 <- capture.output(toc())
    tic()
    
    # Проверяем из клиента
    # SELECT * FROM states WHERE toDate(begin) >= yesterday() AND begin < now() ORDER BY begin DESC limit 10
    #if (is.character(df$begin)){
    if (is.numeric(df$begin)){
      df %<>% mutate_at(vars(begin, end), anytime, tz="Europe/Moscow", asUTC=FALSE)
    }
    
    #browser()
    msg2 <- capture.output(toc())
    
    msg <- paste0("Query: ", msg1, ". POSIX processing: ", msg2)
    flog.info(msg)
    values$info_str <- msg
    flog.info(paste0("load_states returned ",  capture.output(print(tail(df, 2)))))
    
    
    df
  }
  
  day_states_df <- reactivePoll(10000, session, check_states, load_states)
  
  # обработчики данных --------------------------------
  
  
  # таблица состояний ------------------------------
  output$states_table <- DT::renderDataTable(
    # https://rstudio.github.io/DT/functions.html
    DT::datatable(req(day_states_df()),
                  rownames=FALSE,
                  filter = 'bottom',
                  options=list(pageLength=7, lengthMenu=c(5, 7, 10, 15),
                               order=list(list(3, 'desc')))) %>%
      DT::formatDate("begin", method="toLocaleString") %>%
      DT::formatDate("end", method="toLocaleString")
  )
  
  # таблица событий ------------------------------  
  output$events_table <- DT::renderDataTable(
    DT::datatable(NULL,
                  rownames=FALSE,
                  filter = 'bottom',
                  options=list(pageLength=7, lengthMenu=c(5, 7, 10, 15)))
  )

  # таблица лог записей  ------------------------------  
  output$logs_table  <- DT::renderDataTable(
    # https://rstudio.github.io/DT/functions.html
    {df <- as_tibble(req(app_log())) %>% arrange(-row_number()) %>%
      tidyr::extract(value, into=c("severity", "timestamp", "message"), 
                     regex="([^[:blank:]]+).+\\[(.+)\\][:blank:]*(.+)"); 
    DT::datatable(df,
                  rownames=FALSE,
                  options=list(pageLength=7, lengthMenu=c(5, 7, 10, 15)))
    }
  )
  
  # информация для справки --------------------------
  output$info_text <- renderText({
    values$info_str
  })
  
  # гистограмма событий --------------------------
  output$event_plot <- renderPlot({
    gp <- ggplot(req(day_states_df()), aes(x=duration)) +
      # theme_bw() +
      theme_ipsum_rc(base_size=14, axis_title_size=12) +
      geom_histogram(binwidth=2)
    
    gp
  })  
  
  # Log file визуализация --------------------------------------------------------
  # This part of the code monitors the file for changes once per
  # 0.5 second (500 milliseconds).
  app_log <- reactiveFileReader(30000, session, log_name, readLines)
  
  output$log_info <- renderText({
    # Read the text, and make it a consistent number of lines so
    # that the output box doesn't grow in height.
    text <- app_log() %>% tail(10)
    text[is.na(text)] <- ""
    paste(text, collapse = '\n')
  })  
  
}

# Run the application 
shinyApp(ui=ui, server=server)

