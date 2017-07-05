library(tidyverse)
library(readxl)
library(magrittr)
library(stringi)
library(Cairo)
library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyBS)
library(shinyjs)
library(futile.logger)

# /var/lib/rstudio-connect/apps/8
# Accessing RStudio Server Open-Source. http://<server-ip>:8787

options(shiny.maxRequestSize=30*1024^2)
        
# devtools::install_github("tidyverse/readxl") -- используем фичи dev версии
# в частности, чтение без расширения файла
# https://github.com/tidyverse/readxl/issues/85
# https://github.com/leeper/rio/issues/130

eval(parse("funcs.R", encoding="UTF-8"))

ui <- fluidPage(
  useShinyjs(),  # Include shinyjs
  
  titlePanel("Канальный план"),

  theme=shinytheme("united"), #("slate"),
  
  sidebarLayout(
    sidebarPanel(
      width=2, # обязательно ширины надо взаимно балансировать!!!!
      fileInput('pplan', 'Выбор .xlsx файла с планом',
                #accept=c('application/vnd.ms-excel', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')),
                accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')),
      actionButton("go", "Публиковать"),
      p(),
      h3(textOutput("type_info", inline=TRUE))
    ),

    mainPanel(
      width=10, # обязательно ширины надо взаимно балансировать!!!!
      fluidRow(
        h3("Список некорректных записей"),
        column(12, DT::dataTableOutput('wrong_rec_table'))
      )
      
      # fluidRow(
      #   column(12, wellPanel("Лог файл", verbatimTextOutput("log_info"))),
      #   #tags$style(type='text/css', '#log_info {background-color: rgba(255,255,0,0.40); color: green;}')
      #   tags$style(type='text/css', '#log_info {font-size: 80%;}')
      # )
      ))
)


server <- function(input, output, session) {

  log_name <- "app.log"
  
  flog.appender(appender.file(log_name))
  flog.threshold(TRACE)
  flog.info("Dashboard started")
  
  pplan <- reactive({
    fname <- paste(req(input$pplan$datapath), ".xlsx", sep="")
    file.copy(input$pplan$datapath, fname)

    normalizePath(fname)
  })
  
  sheet_names <- reactive({
    tmp <- excel_sheets(pplan())
    sheets <- tmp[!stri_detect_fixed(tmp, c('КП', 'AC'))]    
    
    sheets
  })
  
  ptype <- reactive({
    # определяем тип файла --------------------
    case_when(
      "IPTV" %in% sheet_names()  ~ "iptv",
      "DVB-S" %in% sheet_names() ~ "dvbs",
      length(sheet_names()) > 8  ~ "dvbc",
      TRUE                       ~ "unknown"
    )
  })
  
  raw_plan_df <- reactive({

    # обойдемся без вложенных else, нет смысла усложнять
    if (ptype == "dvbc") {
      # парсим закладки файла
      
      progress <- Progress$new(session, min = 1, max = length(sheet_names()))
      on.exit(progress$close())
      
      # progress$set(message=HTML(paste0("Парсинг закладок (", length(sheets_df()), " шт)", "<br/>")), # format(Sys.time(), "%H:%M:%S")),
      #              detail="...")
      
      progress$set(message = paste0("Парсинг закладок (", length(sheet_names()), " шт) . . . ."),
                   detail = "...")
      
      # вариант 2, взят отсюда: http://readxl.tidyverse.org/articles/articles/readxl-workflows.html
      # так быстрее на 20% и памяти в 3 раза меньше требуется
      # на больших объемах скорость начинает выигрывать в разы, объем памяти также, в 4-6 раз меньше.
      # browser()
      df0 <- sheet_names() %>%
        purrr::map_df(parseSheet, fname = pplan(), progress = progress, .id = NULL) %>%
        mutate(timezone = 0)
    }
    
    # общий постпроцессинг --------------
    df <- df0 %>%  
      mutate(epg_id=stri_trim_left(epg_id, pattern="\\P{Wspace}")) %>% # убрали лидирующие пробелы
      mutate(date=Sys.Date()) %>% # берем после разговора с Кириллом
      mutate(type=ptype()) %>%
      select(date, row_num, epg_id, title, city, everything())    
    
    
  })
  
  clean_plan_df <- reactive({
    # почистим данные от мусора, а потом выясним пересечение с исходным
    raw_plan_df() %>%
      filter(!is.na(epg_id)) %>%
      filter(stri_startswith_fixed(epg_id, 'epg')) %>%
      distinct()
  })

  output$wrong_rec_table <- DT::renderDataTable({
    anti_join(raw_plan_df(), clean_plan_df())}, 
    colnames=c('Город'='city', 'Строка'='row_num', 'Канал'='title', 'EPG ID'='epg_id'),
    rownames = FALSE,
    options=list(pageLength=10, lengthMenu=c(5, 10, 15))
  )

  output$type_info <- renderText({
    ptype()
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

