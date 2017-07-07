library(tidyverse)
library(readxl)
library(magrittr)
library(stringi)
library(Cairo)
library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyBS)
library(shinyjs)
library(iterators)
library(foreach)
library(config)
library(DBI)
library(RPostgreSQL)
# library(doParallel)
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
  useShinyjs(), 
  
  titlePanel("Канальный план"),
  # Some custom CSS for a smaller font for preformatted text
  tags$head(tags$style(HTML("pre, table.table {font-size: smaller;}"))),
  theme = shinytheme("united"),
  #("slate"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      # обязательно ширины надо взаимно балансировать!!!!
      fileInput(
        'pplan',
        'Выбор .xlsx файла с планом',
        #accept=c('application/vnd.ms-excel', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')),
        accept = c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')
      ),
      actionButton("publish_btn", "Публиковать"),
      p(),
      h4(textOutput("type_text", inline = TRUE)),
      p(),
      h4(textOutput("info_text", inline = TRUE))
    ),
    
    mainPanel(width = 10, # обязательно ширины надо взаимно балансировать!!!!
              tabsetPanel(
                id = "panel_id",
                #selected="dynamics",
                tabPanel("Ошибки", value = "wrong_tab",
                         fluidRow(
                           # h3("Список некорректных записей"),
                           p(),
                           column(12, div(DT::dataTableOutput('wrong_rec_table')), style="font-size: 90%")
                         )),
                tabPanel("Корректный план", value = "right_tab",
                         fluidRow(
                           # h3("Список выверенных записей"),
                           p(),
                           column(12, div(DT::dataTableOutput('clean_rec_table')), style="font-size: 90%")
                         ))
                
              ))
    )
  )


server <- function(input, output, session) {

  log_name <- "app.log"
  
  
  flog.appender(appender.file(log_name))
  flog.threshold(TRACE)
  flog.info("Dashboard started")
  shinyjs::disable("publish_btn")
  
  # обработчики данных --------------------------------
  values <- reactiveValues(info_str = "zero")
  
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
    # загрузка dvbc --------------------
    if (ptype() == "dvbc") {
      # парсим закладки файла
      
      progress <- Progress$new(session, min = 1, max = length(sheet_names()))
      on.exit(progress$close())
      
      # progress$set(message=HTML(paste0("Парсинг закладок (", length(sheets_df()), " шт)", "<br/>")), # format(Sys.time(), "%H:%M:%S")),
      #              detail="...")
      
      progress$set(message = paste0("Парсинг закладок (", length(sheet_names()), " шт) . . . ."),
                   detail = "...")
      
      # browser()
      if(FALSE){
        # вариант 2, взят отсюда: http://readxl.tidyverse.org/articles/articles/readxl-workflows.html
        # так быстрее на 20% и памяти в 3 раза меньше требуется
        # на больших объемах скорость начинает выигрывать в разы, объем памяти также, в 4-6 раз меньше.
        df0 <- sheet_names() %>%
          purrr::map_df(parseSheet, fname=pplan(), progress=progress, .id=NULL)
      }else{
        # так хоть и чуть медленнее, но можно распараллелить
        df0 <- foreach(sheet_name=iter(sheet_names()), 
                .packages=c('tidyverse', 'readxl', 'magrittr', 'stringi', 'futile.logger'), 
                .combine=rbind) %do% {
                  parseSheet(sheet_name, fname=pplan(), progress=progress)
                }        
      }
      
      df0 %<>% mutate(timezone = 0)
    }
    # загрузка dvbs --------------------
    if (ptype() == "dvbs") {
      df0 <- read_excel(pplan(), sheet="DVB-S", skip=1) # пропускаем шапку
      repl_df <- tribble(
        ~pattern, ~replacement,
        "Наименование канала", "title",
        "Час Зона", "timezone",
        "EPG ID", "epg_id",
        "LCN", "lcn",
        "#", "row_num"
      )
      names(df0) <- stri_replace_all_fixed(names(df0),
                                           pattern = repl_df$pattern,
                                           replacement = repl_df$replacement,
                                           vectorize_all = FALSE)
      df0 %<>%
        select(row_num, title, epg_id, timezone, lcn) %>%
        mutate(city='') %>%
        mutate(timezone=as.numeric(stri_extract_first_regex(timezone, pattern="\\d+"))) %>%
        replace_na(list(timezone=0))
    }
    # загрузка iptv --------------------
    if (ptype() == "iptv") {
      df0 <- read_excel(pplan(), sheet="IPTV", skip=1) # пропускаем шапку
      
      repl_df <- tribble(
        ~pattern, ~replacement,
        "Название канала", "title",
        "EPG ID", "epg_id",
        "Позиция (LCN)", "lcn",
        "#", "row_num"
      )
      names(df0) <- stri_replace_all_fixed(names(df0),
                                           pattern = repl_df$pattern,
                                           replacement = repl_df$replacement,
                                           vectorize_all = FALSE)
      df0 %<>%
        select(row_num, title, epg_id, lcn) %>%
        mutate(city='') %>%
        mutate(timezone=0)
    }    
    
    # общий постпроцессинг --------------
    df <- df0 %>%  
      # если мы заботимся о чистоте мастер данных, то не надо отбрасывать лидирующие пробелы. это ошибка
      # mutate(epg_id=stri_trim_left(epg_id, pattern="\\P{Wspace}")) %>% # убрали лидирующие пробелы
      filter(title!="Резерв") %>%
      mutate(row_num=as.numeric(row_num)) %>%
      mutate(timestamp=Sys.time()) %>% # берем после разговора с Кириллом
      mutate(type=ptype()) %>%
      filter(!is.na(lcn)) %>% # отсекаем пояснения и легенду внизу таблицы   
      select(-lcn) %>%
      select(timestamp, row_num, epg_id, title, city, everything()) %>%
      # определяем полезное подмножество ---------
      # для улучшения диагностики сначала отмаркируем колонку с ошибками
      mutate(error=case_when(
        is.na(epg_id) ~ "Отсутствует EPG ID",
        !stri_startswith_fixed(epg_id, 'epg') ~ "EPG id начинается не с 'epg'"
      ))
  })
  
  clean_plan_df <- reactive({
    raw_plan_df() %>%
      filter(is.na(error)) %>%
      select(-error) %>%
      # filter(row_num > 5) %>% # для тестов
      distinct()
  })

  observe({
    shinyjs::enable("publish_btn", clean_plan_df())
  })  
  
  # таблица ошибок ------------------------------
  output$wrong_rec_table <- DT::renderDataTable(
    # https://rstudio.github.io/DT/functions.html
    DT::datatable(filter(raw_plan_df(), !is.na(error)),
                  colnames=c('Город'='city', 'Строка'='row_num', 'Канал'='title', 'Час. зона'='timezone',
                             'EPG ID'='epg_id', 'Дата'='timestamp', 'Тип'='type', 'Ошибка'='error'),
                  rownames=FALSE,
                  options=list(pageLength=5, lengthMenu=c(5, 7, 10, 15))) %>% 
      DT::formatDate("Дата", method = "toLocaleString")
  )

  # таблица правильных записей ------------------------------  
  output$clean_rec_table <- DT::renderDataTable(
    DT::datatable(clean_plan_df(),
                  colnames=c('Город'='city', 'Строка'='row_num', 'Канал'='title', 'Час. зона'='timezone',
                             'EPG ID'='epg_id', 'Дата'='timestamp', 'Тип'='type'),
                  rownames=FALSE,
                  filter = 'bottom',
                  options=list(pageLength=7, lengthMenu=c(5, 7, 10, 15))) %>%
      DT::formatDate("Дата", method = "toLocaleString")
  )
  
  output$type_text <- renderText({
    ptype()
  })
  
  output$info_text <- renderText({
    values$info_str
  })
  
  # публикация в PostgreSQL --------------------
  observeEvent(input$publish_btn, {
    res <- purrr::safely(publishToSQL)(clean_plan_df())
    values$info_str <- if_else(is.null(res$error), "Опубликовано", "Ошибка БД")
    }
    )
  
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

