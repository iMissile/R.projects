library(tidyverse)
library(lubridate)   # date manipulation
library(forcats)
library(magrittr)
library(scales)      # pairs nicely with ggplot2 for plot label formatting
# library(gridExtra)   # a helper for arranging individual ggplot objects
library(ggthemes)    # has a clean theme for ggplot2
library(viridis)     # best. color. palette. evar.
library(RColorBrewer)# best. color. palette. evar.
library(hrbrthemes)
library(extrafont)   # http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
library(stringi)
library(stringr)
library(Cairo)
library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyBS)
library(shinyjs)
library(futile.logger)
library(uuid) # для генерации случайного имени лог файла

# В DataTable поиск работает только для UNICODE!!!!!!

eval(parse("funcs.R", encoding="UTF-8"))

# http://shiny.rstudio.com/gallery/plot-interaction-basic.html
# https://shiny.rstudio.com/articles/debugging.html
# https://blog.rstudio.org/2015/06/16/shiny-0-12-interactive-plots-with-ggplot2/

# options(shiny.error=browser)
options(shiny.reactlog=TRUE)
# options(shiny.usecairo=TRUE)

# первичная инициализация --------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),  # Include shinyjs
  titlePanel("Аналитика <...>"),
  # Some custom CSS for a smaller font for preformatted text
  tags$head(
    tags$style(HTML("
                    pre, table.table {
                    font-size: smaller;
                    }
                    "))
    ),
  #                     font-size: smaller;
  # http://stackoverflow.com/questions/25387844/right-align-elements-in-shiny-mainpanel/25390164
  theme=shinytheme("united"), #("slate"),
  # shinythemes::themeSelector(),
  
  sidebarLayout(
    sidebarPanel(
      width = 2, # обязательно ширины надо взаимно балансировать!!!!
      h3(textOutput("time_info")),
      p(),
      actionButton("load_btn", "Load xDR")
    ),
    
    mainPanel(width=10, # обязательно ширины надо взаимно балансировать!!!!
              fluidRow(
                column(2, selectInput("dynamic_time_depth", "Диапазон", 
                                      choices=list(`Сутки`=1, `3 дня`=3, 
                                                   `Неделя`=7, `2 недели`=14, `Месяц`=30),
                                      selected=14, multiple=FALSE)),
                column(10, selectInput("site_dynamic", "Площадки", c(1,2,3,4,5,6,7), 
                                       selected=c(1, 3, 5, 7), multiple=TRUE, width="100%"))
              ),
              fluidRow(
                column(12, dataTableOutput("xdr_table"))
              ),
              fluidRow(
                column(12, wellPanel("Лог файл", verbatimTextOutput("log_info"))),
                #tags$style(type='text/css', '#log_info {background-color: rgba(255,255,0,0.40); color: green;}')
                tags$style(type='text/css', '#log_info {font-size: 80%;}')
              )
              
    ))
    )

# Define server logic
server <- function(input, output, session) {
  
  log_name <- "app.log"
  
  flog.appender(appender.file(log_name))
  flog.threshold(TRACE)
  flog.info("Dashboard started")
  
  # browser()
  
  # реактивные переменные ------------------------------------------------
  xdr_list <- reactive({
    dir(path="./data/", 
        pattern="edr_BASE-edr_http_format_.*", full.names=TRUE)
  })
  
  
  xdr_df <- reactive({
    # flog.info(paste0("loading edr http ", input$ehm_btn))
    # вариант 2, взят отсюда: http://readxl.tidyverse.org/articles/articles/readxl-workflows.html
    # так быстрее на 20% и памяти в 3 раза меньше требуется
    # на больших объемах скорость начинает выигрывать в разы, объем памяти также, в 4-6 раз меньше.
    
    flog.info(paste0("loading xDR... (btn_click=", input$load_btn, ")"))

    xdr_sublist <- xdr_list() # %>% head(1)
    progress <- Progress$new(session, min=1, max=length(xdr_sublist))
    on.exit(progress$close())
    
    progress$set(message=paste0("Загружаем xDR (", length(xdr_sublist), "шт)  ", format(Sys.time(), "%H:%M:%S")),
                 detail="...")

    df <- xdr_sublist %>%
      purrr::map_df(processxDR, progress=progress, .id = NULL)
    
    # очистим имена колонок от кривых символов
    fix_names <- names(df) %>%
      stri_replace_all_fixed(pattern=c("#", "-", " "), replacement=c("", "_", "_"), vectorize_all=FALSE)
    names(df) <- fix_names
    
    # выберем только то, что нам интересно
    df %>%
      select(timestamp=sn_end_time, downlink_bytes=transaction_downlink_bytes, uplink_bytes=transaction_uplink_bytes)
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
  
  # Top N визуализация --------------------------------------------------------
  
  output$top10_table <- renderDataTable({
    top10_df()}, options=list(pageLength=5, lengthMenu=c(5, 7))
  )
  
  # Визуализация категорий трафика ------------------------------------------------------
  output$xdr_table <- renderDataTable(
    {xdr_df()}, options=list(pageLength=5, lengthMenu=c(5, 7))
  ) 
  
  # обработчик часов  --------------------------------------------------
  output$time_info <- renderText({
    invalidateLater(1000 * 1) # обновляем в автономном режиме раз в N минут
    format(Sys.time(), "%H:%M:%S")
  })  
  
  # хелпер справочной панели --------------------------------------------
  output$info_text <- renderText({
    paste0("Глубина отображения динамики = ", input$dynamic_time_depth, " дн.\n",
           "Активная панель = ", input$panel_id)
  })    
  
}



shinyApp(ui, server)
