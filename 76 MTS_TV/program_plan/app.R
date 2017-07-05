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
      actionButton("go", "Публиковать")
    ),

    mainPanel(
      width=10, # обязательно ширины надо взаимно балансировать!!!!
      h3("Список некорректных записей"),
      dataTableOutput('wrong_rec_table')
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
  
  sheets_df <- reactive({
    tmp <- excel_sheets(pplan())
    sheets <- tmp[!stri_detect_fixed(tmp, c('КП', 'AC'))]    
    
    sheets
  })
  
  raw_plan_df <- reactive({
    # парсим закладки файла
    
    progress <- Progress$new(session, min=1, max=length(sheets_df()))
    on.exit(progress$close())
    
    # progress$set(message=HTML(paste0("Парсинг закладок (", length(sheets_df()), " шт)", "<br/>")), # format(Sys.time(), "%H:%M:%S")), 
    #              detail="...")

    progress$set(message=paste0("Парсинг закладок (", length(sheets_df()), " шт) . . . ."),
                detail="...")
    
    # вариант 2, взят отсюда: http://readxl.tidyverse.org/articles/articles/readxl-workflows.html
    # так быстрее на 20% и памяти в 3 раза меньше требуется
    # на больших объемах скорость начинает выигрывать в разы, объем памяти также, в 4-6 раз меньше.
    # browser()
    df <- sheets_df() %>%
      purrr::map_df(parseSheet, fname=pplan(), progress=progress, .id = NULL) %>%
      mutate(epg_id=stri_trim_left(epg_id, pattern="\\P{Wspace}")) # убрали лидирующие пробелы    
    
  })
  
  clean_plan_df <- reactive({
    # почистим данные от мусора, а потом выясним пересечение с исходным
    raw_plan_df() %>%
      filter(!is.na(epg_id)) %>%
      filter(stri_startswith_fixed(epg_id, 'epg')) %>%
      distinct()
  })

  output$wrong_rec_table <- renderDataTable({
    anti_join(raw_plan_df(), clean_plan_df())}, 
    options=list(pageLength=10, lengthMenu=c(5, 10, 15))
  )
}

# Run the application 
shinyApp(ui=ui, server=server)

