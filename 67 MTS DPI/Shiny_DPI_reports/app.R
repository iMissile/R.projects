library(tidyverse)
library(lubridate)   # date manipulation
library(padr)
library(forcats)
library(magrittr)
library(countrycode) # turn country codes into pretty names
library(scales)      # pairs nicely with ggplot2 for plot label formatting
# library(gridExtra)   # a helper for arranging individual ggplot objects
library(ggthemes)    # has a clean theme for ggplot2
library(viridis)     # best. color. palette. evar.
library(RColorBrewer)# best. color. palette. evar.
library(hrbrthemes)
library(extrafont)   # http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
library(stringi)
library(stringr)
# library(forcats)
#library(sna)
#library(igraph)
#library(intergraph) # http://mbojan.github.io/intergraph/
# library(ggpmisc)
#library(ggnetwork)
library(Cairo)
library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyBS)
library(shinyjs)
library(futile.logger)
library(RcppRoll)

# В DataTable поиск работает только для UNICODE!!!!!!

eval(parse("funcs.R", encoding="UTF-8"))

# http://shiny.rstudio.com/gallery/plot-interaction-basic.html
# https://shiny.rstudio.com/articles/debugging.html
# https://blog.rstudio.org/2015/06/16/shiny-0-12-interactive-plots-with-ggplot2/

flog.appender(appender.file('app.log'))
flog.threshold(TRACE)
flog.info("Dashboard started")

# options(shiny.error=browser)
# options(shiny.reactlog=TRUE)
# options(shiny.usecairo=TRUE)

# первичная инициализация --------------------------
regions <- c("Владивосток", "Новосибирск", "Екатеринбург", "Н.Новгород", 
             "Краснодар", "Москва", "Санкт-Петербург")

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),  # Include shinyjs
  titlePanel("Аналитика DPI"),
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
  tags$head(tags$style(".rightAlign{float:right;}")), 
  theme=shinytheme("united"), #("slate"),
  # shinythemes::themeSelector(),
  
  sidebarLayout(
    sidebarPanel(
      width = 2, # обязательно ширины надо взаимно балансировать!!!!
      h3(textOutput("time_info")),
      p(),
      #strong("Цветовая палитра"),
      radioButtons("dynamic_pal", "Цветовая палитра",
                   list(`Набор 1`="Set1", `Набор 2`="Accent", `Набор 3`="Dark2"
                        #`Мин-Макс 1`="YlGn", `Мин-Макс 2`="PuRd", 
                        #`От центра 1`="Spectral", `От центра 2`="PiYG"
                        )),
      # Кнопка запуска расчетов event_heat_map
      # actionButton("ehm_btn", "Карта событий"),
      checkboxInput("wrap_dynamic", "По регионам", value=FALSE),
      verbatimTextOutput("info_text")
    ),
    
    mainPanel(width=10, # обязательно ширины надо взаимно балансировать!!!!
              tabsetPanel(
                id="panel_id",
                selected="dynamics",
                tabPanel("Топ N", value="top_n",
                         fluidRow(
                           column(6, plotOutput("top_downlink_plot")),
                           column(6, plotOutput("top_uplink_plot"))
                           ),
                         fluidRow(
                           column(6, downloadButton("top_downlink_download", class = 'rightAlign')),
                           column(6, downloadButton("top_uplink_download", class = 'rightAlign'))
                           ),
                         fluidRow(
                           column(12, selectInput("site", "Площадки", regions, 
                                                 selected=regions[c(1)], multiple=TRUE, width="100%"))
                           ),
                         fluidRow(
                           column(12, dataTableOutput("top10_table"))
                           )
                         ),
                tabPanel("Динамика", value="dynamics", 
                         fluidRow(
                           column(12, plotOutput("dynamic_plot", height = "600px"))
                           ),
                         fluidRow(
                           column(2, selectInput("dynamic_time_depth", "Диапазон", 
                                                 choices=list(`Сутки`=1, `3 дня`=3, 
                                                              `Неделя`=7, `Месяц`=30),
                                                 selected=3, multiple=FALSE)),
                           column(10, selectInput("site_dynamic", "Площадки", regions, 
                                                  selected=regions[c(1, 3, 5, 7)], multiple=TRUE, width="100%"))
                         )
                ),
                tabPanel("Таблица", value="row_edr", p(), dataTableOutput("edr_table"))
              ))
  )
)

# Define server logic required to draw a network
server <- function(input, output, session) {
  
  # реактивные переменные ------------------------------------------------
  edr_http <- reactive({
    flog.info(paste0("loading edr http ", input$ehm_btn))
    
    # req(read_csv("edr_http_small.csv", progress=interactive())) %>%
    req(readRDS("edr_http_small.rds")) %>%
      select(msisdn, end_timestamp, uplink_bytes, downlink_bytes, site)
    # req(read_csv("edr_http.csv", progress=interactive()) %>% slice(1:20000))
    # write_csv(t %>% sample_frac(0.2), "edr_http_small.csv")
  })
  
  # управляем контекстным отображением элементов --------------------------
  observeEvent(input$panel_id, {
    #browser()
    if(input$panel_id == "dynamics"){
      purrr::walk(c("wrap_dynamic", "dynamic_pal"), shinyjs::show)
      
    }else{
      purrr::walk(c("wrap_dynamic", "dynamic_pal"), shinyjs::hide)
    }
    })
               
  
  top10_df <- reactive({
    edr_http() %>%
      select(timestamp=end_timestamp, down=downlink_bytes, up=uplink_bytes, site, msisdn) %>%
      gather(up, down, key="direction", value="bytes") %>%
      group_by(site, direction, msisdn) %>%
      summarise(user_recs=n(), bytes=sum(bytes)) %>%
      top_n(10, bytes) %>%
      ungroup() %>%
      mutate(msisdn=as.character(msisdn) %>% 
      {sprintf("(%s) %s-%s-%s", stri_sub(., 1, 3), 
               stri_sub(., 4, 5), stri_sub(., 6, 7), stri_sub(., 8, 9))}) %>%
      filter(site %in% input$site)
  })
  
  top10_up_df <- reactive({
    top10_df() %>%
      filter(direction=="up") %>%
      mutate(msisdn=as.factor(msisdn)) %>%
      group_by(msisdn) %>%
      summarise(volume=sum(bytes)) %>%
      top_n(10, volume)
  })

  top10_down_df <- reactive({
    top10_df() %>%
      filter(direction=="down") %>%
      mutate(msisdn=as.factor(msisdn)) %>%
      group_by(msisdn) %>%
      summarise(volume=sum(bytes)) %>%
      top_n(10, volume)
  })

  traffic_df <- reactive({
    edr_http() %>%
      select(timestamp=end_timestamp, down=downlink_bytes, up=uplink_bytes, site, msisdn) %>%
      mutate(timegroup=hgroup.enum(timestamp, time.bin=6)) %>%
      group_by(site, timegroup) %>%
      summarize(up=sum(up), down=sum(down)) %>%
      ungroup() %>%
      gather(up, down, key="direction", value="volume") %>%
      mutate(volume=volume/1024/1024) %>% #пересчитали в Мб
      group_by(site, direction) %>%
      mutate(volume_meanr = RcppRoll::roll_meanr(x=volume, n=7, fill=NA)) %>%
      ungroup()
  })
  
  # Top N графики --------------------------------------------------------
  output$top_downlink_plot <- renderPlot({
    plotTop10Downlink(top10_down_df())
  })
  
  output$top_uplink_plot <- renderPlot({
    plotTop10Uplink(top10_up_df())
  })  
  
  output$edr_table <- renderDataTable({
    edr_http()}, options=list(pageLength=10, lengthMenu=c(5, 10, 15))
    )

  output$top10_table <- renderDataTable({
    top10_df()}, options=list(pageLength=5, lengthMenu=c(5, 7))
  )

  output$dynamic_plot <- renderPlot({
    timeframe <- getTimeframe(input$dynamic_time_depth, 0)
    # browser()

    df <- traffic_df() %>%
      filter(site %in% input$site_dynamic) %>%
      filter(timegroup >= timeframe[1]) %>%
      filter(timegroup <= timeframe[2])
    # browser()
    plotFacetTraffic(df, input$dynamic_pal)
  })  

  # обработчики кнопок выгрузки файлов --------------------------------------------------
  output$top_downlink_download <- downloadHandler(
    filename = function() {
      paste0("downlink-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(top10_down_df(), file)
    }
  )

  output$top_uplink_download <- downloadHandler(
    filename = function() {
      paste0("uplink-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(top10_up_df(), file)
    }
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
