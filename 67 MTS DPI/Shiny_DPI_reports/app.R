library(tidyverse)
library(lubridate)   # date manipulation
library(padr)
library(ggrepel)
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
library(zoo)
library(forecast)
library(Cairo)
library(shiny)
library(plotly)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyBS)
library(shinyjs)
library(futile.logger)
library(RcppRoll)
library(fuzzyjoin)

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
      checkboxInput("show_point_labels", "Вывести значения", value=TRUE) #,
      #verbatimTextOutput("info_text")
    ),
    
    mainPanel(width=10, # обязательно ширины надо взаимно балансировать!!!!
              tabsetPanel(
                id="panel_id",
                #selected="dynamics",
                tabPanel("Динамика", value="dynamics", 
                         fluidRow(
                           # this is an extra div used ONLY to create positioned ancestor for tooltip
                           # we don't change its position
                           column(12, 
                                  div(
                                    style = "position:relative; font-size:smaller;",
                                    # column(12, plotOutput("dynamic_plot", height = "600px"))
                                    plotOutput("dynamic_plot", 
                                               height="600px",
                                               hover=hoverOpts("plot_hover", delay=100, delayType = "debounce")),
                                    uiOutput("hover_info")
                                  )
                           )
                         ),
                         fluidRow(
                           column(2, selectInput("dynamic_time_depth", "Диапазон", 
                                                 choices=list(`Сутки`=1, `3 дня`=3, 
                                                              `Неделя`=7, `2 недели`=14, `Месяц`=30),
                                                 selected=14, multiple=FALSE)),
                           column(10, selectInput("site_dynamic", "Площадки", regions, 
                                                  selected=regions[c(1, 3, 5, 7)], multiple=TRUE, width="100%"))
                           )
                         ),
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
                # tabPanel("Таблица", value="row_edr", p(), dataTableOutput("edr_table"))
                tabPanel("Tog N - Plot.ly", value="plotly_tab", 
                         fluidRow(
                           column(6, plotlyOutput("plotly_plot1")),
                           column(6, plotlyOutput("plotly_plot2"))
                           )
                         ),
                tabPanel("'Соц. сети'", value="http_category", 
                         fluidRow(
                           column(12, plotOutput("http_category_plot"))
                           ),
                         fluidRow(
                           column(12, dataTableOutput("http_category_table"))
                           )
                         ),
                tabPanel("Прогноз", value="forecast_tab", 
                         fluidRow(
                           column(6, plotOutput("up_forecast_plot")),
                           column(6, plotOutput("down_forecast_plot"))
                           ),
                         fluidRow(
                           radioButtons("f_depth", "Глубина прогноза",
                                        c("4 недели" = 28,
                                          "2 недели" = 14,
                                          "1 неделя" = 7), selected=7)
                           )
                         )
                
                
              ))
  )
)

# Define server logic required to draw a network
server <- function(input, output, session) {
  
  # реактивные переменные ------------------------------------------------
  edr_http <- reactive({
    flog.info(paste0("loading edr http ", input$ehm_btn))
    
    req(read_csv("edr_http_small.csv", progress=interactive())) %>%
    # req(readRDS("edr_http_small.rds")) %>%
      select(msisdn, end_timestamp, uplink_bytes, downlink_bytes, site, http_host)
    # req(read_csv("edr_http.csv", progress=interactive()) %>% slice(1:20000))
    # write_csv(t %>% sample_frac(0.2), "edr_http_small.csv")
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
               stri_sub(., 4, 6), stri_sub(., 7, 8), stri_sub(., 9, 10))}) %>%
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
      select(timestamp=end_timestamp, down=downlink_bytes, 
             up=uplink_bytes, site, msisdn) %>%
      mutate(timegroup=hgroup.enum(timestamp, time.bin=6)) %>%
      group_by(site, timegroup) %>%
      summarise(up=sum(up), down=sum(down)) %>%
      ungroup() %>%
      gather(up, down, key="direction", value="volume") %>%
      mutate(volume=volume/1024/1024) %>% #пересчитали в Мб
      group_by(site, direction) %>%
      mutate(volume_meanr=RcppRoll::roll_meanr(x=volume, n=7, fill=NA)) %>%
      ungroup()
  })

  http_cat_df <- reactive({
    repl_df <- tribble(
      ~pattern, ~category,
      "instagramm\\.com", "Instagramm",
      "vk\\.com", "ВКонтакте",
      "xxx", "XXX",
      "facebook.com", "Facebook",
      "twitter.com", "Twitter",
      "windowsupdate\\.com", "Microsoft" 
    )
    
    df <- edr_http() %>%
      select(timestamp=end_timestamp, down=downlink_bytes, up=uplink_bytes, site, http_host) %>%
      group_by(http_host) %>%
      regex_left_join(repl_df, by=c(http_host="pattern")) %>%
      ungroup() %>%
      filter(complete.cases(.)) %>%
      mutate(category=as.factor(category)) %>%
      gather(up, down, key="direction", value="volume") %>%
      group_by(category, direction) %>%
      summarise(volume=sum(volume))
    
    df
  })
  
  # управляем контекстным отображением элементов --------------------------
  observeEvent(input$panel_id, {
    #browser()
    if(input$panel_id == "dynamics"){
      purrr::walk(c("wrap_dynamic", "dynamic_pal", "show_point_labels"), shinyjs::show)
      
    }else{
      purrr::walk(c("wrap_dynamic", "dynamic_pal", "show_point_labels"), shinyjs::hide)
    }
  })

  # Top N визуализация --------------------------------------------------------
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

  # ВременнАя визуализация --------------------------------------------------------
  output$dynamic_plot <- renderPlot({
    timeframe <- getTimeframe(input$dynamic_time_depth, 0)

    df <- traffic_df() %>%
      filter(site %in% input$site_dynamic) %>%
      filter(timegroup >= timeframe[1]) %>%
      filter(timegroup <= timeframe[2]) %>%
      # расчет среднего по всему объему группы для каждой группы
      group_by(timegroup, direction) %>%
      mutate(global_meanr=mean(volume_meanr)) %>%
      ungroup()

    # browser()
    plotFacetTraffic(df, input$dynamic_pal, input$wrap_dynamic, input$show_point_labels)
  })  

  
  # Пример визуализации с применением plot.ly -------------------------------------------
  output$plotly_plot1 <- renderPlotly({
    gp <- plotTop10Downlink(top10_down_df())
    ggplotly(gp)
  })  
  
  output$plotly_plot2 <- renderPlotly({
    gp <- plotTop10Uplink(top10_up_df())
    ggplotly(gp)
  })    

  # Визуализация категорий трафика ------------------------------------------------------
  output$http_category_plot <- renderPlot({
    plotHttpCategory(http_cat_df())
  }) 
  
  
  output$http_category_table <- renderDataTable(
    {http_cat_df()}, options=list(pageLength=5, lengthMenu=c(5, 7))
  ) 
  
  # Визуализация прогноза трафика ------------------------------------------------------
  output$up_forecast_plot <- renderPlot({
    df1 <- edr_http() %>%
      select(timestamp=end_timestamp, down=downlink_bytes, up=uplink_bytes, site, msisdn) %>%
      #filter(timestamp<now()) %>%
      filter(site=="Москва") %>%
      mutate(timegroup=hgroup.enum(timestamp, time.bin=24)) %>%
      # thicken("day", col="time") %>% # не работает
      group_by(timegroup) %>%
      summarize(up=sum(up), down=sum(down)) %>%
      ungroup() %>% 
      filter(complete.cases(.))
    
    sensor <- ts(df1$up, frequency=7)
    # sensor <- ts(c(df1$up, max(df1$up)*.1), frequency=14)

    fit <- auto.arima(sensor)
    # fit <- arima(sensor, c(0,1,0))
    # fcast <- forecast(fit, h=input$f_depth) # h - Number of periods for forecasting
    fcast <- forecast(fit, h=as.numeric(input$f_depth)) # h - Number of periods for forecasting
    
    browser()
    autoplot(fcast) + theme_bw()
    # plot(fcast) # + geom_forecast(h=input$f_depth, level=c(50,80,95)) + theme_bw()
  }) 
  
  output$down_forecast_plot <- renderPlot({
    df1 <- edr_http() %>%
      select(timestamp=end_timestamp, down=downlink_bytes, up=uplink_bytes, site, msisdn) %>%
      #filter(timestamp<=now()) %>%
      filter(site=="Москва") %>%
      mutate(timegroup=hgroup.enum(timestamp, time.bin=24)) %>%
      # thicken("day", col="time") %>% # не работает
      group_by(timegroup) %>%
      summarize(up=sum(up), down=sum(down)) %>%
      ungroup() %>% 
      filter(complete.cases(.))

    sensor <- ts(df1$down, frequency=7)
    #sensor <- ts(c(df1$down, max(df1$down)*.1), frequency=7)
    
    #fit <- ets(sensor)
    fit <- auto.arima(sensor)
    # browser()
    
    fcast <- forecast(fit, h=as.numeric(input$f_depth)) # h - Number of periods for forecasting
    autoplot(fcast) + theme_bw()
    # geom_forecast(h=input$f_depth, level=c(50,80,95)) 
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

  # tooltip хелпер  --------------------------------------------  
  # https://gitlab.com/snippets/16220
  output$hover_info <- renderUI({
    # browser()
    
    hover <- input$plot_hover
    point <- nearPoints(traffic_df(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    # browser()
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Площадка: </b>", point$site, "<br/>",
                    "<b> Время: </b>", point$timegroup, "<br/>",
                    "<b> Объем: </b>", round(point$volume, 1)
                    #"<b> Distance from left: </b>", left_px, "<b>, from top: </b>", top_px
                    )))
    )
  })  
}



shinyApp(ui, server)
