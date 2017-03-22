library(tidyverse)
library(lubridate)   # date manipulation
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
library(futile.logger)

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
  theme=shinytheme("united"), #("slate"),
  # shinythemes::themeSelector(),
  
  sidebarLayout(
    sidebarPanel(
      width = 2, # обязательно ширины надо взаимно балансировать!!!!
      radioButtons("plot_type", "Тип графика",
                   c("base", "ggplot2")),
      radioButtons("ehm_pal", "Палитра",
                   c("viridis", "brewer")),
      # Кнопка запуска расчетов event_heat_map
      actionButton ("ehm_btn", "Карта событий")
    ),
    
    mainPanel(width=10, # обязательно ширины надо взаимно балансировать!!!!
              tabsetPanel(
                tabPanel("Plot", 
                         fluidRow(
                           column(5, plotOutput("top_download_plot")),
                           column(5, plotOutput("top_upload_plot"))
                           ),
                         fluidRow(
                           column(12, selectInput("site", "Площадки", regions, 
                                                 selected=regions[c(1,4,6,7)], multiple=TRUE, width="100%"))
                         ),
                         fluidRow(
                           column(12, dataTableOutput("top10_table"))
                         )
                ),
                tabPanel("Summary", verbatimTextOutput("summary")),
                tabPanel("Таблица", p(), dataTableOutput("edr_table"))
              ))
  )
)

# Define server logic required to draw a network
server <- function(input, output, session) {
  
  edr_http <- reactive({
    flog.info(paste0("loading edr http ", input$ehm_btn))
    
    # req(read_csv("edr_http_small.csv", progress=interactive())) %>%
    req(readRDS("edr_http.rds")) %>%
      select(msisdn, end_timestamp, uplink_bytes, downlink_bytes, site)
    # req(read_csv("edr_http.csv", progress=interactive()) %>% slice(1:20000))
    # write_csv(t %>% sample_frac(0.2), "edr_http_small.csv")
  })
  
  top10_df <- reactive({
    df <- edr_http() %>%
      select(timestamp=end_timestamp, down=downlink_bytes, up=uplink_bytes, site, msisdn) %>%
      gather(up, down, key="direction", value="bytes") %>%
      group_by(site, direction, msisdn) %>%
      summarise(user_recs=n(), bytes=sum(bytes)) %>%
      top_n(10, bytes) %>%
      ungroup()      
  })
  
  output$top_download_plot <- renderPlot({
    plotTop10Download(edr_http())
  })
  
  output$top_upload_plot <- renderPlot({
    plotTop10Upload(edr_http())
  })  
  
  output$edr_table <- renderDataTable({
    edr_http()},
    options=list(pageLength=10, lengthMenu=c(5, 10, 15))
    )

  output$top10_table <- renderDataTable({
    top10_df()},
    options=list(pageLength=5, lengthMenu=c(5, 7))
  )
  
    
}


shinyApp(ui, server)
