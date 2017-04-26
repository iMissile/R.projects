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
      p()
    ),
    
    mainPanel(width=10, # обязательно ширины надо взаимно балансировать!!!!
              fluidRow(
                column(2, selectInput("dynamic_time_depth", "Диапазон", 
                                      choices=list(`Сутки`=1, `3 дня`=3, 
                                                   `Неделя`=7, `2 недели`=14, `Месяц`=30),
                                      selected=14, multiple=FALSE)),
                column(10, selectInput("site_dynamic", "Площадки", c(1,2,3,4,5,6,7), 
                                       selected=c(1, 3, 5, 7), multiple=TRUE, width="100%"))
              )
  ))
)

# Define server logic required to draw a network
server <- function(input, output, session) {
  
  # реактивные переменные ------------------------------------------------
  edr_http <- reactive({
    # flog.info(paste0("loading edr http ", input$ehm_btn))
  })
  
  # Top N визуализация --------------------------------------------------------

  output$top10_table <- renderDataTable({
    top10_df()}, options=list(pageLength=5, lengthMenu=c(5, 7))
  )

  # ВременнАя визуализация --------------------------------------------------------

  # Визуализация категорий трафика ------------------------------------------------------
  output$http_category_plot <- renderPlot({
    plotHttpCategory(http_cat_df())
  }) 
  
  
  output$http_category_table <- renderDataTable(
    {http_cat_df()}, options=list(pageLength=5, lengthMenu=c(5, 7))
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
