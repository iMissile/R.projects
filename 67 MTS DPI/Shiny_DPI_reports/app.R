library(tidyverse)
library(lubridate)   # date manipulation
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
library(futile.logger)

# eval(parse("heatmap_func.R", encoding="UTF-8"))

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
  titlePanel("Аналитика DPI"),
  # Some custom CSS for a smaller font for preformatted text
  tags$head(
    tags$style(HTML("
                    pre, table.table {
                    font-size: smaller;
                    }
                    "))
    ),
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
    
    mainPanel(
      width = 10, # обязательно ширины надо взаимно балансировать!!!!
      fluidRow(
        column(width=6, 
               plotOutput("plot1", 
                          click="plot_click",
                          dblclick="plot_dblclick",
                          hover="plot_hover",
                          brush="plot_brush")),
        column(width=3,
               verbatimTextOutput("click_info")),
        column(width=3,
               verbatimTextOutput("data_info"))
      ),
      fluidRow(
        column(width=8, dataTableOutput("edr_table"))
      )
    )
  )  
    )

# Define server logic required to draw a network
server <- function(input, output, session) {
  
  edr_http <- reactive({
    flog.info(paste0("loading edr http ", input$ehm_btn))
    
    req(read_csv("edr_http_small.csv", progress=interactive())) %>%
      select(msisdn, end_timestamp, uplink_bytes, downlink_bytes)
    # req(read_csv("edr_http.csv", progress=interactive()) %>% slice(1:20000))
    # write_csv(t %>% sample_frac(0.2), "edr_http_small.csv")
  })
  
  output$plot1 <- renderPlot({
    if (input$plot_type == "base") {
      plot(mtcars$wt, mtcars$mpg)
    } else if (input$plot_type == "ggplot2") {
      gp <- ggplot(net(), aes(x=x, y=y, xend=xend, yend=yend)) +
        # geom_edges(aes(linetype=type, color=type, lwd=type)) +
        geom_edges(aes(linetype=type), color="grey75", lwd=1.2)+ #  , curvature = 0.1) +
        geom_nodes(color="gold", size=8) +
        # geom_nodelabel(aes(label=vertex.names), fontface="bold") +
        # geom_nodelabel_repel(aes(color=ip_addr, label=vertex.names), fontface = "bold", box.padding=unit(2, "lines")) +
        geom_nodelabel_repel(aes(label=ip_addr), fontface = "bold", box.padding=unit(2, "lines"), 
                             segment.colour="red", segment.size=1) +
        geom_edgetext_repel(aes(label=volume), color="white", fill="grey25",
                            box.padding = unit(1, "lines")) +
        theme_blank() +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              panel.background = element_rect(fill = "grey25"),
              panel.grid = element_blank())
      
      gp
    }
  })
  
  output$edr_table <- renderDataTable({
    edr_http()},
    options=list(pageLength=5, lengthMenu=c(5, 10))
    )
  
    
}


shinyApp(ui, server)
