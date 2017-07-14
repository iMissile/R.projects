# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# Обязательно в кодировке UTF-8
rm(list=ls()) # очистим все переменные

library(tidyverse)
library(readxl)
library(magrittr)
library(stringi)
library(Cairo)
library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyBS)
library(shinyjs)
library(config)
library(DBI)
library(RPostgreSQL)
library(futile.logger)
## ---------
# library(RColorBrewer)
# library(gtable)
# library(grid) # для grid.newpage()
# library(gridExtra) # для grid.arrange()
# library(curl)
# library(httr)
# library(jsonlite)
# library(arules)
# library(htmltools)

options(shiny.usecairo=TRUE)

eval(parse("funcs.R", encoding="UTF-8"))

# ================================================================
ui <- 
  navbarPage("DVT IoT",
  title=HTML('<div><a href="http://devoteam.com/"><img src="./img/devoteam_176px.png" width="80%"></a></div>'),
  # windowTitle="CC4L",
  collapsible=TRUE,
  id="tsp",
  theme=shinytheme("flatly"),
  # shinythemes::themeSelector(),
  
  # includeCSS("styles.css"),
  
  tabPanel("Поле", value="field"),
  tabPanel("About", value="about"),
  
  
  # titlePanel("Контроль орошения полей"),
  # ----------------
  conditionalPanel(
    "input.tsp=='field'",
    fluidRow(
      tags$style(type='text/css', '#cweather_text {white-space:pre;}'), 
      column(6, h2("Контроль орошения полей"), h3(textOutput("cweather_text", inline=TRUE))),
      column(6,
             fluidRow(
               column(4, selectInput("history_days", "Глубина истории (дни)", 
                                     choices = c(0, 1, 3, 5, 7), selected = 5)),
               column(4, selectInput("predict_days", "Горизонт прогноза (дни)",
                                     choices = c(1, 2, 3, 5), selected = 2)),
               column(4, selectInput("time_bin", "Группировка (часы)",
                                     choices = c(0.5, 1, 2, 3, 4, 6, 12), selected = 1))
               )
             )
      ),
    
    fluidRow(
        column(6, plotOutput('temp_plot', height = "600px")), # 
        column(6, plotOutput('weather_plot', height = "600px")) # , height = "300px"
      ),
    
    fluidRow(
      column(2,checkboxInput(inputId = "sync_graphs",
                             label = strong("Прогноз влажности почвы"),
                             value = TRUE)),
      column(2, checkboxInput(inputId = "expand_y",
                              label = strong("Расширить ось Y"),
                              value = FALSE))
    )    
  )
)

# ================================================================
server <- function(input, output, session) {
  
  cweather_df <- reactive({
  })  


  field_df <- reactive({

  })  
  
  raw_weather_df <- reactive({
  })  
  
  rain_df <- reactive({
  })  

  weather_df <- reactive({
  })  
  
  output$cweather_plot <- renderPlot({
  })

  output$cweather_text <- renderText({
  })

  output$temp_plot <- renderPlot({
  })

  output$weather_plot <- renderPlot({
  })
  
  output$data_tbl <- DT::renderDataTable({
    })

  output$map_plot <- renderPlot({
  })

}

shinyApp(ui = ui, server = server)
