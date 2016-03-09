library(shiny)
library(ggplot2)
library(scales)

source("IoT_funcs.R")

# чтобы работало в shiny необходимо файл сохранять в utf!!!
# буква я

#Get TokenID
tokenID <- getTokenID()


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  v <- reactiveValues(depth = "1d") # глубина временного ряда
  
  observeEvent(input$lastDay, {
    v$depth <- "1d"
  })
  
  observeEvent(input$lastWeek, {
    v$depth <- "1w"
  })
  
  output$text1 <- renderText({ 
    paste0("Глубина данных ", v$depth)
  })
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  
  output$loadPlot <- renderPlot({
    ggplot(data = getTSdata(v$depth), aes(x=timestamp, y=value)) +
      theme_bw() +
      geom_point(size = 2, fill = "white", shape = 21, na.rm = TRUE) +    # White fill
      geom_line(size = 0.5, color = "blue", na.rm = TRUE) +
      scale_x_datetime(labels = date_format_tz("%H", tz="Europe/Moscow"), breaks = date_breaks("1 hours"), minor_breaks = date_breaks("30 mins")) +
      labs(x = "Дата",  #x = iconv("Дата, время", "windows-1251", "UTF8", "byte"),
           y = "Загрузка интерфейса, %")
  })
  
  output$mymap <- renderLeaflet({
    point <- getCurrentCraftPos(tokenID) # глобальный tokenID
    
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng = point[[2]],
                 lat = point[[1]],
                 popup = "Борт")
  })
  
})
