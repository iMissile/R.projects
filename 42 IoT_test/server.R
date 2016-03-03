library(shiny)
library(ggplot2)
library(scales)

source("IoT_funcs.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  
  output$loadPlot <- renderPlot({
    gp <- ggplot(data = getTSdata("1d"), aes(x=timestamp, y=value)) +
      theme_bw() +
      geom_point(size = 2, fill = "white", shape = 21, na.rm = TRUE) +    # White fill
      geom_line(size = 0.5, color = "blue", na.rm = TRUE) +
      scale_x_datetime(labels = date_format_tz("%H", tz="Europe/Moscow"), breaks = date_breaks("1 hours"), minor_breaks = date_breaks("30 mins")) +
      labs(x="Date", y="Interface load, %")
  })
  
})
