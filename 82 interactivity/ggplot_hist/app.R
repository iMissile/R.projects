#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(lubridate)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        verbatimTextOutput("message_log")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  demo_df <- tibble(category=ymd("2017-10-12", "2017-10-11", "2017-10-10", "2017-10-9"),
                    value=c(4.2, 7.7, 3.9, 6.2),
                    type=c("BAD", "OK", "OK", "BAD"))
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     gp <- ggplot(demo_df, aes(x=category, y=value)) +
       geom_bar(aes(fill=type), stat="identity") +
       scale_fill_brewer(palette="Dark2")
     
     gp
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

