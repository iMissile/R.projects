library(shiny)
library(ggplot2)
getwd()
source("regexSelect.R")
source("regexSelectUI.R")

ui <- fluidPage(
  selectInput('var','Choose Variable',
              choices = names(diamonds)[sapply(diamonds,function(x) inherits(x,c('character','factor')))],
              selected = 'clarity'),
  uiOutput('regexchoose'),
  plotOutput("data")
  )

server <- function(input, output, session) {
  
  observeEvent(input$var,{ 
    output$regexchoose<-shiny::renderUI({
      regexSelectUI(id = "a", label = input$var,
                    choices = unique(diamonds[[input$var]]),
                    # checkbox.inline =TRUE,
                    checkbox.show=TRUE
                    )
    })
  })
  
    curr_cols<-callModule(regexSelect, "a",shiny::reactive(unique(diamonds[[input$var]])))
    
    observeEvent(curr_cols(),{
      cols_now<-curr_cols()
      output$data <- shiny::renderPlot({
        ggplot(diamonds[diamonds[[input$var]]%in%cols_now,],aes_string(x='table',y='carat',colour=input$var))+geom_point()
      })
  })
 
}
  
shinyApp(ui, server)
