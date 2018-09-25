# https://shiny.rstudio.com/articles/plot-interaction-advanced.html
# https://stackoverflow.com/questions/41654801/r-shiny-plot-click-with-geom-bar-and-facets

library(tidyverse)
library(lubridate)
library(shiny)
library(ggiraph)
library(hrbrthemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         verbatimTextOutput("info1"),
         verbatimTextOutput("info2")
      ),
      
      mainPanel(
         fluidRow(
            # в ggigraphOutput height не работает, надо задавать размер svg в ggiraph
            column(6, ggiraphOutput("interactivePlot", height="500px")),
            column(6, plotOutput("staticPlot", click="plot_click"))
         )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   tmp_df <- tibble(category=ymd("2017-10-12", "2017-10-11", "2017-10-9", "2017-10-8"),
                    value=c(4.2, 7.7, 3.9, 6.2),
                    type=c("ПЛОХО", "ХОРОШО", "ХОРОШО", "ПЛОХО")) %>%
      mutate(layer="L_1")
   
   demo_df <- bind_rows(tmp_df, tmp_df %>% mutate(value=round(value+runif(n(), -1, 1), 1), layer="L_2")) %>%
      mutate(id=as.character(row_number()), tooltip=as.character(value))
   
   output$staticPlot <- renderPlot({
      gp <- ggplot(demo_df, aes(x=category, y=value)) +
         geom_bar(aes(fill=type, alpha=layer), stat="identity", position="stack") +
         scale_fill_brewer(palette="Dark2") +
         scale_alpha_manual(values=c("L_1"=0.5, "L_2"=1)) +
         facet_wrap(~type)
      
      gp
   })
   
   output$interactivePlot <- renderggiraph({
      gp <- ggplot(demo_df, aes(x=category, y=value, tooltip=value, data_id=id)) +
         geom_bar_interactive(aes(fill=type, alpha=layer), stat="identity", position="stack") +
         scale_fill_brewer(palette="Dark2") +
         scale_alpha_manual(values=c("L_1"=0.5, "L_2"=1)) +
         facet_wrap(~type) +
         # добавим проверку поддержки русских букв
         xlab("Категория") +
         ylab("Значение") +
         ggtitle("Гистограммы по фасетам", subtitle = "Подпись второго уровня") +
         theme_ipsum_rc()
      
      # по умолчания ggigraph делает svg viewbox размером 6in x 6in!!!
      ggiraph(code=print(gp), height_svg=300/72)
      # ggiraph(code=print(gp))
   })
   
   output$info1 <- renderText({
      # browser()
      t <- input$plot_click
      paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y, "\nfacet=", input$plot_click$panelvar1)
   })
   
   output$info2 <- renderText({
      # browser()
      t <- input$plot_click
      paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y, "\nfacet=", input$plot_click$panelvar1)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
