library(dplyr)
# library(forcats)
library(sna)
library(igraph)
library(intergraph) # http://mbojan.github.io/intergraph/
library(scales)
library(ggplot2)
# library(ggpmisc)
library(ggnetwork)
library(shiny)
library(futile.logger)

# http://shiny.rstudio.com/gallery/plot-interaction-basic.html
# https://shiny.rstudio.com/articles/debugging.html
# https://blog.rstudio.org/2015/06/16/shiny-0-12-interactive-plots-with-ggplot2/

flog.appender(appender.file('network.log'))
flog.threshold(TRACE)
flog.info("Network dashboard started")


options(shiny.error=browser)
options(shiny.reactlog=TRUE)
options(shiny.usecairo=TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  tags$head(
    tags$style(HTML("
                    pre, table.table {
                    font-size: smaller;
                    }
                    "))
    ),
  
  fluidRow(
    column(width = 2, wellPanel(
      radioButtons("plot_type", "Тип графика",
                   c("base", "ggplot2")
      )
    )),
    
    column(width = 5,
           # In a plotOutput, passing values for click, dblclick, hover, or brush
           # will enable those interactions.
           plotOutput("plot1", height = 350,
                      click = "plot_click",
                      dblclick = "plot_dblclick",
                      hover = "plot_hover",
                      brush = "plot_brush"
                      ), 
           p(),
           verbatimTextOutput("data_info")
           ),
    column(width = 5,
           # А здесь мы нарисуем отрендеренный вручную график
           imageOutput("image", height = 350,
                       click = "plot_click",
                       dblclick = "plot_dblclick",
                       hover = "plot_hover",
                       brush = "plot_brush"
           )
    )
    
    
    ),

  fluidRow(
    column(width = 3, verbatimTextOutput("click_info")),
    column(width = 3, verbatimTextOutput("dblclick_info")),
    column(width = 3, verbatimTextOutput("hover_info")),
    column(width = 3, verbatimTextOutput("brush_info"))
  )
    )

# Define server logic required to draw a network
server <- function(input, output, session) {
  # определяем сетевой объект на уровне сессии пользователя
  g <- graph_from_literal(A-+B-+C, D-+A, C+-E-+D, E-+B)
  set.seed(123)
  g <- igraph::set_vertex_attr(g, "ip_addr", # "label"
                               value=stringr::str_c("192.168.1.", sample(1:254, vcount(g), replace=FALSE)))
  # прошлись по граням
  val <- stringr::str_c("UP = ", sample(1:10, ecount(g), replace=FALSE))
  g <- igraph::set_edge_attr(g, "volume", value=val)
  g <- igraph::set_edge_attr(g, "type", value=sample(letters[24:26], ecount(g), replace=TRUE))
  lo <- layout_on_grid(g) # lo is a matrix of coordinates
  # !! из анализа github понял, что можно в качестве layout матрицу подсовывать!!
  net <- ggnetwork(g, layout=lo)
  
  output$plot1 <- renderPlot({
    if (input$plot_type == "base") {
      plot(mtcars$wt, mtcars$mpg)
    } else if (input$plot_type == "ggplot2") {
      gp <- ggplot(net, aes(x=x, y=y, xend=xend, yend=yend)) +
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

  output$image <- renderImage({
      gp <- ggplot(net, aes(x=x, y=y, xend=xend, yend=yend)) +
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
      
      flog.info("ggplot for Image built")
      
      # сохраним в файл, разбираемся с антиалайзингом
      # http://blog.revolutionanalytics.com/2009/01/10-tips-for-making-your-r-graphics-look-their-best.html
      # http://gforge.se/2013/02/exporting-nice-plots-in-r/ (см. отдельно UPDATE и "lines and text anti-aliased - not fills/polygons")
      outfile <- "render_w_cairo.png"
      # outfile <- tempfile(fileext='.png')
      flog.info(paste0("rendered file:", outfile))
      
      # Read plot2's width and height. These are reactive values, so this
      # expression will re-run whenever these values change.
      width  <- session$clientData$output_image_width
      height <- session$clientData$output_image_height
      flog.info(paste0("render size (H x W): ", height, " x ", width))
      
      
      png(filename=outfile, type="cairo", #pointsize=24, 
          units="cm", height=10, width=20, res=150, pointsize=8, antialias="default")
      print(gp)
      dev.off()
      flog.info("Image rendered")
      
      
      # Return a list containing information about the image
      list(
        src = outfile,
        #contentType = "image/png",
        #width = width,
        #height = height,
        alt = "This is alternate text"
      )      
      
  }, deleteFile=FALSE)
  
  
    
  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    str(input$plot_click)
  })
  output$hover_info <- renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
  output$dblclick_info <- renderPrint({
    cat("input$plot_dblclick:\n")
    str(input$plot_dblclick)
  })
  output$brush_info <- renderPrint({
    # cat("input$plot_brush:\n")
    # str(input$plot_brush)
    # Get width and height of image output
    #w  <- session$clientData#$output_image_render_width
    #h <- session$clientData#$output_image_render_height
    #str(w, h)      
  })

  output$data_info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
    nearPoints(net, input$plot_click, xvar="x", yvar="y")
  })
  
}


shinyApp(ui, server)


# Run the application 
# shinyApp(ui=ui, server=server, display.mode="showcase") # так не рабоает, потому что не runApp
# см http://stackoverflow.com/questions/26291523/showcase-display-mode-for-published-app-in-shinyapps-io
# mode showcase включил через файл DESCRIPTION

