library(magrittr)
library(githubinstall)
library(DiagrammeR)
library(DiagrammeRsvg)
library(htmlwidgets)
library(trelliscope)

# ===========================================
# взято отсюда: https://gist.github.com/hrbrmstr/b5e8ec60490b825ea889

widgetThumbnail <- function(p, thumbPath, timeout = 500) {
  phantom <- findPhantom()
  thumbPath <- path.expand(thumbPath)
  
  if(phantom == "") {
    message("** phantomjs dependency could not be found - thumbnail cannot be generated (run phantomInstall() for details)")
  } else {
    res <- try({
      ff <- tempfile(fileext = ".html")
      ffjs <- tempfile(fileext = ".js")
      
      # don't want any padding
      p$sizingPolicy$padding <- 0
      # suppressMessages(saveWidget(p, ff, selfcontained = FALSE))
      saveWidget(p, ff, selfcontained = FALSE)
      
      js <- paste0("var page = require('webpage').create();
                   page.open('file://", ff, "', function() {
                   window.setTimeout(function () {
                   page.render('", thumbPath, "');
                   phantom.exit();
                   }, ", timeout, ");
                   });")
      cat(js, file = ffjs)
      system2(phantom, ffjs)
    })
    if(inherits(res, "try-error"))
      message("** could not create htmlwidget thumbnail...")
    
    # system(paste("open ", ffjs))
    # system(paste("open ", dirname(ffjs)))
}
  }

#' @export
phantomInstall <- function() {
  message("Please visit this page to install phantomjs on your system: http://phantomjs.org/download.html")
}

# similar to webshot
findPhantom <- function() {
  
  phantom <- Sys.which("phantomjs")
  
  if(Sys.which("phantomjs") == "") {
    if(identical(.Platform$OS.type, "windows")) {
      phantom <- Sys.which(file.path(Sys.getenv("APPDATA"), "npm", "phantomjs.cmd"))
    }
  }
  
  phantom
  
}
# ===========================================

# Create a node data frame (ndf)
nodes <-
  create_nodes(
    nodes = c("a", "b", "c", "d"),
    type = c("A", "A", "Z", "Z"),
    label = TRUE,
    value = c(3.5, 2.6, 9.4, 2.7))

# Create an edge data frame (edf)
edges <-
  create_edges(
    from = c("a", "b", "c"),
    to = c("d", "c", "a"),
    rel = c("A", "Z", "A"))

# Create a graph with the ndf and edf
graph <-
  create_graph(nodes_df = nodes,
               edges_df = edges)

# Create a PDF file for the graph (`graph.pdf`)
# graph %>% export_graph("graph.pdf")

# Create a PNG file for the graph (`mypng`)
graph %>%
  export_graph(
    file_name = "my.png",
    file_type = "PNG")

# ==========================
gp <-
grViz("
      digraph DAG {
      
      # Intialization of graph attributes
      graph [overlap = true]
      
      # Initialization of node attributes
      node [shape = box,
      fontname = Helvetica,
      color = blue,
      type = box,
      fixedsize = true]
      
      # Initialization of edge attributes
      edge [color = green,
      rel = yields]
      
      # Node statements
      1; 2; 3; 4; 8; 9; 10; 11
      
      # Revision to node attributes
      node [shape = circle]
      
      # Node statements
      5; 6; 7
      
      # Edge statements
      1->5; 2->6; 3->9; 4->7; 5->8; 5->10; 7->11
      
      # Revision to edge attributes
      edge [color = red]
      
      # Edge statements
      1->8; 3->6; 3->11; 3->7; 5->9; 6->10
      }
      ")


gp1 = DiagrammeR::grViz("
      digraph rmarkdown {
      node [shape = box ]
      'A' -> 'B'
      }
      ")

# Это взято из hrbrmstr/widgt.R: https://gist.github.com/hrbrmstr/b5e8ec60490b825ea889
# Требует загруженного 'htmlwidgets'
saveWidget(gp, "1.html", selfcontained = FALSE)

# Требует скачивания и установки в PATH phantomjs: детали см. phantomInstall()
# http://stackoverflow.com/questions/30307191/using-diagrammer-in-word-document-generated-using-rmarkdown
widgetThumbnail(gp, paste0(getwd(), "/hoge.jpg"), timeout = 5000) 



# так не работает для digigraph
# export_graph(gp, 
#     file_name = "diagr",
#     file_type = "PNG")



