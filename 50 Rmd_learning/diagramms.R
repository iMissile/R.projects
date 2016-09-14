library(DiagrammeR)

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

export_graph(gp, 
    file_name = "diagr.png",
    file_type = "png")