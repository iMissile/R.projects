rm(list=ls()) # очистим все переменные
library(ggplot2)
library(ggnetwork)
library(sna)
library(intergraph) # http://mbojan.github.io/intergraph/
library(igraph)
# library(magrittr)
library(ggnetwork) # https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.htmlpacman::p_load("ggnetwork", "ggnet", "geomnet")

# Example 1 -----------------------
# https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.html#edge-weights

# LetТs define a small random graph to illustrate each component of ggnetwork:
n <- network(rgraph(10, tprob = 0.2), directed = FALSE)
# convert to 'igraph'
g <- asIgraph(n)



# http://igraph.org/r/doc/graph_from_literal.html
# In directed graphs, edges will be created only if the edge operator includes a arrow head (С+Т) 
g <- graph_from_literal(A-+B-+C, D-+A, C+-E-+D, E-+B)
net <- asNetwork(g)
plot(g, layout=layout_on_grid(g))
