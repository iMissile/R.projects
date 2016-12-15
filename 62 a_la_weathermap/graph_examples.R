rm(list=ls()) # очистим все переменные
library(ggplot2)
library(ggnetwork)
library(sna)
library(intergraph) # http://mbojan.github.io/intergraph/
library(igraph)
# library(magrittr)
library(ggnetwork) # https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.html

# Example 1 -----------------------
# https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.html#edge-weights

# LetТs define a small random graph to illustrate each component of ggnetwork:
n <- network(rgraph(10, tprob = 0.2), directed = FALSE)
# convert to 'igraph'
g <- asIgraph(n)



# http://igraph.org/r/doc/graph_from_literal.html
# In directed graphs, edges will be created only if the edge operator includes a arrow head (С+Т) 
g <- graph_from_literal(A-+B-+C, D-+A, C+-E-+D, E-+B)
V(g) # Vertex sequence
E(g) # Edge sequence
net <- asNetwork(g)
g <- g %>% add_layout_(on_grid())
# plot(g, layout=layout_on_grid(g))
plot(g)
plot(g, layout=layout_as_star(g))

# We now add a categorical edge attribute called "type", which is set to either "x", "y" or "z", 
# and a continuous vertex attribute called  "day", which is set to either 1, 2 or 3.
e <- network.edgecount(net)
set.edge.attribute(net, "type", sample(letters[24:26], e, replace=TRUE))
set.edge.attribute(net, "day", sample(1:3, e, replace = TRUE))
m <- ggnetwork(g)

n <- ggnetwork(net, layout = "target")
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype=type, color=type, lwd=type)) +
  geom_nodelabel(aes(label=vertex.names),
                 fontface="bold") +
  theme_blank()

