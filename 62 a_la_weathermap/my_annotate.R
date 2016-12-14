library(igraph)
library(magrittr)
library(ggnetwork) # https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.htmlpacman::p_load("ggnetwork", "ggnet", "geomnet")

# http://igraph.org/r/doc/graph_from_literal.html
# In directed graphs, edges will be created only if the edge operator includes a arrow head (С+Т) 
g <- graph_from_literal(A-+B-+C, D-+A, C+-E-+D, E-+B)
V(g) # Vertex sequence
E(g) # Edge sequence


g %>% add_layout_(as_tree()) %>% plot()


plot(g, layout=layout_as_tree(g, root=C))
plot(g, layout=layout_on_grid(g))
# g <- add_layout_(g, as_star())
# print(g)
# plot(g, layout=layout_as_star(g))
stop()

# √енераци€ данных по сети вз€та отсюда:
# http://www.mjdenny.com/Preparing_Network_Data_In_R.html

# https://blog.rstudio.org/2016/08/29/tibble-1-2-0/
tribble(
  ~x, ~y,
   1, "a",
   2, "z"
)