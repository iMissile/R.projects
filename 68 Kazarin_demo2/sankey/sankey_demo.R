library(tidyverse)
library(networkD3)
library(igraph)

df <- readr::read_csv("./sankey/catalog.csv", locale=locale("ru", encoding="windows-1251"))

ss <- tail(seq_along(df), -2)
edge_list <- purrr::map_dfr(ss, ~tibble(source=df[[.x-1]], target=df[[.x]]))
# Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
gD <- igraph::simplify(igraph::graph.data.frame(edge_list, directed=FALSE))

# layouts -- http://igraph.org/r/doc/, letter L
# plot params -- http://igraph.org/r/doc/plot.common.html
plot(gD, layout=layout_with_fr(gD), vertex.size=5, vertex.label=NA)

nodes <- data.frame(name=V(gD)$name, stringsAsFactors=FALSE)
links <- as.data.frame(as_edgelist(gD, names=FALSE)) %>%
  purrr::set_names(c("source", "target")) %>%
  # для js необходимо сместить индексы на 0
  # It looks like Source/Target is not zero-indexed. This is required in JavaScript and so your plot may not render.
  modify(~ .-1) %>%
  mutate(value=1)

sankeyNetwork(Links=links, Nodes=nodes,
              Source="source", Target="target",
              Value="value", NodeID="name",
              fontSize=12, nodeWidth=30)
stop()

# Open a new png device to print the figure out to (or use tiff, pdf, etc).
png(filename = "figure.png", width = 3600, height = 2000, units = 'px', res=150)
plot(gD, layout=layout_nicely(gD))
dev.off() #close the png device to save the figure. 

stop()

# соберем все имена нод
nodes_df <- df %>%
  select(-1) %>%
  gather(key="code", value="name") %>%
  select(-code) %>%
  distinct() %>%
  mutate(idx=row_number()) %>%
  select(idx, everything())

links <- df

# http://r4ds.had.co.nz/iteration.html
# x <- select(df, -1)
plain_df <- NULL
for (i in head(seq_along(df), -1)) {
  # body
  print(i)
  
}
  
