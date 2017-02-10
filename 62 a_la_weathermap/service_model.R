# генерируем граф ресурсно-сервисной модели
# базируется на minimal_sample.R

library(tidyverse)
library(sna)
library(scales)
library(igraph)
library(intergraph) # http://mbojan.github.io/intergraph/
library(ggnetwork)  # https://briatte.github.io/ggnetwork

# создаем вручную модель средствами igraph
g <- graph_from_literal(Service--CPU_1, Service--CPU_2, Service--Memory, Service--net, Service--GW, Service--web_server)
set.seed(123)


# plot(g, layout=layout_on_grid(g))
plot(g, layout=layout_as_tree(g))


net <- intergraph::asNetwork(g) # пересоберем сетку, как это сделали выше
net %v% "family" <- sample(letters[1:3], 10, replace = TRUE)
net %v% "importance" <- sample(1:3, 10, replace = TRUE)

class(net)

n <- ggnetwork(net)

ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50") +
  geom_nodes(aes(x, y, color = family, size = 1.5 * importance)) +
  geom_nodetext(aes(label = vertex.names, size = 0.5 * importance)) +
  #geom_edgetext(aes(label = day), color = "grey25") +
  scale_color_brewer(palette = "Set2") +
  scale_size_area("importance", breaks = 1:3, max_size = 9) +
  theme_blank()
