rm(list=ls()) # очистим все переменные

# https://briatte.github.io/ggnetwork

library("ggnetwork")
library(network)
library(sna)
library(ggplot2)
n <- network(rgraph(10, tprob = 0.2), directed = FALSE)

n %v% "family" <- sample(letters[1:3], 10, replace = TRUE)
n %v% "importance" <- sample(1:3, 10, replace = TRUE)

e <- network.edgecount(n)
network::set.edge.attribute(n, "type", sample(letters[24:26], e, replace = TRUE))
network::set.edge.attribute(n, "day", sample(1:3, e, replace = TRUE))


ggplot(ggnetwork(n), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = type), color = "grey75") +
  geom_nodes(color = "gold", size = 8) +
  geom_nodetext(aes(label = LETTERS[ vertex.names ])) +
  geom_edgetext(aes(label = day), color = "white", fill = "grey25") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "grey25"),
        panel.grid = element_blank())