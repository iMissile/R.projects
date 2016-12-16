#rm(list=ls()) # очистим все переменные

# https://briatte.github.io/ggnetwork

# library(tibble)
library(dplyr)
# library(forcats)
library(sna)
library(igraph)
library(intergraph) # http://mbojan.github.io/intergraph/
library(scales)
library(ggplot2)
# library(ggpmisc)
library(ggnetwork)

# переработка функций ------------------------------------------
StatMidEdges <-
  ggplot2::ggproto("StatMidEdges", Stat,
                   compute_layer = function(data, scales, params) {
                     data = subset(data, !(x == xend & y == yend))
                     data$x = (data$x + data$xend) / 2
                     data$y = (data$y + data$yend) / 2
                     unique(subset(data, select = c(-xend, -yend)))
                   }
  )



my_geom_edgetext <- function(mapping = NULL, data = NULL,
                          position = "identity", parse = FALSE, ...,
                          nudge_x = 0, nudge_y = 0,
                          label.padding = unit(0.25, "lines"),
                          label.r = ggplot2::unit(0.15, "lines"),
                          label.size = 0,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    
    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMidEdges,
    geom = ggplot2::GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      ...
    )
  )
}


# -----------------------------------------------------------
# пример в котором из всего массива меток по граням размещается только для последей вершины
# в ggnetwork функции geom_nodelabel и geom_edgetext почти идентичны
# первая размещает все метки, а вторая -- нет
# отличия в stat = StatNodes и stat = StatMidEdges
g <- graph_from_literal(A-+B-+C, D-+A, C+-E-+D, E-+B)
set.seed(123)
g <- igraph::set_vertex_attr(g, "ip_addr", # "label"
                             value=stringr::str_c("192.168.1.", sample(1:254, vcount(g), replace=FALSE)))
# прошлись по граням
val <- stringr::str_c("UP = ", sample(1:10, ecount(g), replace=FALSE))
g <- igraph::set_edge_attr(g, "volume", value=val)
g <- igraph::set_edge_attr(g, "type", value=sample(letters[24:26], ecount(g), replace=TRUE))
plot(g, layout=layout_on_grid(g))


net <- intergraph::asNetwork(g) # пересоберем сетку, как это сделали выше
class(net)
lo <- layout_on_grid(g) # lo is a matrix of coordinates

# !! из анализа github понял, что можно в качестве layout матрицу подсовывать!!
n <- ggnetwork(g, layout=lo)

# не рисует метки на гранях, если подсовывать объект, преобразованный функцией ggnetwork!
# может из-за того, что там не вектора, а массивы 1 x n?

gp <-
  ggplot(n, aes(x=x, y=y, xend=xend, yend=yend)) +
  # geom_edges(aes(linetype=type, color=type, lwd=type)) +
  geom_edges(aes(linetype=type), color="grey75", lwd=1.2)+ #  , curvature = 0.1) +
  geom_nodes(color="gold", size=8) +
  # geom_nodelabel(aes(label=vertex.names), fontface="bold") +
  # geom_nodelabel_repel(aes(color=ip_addr, label=vertex.names), fontface = "bold", box.padding=unit(2, "lines")) +
  # geom_nodelabel_repel(aes(label=ip_addr), fontface = "bold", box.padding=unit(2, "lines"), 
  #                      segment.colour="red", segment.size=1) +
  geom_edgetext_repel(aes(label=volume), color="white", fill="grey25",
                      box.padding = unit(1, "lines")) +
  # geom_edgetext(aes(label=ip_addr, color=volume), fill="green", show.legend=TRUE) +
  geom_nodelabel(aes(label=ip_addr, color=volume), fill="blue", show.legend=TRUE) +
  # stat_debug_group() +
  
  theme_blank() +
  # theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "grey25"),
        panel.grid = element_blank())

gp
stop()

# =======================================================
# пример из описания, размещение меток по граням работает ---------------------------
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