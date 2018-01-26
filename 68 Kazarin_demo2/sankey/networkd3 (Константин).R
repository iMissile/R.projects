library(tidyverse)
library(networkD3)
library(igraph)

catalog <- read.csv(file = "./sankey/catalog.csv") 

catalog_net <- catalog %>% 
  select("cost_element_name", "cost_group_name", "cost_ius_name", "cost_class_name") %>% 
  rename("a" = "cost_element_name",
         "b" = "cost_group_name",
         "c" = "cost_ius_name",
         "d" = "cost_class_name")

catalog_net[] <- paste(names(catalog_net)[col(catalog_net)], unlist(catalog_net), sep =  "_")

# Вариант 1 

ss <- tail(seq_along(catalog_net), -2)
edge_list <- purrr::map_dfr(ss, ~tibble(source=catalog_net[[.x-1]], target=catalog_net[[.x]]))
# Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
gD <- igraph::simplify(igraph::graph.data.frame(edge_list, directed=FALSE))

# gD <- igraph::simplify(igraph::graph.data.frame(edge_list, directed=FALSE))

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


# Вариант 2
map_a_b <- catalog_net %>% group_by(a, b) %>% dplyr::summarise(sum=n())
map_b_c <- catalog_net %>% group_by(b, c) %>% dplyr::summarise(sum=n())
map_c_d <- catalog_net %>% group_by(c, d) %>% dplyr::summarise(sum=n())

colnames(map_a_b)[1:2] <- colnames(map_b_c)[1:2] <- colnames(map_c_d)[1:2] <- c("source","target")

links_1 <- rbind(as.data.frame(map_a_b), 
               as.data.frame(map_b_c), 
               as.data.frame(map_c_d))

nodes_1 <- data.frame(name=unique(c(links_1$source, links_1$target)))
links_1$source <- match(links_1$source, nodes_1$name) - 1
links_1$target <- match(links_1$target, nodes_1$name) - 1

sankeyNetwork(Links = links_1, Nodes = nodes_1, Source = "source",
              Target = "target", Value = "sum", NodeID = "name",
              fontSize = 12, nodeWidth = 30)