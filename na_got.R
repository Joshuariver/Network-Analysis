# Source from R-blogger
# https://www.r-bloggers.com/network-analysis-of-game-of-thrones-family-ties/


rm(list=ls())
setwd("D:/R_Lab/network analysis")

library(tidyverse)
library(igraph)
library(statnet)
load("union_edges.RData")
load("union_characters.RData")


head(union_edges)


head(union_characters)

union_graph <- graph_from_data_frame(union_edges, directed = TRUE, vertices = union_characters)

color_vertices <- union_characters %>%
  group_by(house, color) %>%
  summarise(n = n()) %>%
  filter(!is.na(color))

colors_edges <- union_edges %>%
  group_by(type, color) %>%
  summarise(n = n()) %>%
  filter(!is.na(color))

layout <- layout_with_fr(union_graph)

plot(union_graph,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(union_graph)$name),
     vertex.shape = V(union_graph)$shape,
     vertex.color = V(union_graph)$color, 
     vertex.size = (V(union_graph)$popularity + 0.5) * 5, 
     vertex.frame.color = "gray", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.8,
     edge.arrow.size = 0.5,
     edge.color = E(union_graph)$color,
     edge.lty = E(union_graph)$lty)

legend("topleft", legend = c(NA, "Node color:", as.character(color_vertices$house), NA, "Edge color:", as.character(colors_edges$type)), pch = 19,
       col = c(NA, NA, color_vertices$color, NA, NA, colors_edges$color), pt.cex = 5, cex = 2, bty = "n", ncol = 1,
       title = "") 

legend("topleft", legend = "", cex = 4, bty = "n", ncol = 1,
       title = "Game of Thrones Family Ties")

union_graph_undir <- as.undirected(union_graph, mode = "collapse")

centr_degree(union_graph_undir, mode = "total")$centralization
## [1] 0.04282795
centr_clo(union_graph_undir, mode = "total")$centralization
## [1] 0.01414082
centr_eigen(union_graph_undir, directed = FALSE)$centralization

union_graph_undir_degree <- igraph::degree(union_graph_undir, mode = "total")

#standardized by number of nodes
union_graph_undir_degree_std <- union_graph_undir_degree / (vcount(union_graph_undir) - 1)
node_degree <- data.frame(degree = union_graph_undir_degree,
                          degree_std = union_graph_undir_degree_std) %>%
  tibble::rownames_to_column()

union_characters <- left_join(union_characters, node_degree, by = c("name" = "rowname"))

node_degree %>%
  arrange(-degree) %>%
  .[1:10, ]


# Closeness

closeness <- igraph::closeness(union_graph_undir, mode = "total")

#standardized by number of nodes
closeness_std <- closeness / (vcount(union_graph_undir) - 1)
node_closeness <- data.frame(closeness = closeness,
                             closeness_std = closeness_std) %>%
  tibble::rownames_to_column()

union_characters <- left_join(union_characters, node_closeness, by = c("name" = "rowname"))

node_closeness %>%
  arrange(-closeness) %>%
  .[1:10, ]


# betweenness

betweenness <- igraph::betweenness(union_graph_undir, directed = FALSE)

# standardize by number of node pairs
betweenness_std <- betweenness / ((vcount(union_graph_undir) - 1) * (vcount(union_graph_undir) - 2) / 2)

node_betweenness <- data.frame(betweenness = betweenness,
                               betweenness_std = betweenness_std) %>%
  tibble::rownames_to_column() 

union_characters <- left_join(union_characters, node_betweenness, by = c("name" = "rowname"))

node_betweenness %>%
  arrange(-betweenness) %>%
  .[1:10, ]


edge_betweenness <- igraph::edge_betweenness(union_graph_undir, directed = FALSE)

data.frame(edge = attr(E(union_graph_undir), "vnames"),
           betweenness = edge_betweenness) %>%
  tibble::rownames_to_column() %>%
  arrange(-betweenness) %>%
  .[1:10, ]


plot(union_graph_undir,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(union_graph_undir)$name),
     vertex.shape = V(union_graph_undir)$shape,
     vertex.color = V(union_graph_undir)$color, 
     vertex.size = betweenness * 0.001, 
     vertex.frame.color = "gray", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.8,
     edge.width = edge_betweenness * 0.01,
     edge.arrow.size = 0.5,
     edge.color = E(union_graph_undir)$color,
     edge.lty = E(union_graph_undir)$lty)
legend("topleft", legend = c("Node color:", as.character(color_vertices$house), NA, "Edge color:", as.character(colors_edges$type)), pch = 19,
       col = c(NA, color_vertices$color, NA, NA, colors_edges$color), pt.cex = 5, cex = 2, bty = "n", ncol = 1)


union_graph_undir_diameter <- union_graph_undir
node_diameter <- get.diameter(union_graph_undir_diameter,  directed = FALSE)

V(union_graph_undir_diameter)$color <- scales::alpha(V(union_graph_undir_diameter)$color, alpha = 0.5)
V(union_graph_undir_diameter)$size <- 2

V(union_graph_undir_diameter)[node_diameter]$color <- "red"
V(union_graph_undir_diameter)[node_diameter]$size <- 5

E(union_graph_undir_diameter)$color <- "grey"
E(union_graph_undir_diameter)$width <- 1

E(union_graph_undir_diameter, path = node_diameter)$color <- "red"
E(union_graph_undir_diameter, path = node_diameter)$width <- 5

plot(union_graph_undir_diameter,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(union_graph_undir_diameter)$name),
     vertex.shape = V(union_graph_undir_diameter)$shape,
     vertex.frame.color = "gray", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.8,
     edge.arrow.size = 0.5,
     edge.lty = E(union_graph_undir_diameter)$lty)
legend("topleft", legend = c("Node color:", as.character(color_vertices$house), NA, "Edge color:", as.character(colors_edges$type)), pch = 19,
       col = c(NA, color_vertices$color, NA, NA, colors_edges$color), pt.cex = 5, cex = 2, bty = "n", ncol = 1)


# Transitivity

transitivity(union_graph_undir, type = "global")
## [1] 0.2850679


transitivity <- data.frame(name = V(union_graph_undir)$name,
                           transitivity = transitivity(union_graph_undir, type = "local")) %>%
  mutate(name = as.character(name))

union_characters <- left_join(union_characters, transitivity, by = "name")

transitivity %>%
  arrange(-transitivity) %>%
  .[1:10, ]


# PageRank centrality

page_rank <- page.rank(union_graph_undir, directed = FALSE)

page_rank_centrality <- data.frame(name = names(page_rank$vector),
                                   page_rank = page_rank$vector) %>%
  mutate(name = as.character(name))

union_characters <- left_join(union_characters, page_rank_centrality, by = "name")

page_rank_centrality %>%
  arrange(-page_rank) %>%
  .[1:10, ]

adjacency <- as.matrix(as_adjacency_matrix(union_graph_undir))
                       
                       
#degree diagonal matrix
degree_diag <- diag(1 / igraph::degree(union_graph_undir))

# PageRank matrix
pagerank <- adjacency %*% degree_diag

eigenvalues <- eigen(pagerank)

eigenvector <- data.frame(name = rownames(pagerank),
                          eigenvector = as.numeric(eigenvalues$vectors[, which.max(eigenvalues$values)]))

union_characters <- left_join(union_characters, eigenvector, by = "name")

eigenvector %>%
  arrange(eigenvector) %>%
  .[1:10, ]


eigen_centrality <- igraph::eigen_centrality(union_graph_undir, directed = FALSE)

eigen_centrality <- data.frame(name = names(eigen_centrality$vector),
                               eigen_centrality = eigen_centrality$vector) %>%
  mutate(name = as.character(name))

union_characters <- left_join(union_characters, eigen_centrality, eigenvector, by = "name")

eigen_centrality %>%
  arrange(-eigen_centrality) %>%
  .[1:10, ]


union_characters %>%
  filter(!is.na(house2)) %>%
  dplyr::select(-contains("_std")) %>%
  gather(x, y, degree:eigen_centrality) %>%
  ggplot(aes(x = name, y = y, color = house2)) +
  geom_point(size = 3) +
  facet_grid(x ~ house2, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#igraph::dyad_census(union_graph_undir)
sna::dyad.census(adjacency)

#igraph::triad_census(union_graph_undir)
sna::triad.census(adjacency)

triad.classify(adjacency, mode = "graph")

node_kpath <- kpath.census(adjacency, maxlen = 5, mode = "graph", tabulate.by.vertex = TRUE, dyadic.tabulation = "sum")
edge_kpath <- kpath.census(adjacency, maxlen = 5, mode = "graph", tabulate.by.vertex = FALSE)
edge_kpath

gplot(node_kpath$paths.bydyad,
      label.cex = 0.5, 
      vertex.cex = 0.75,
      displaylabels = TRUE,
      edge.col = "grey")
node_kcycle <- kcycle.census(adjacency, maxlen = 8, mode = "graph", tabulate.by.vertex = TRUE, cycle.comembership = "sum")
edge_kcycle <- kcycle.census(adjacency, maxlen = 8, mode = "graph", tabulate.by.vertex = FALSE)
edge_kcycle

node_kcycle_reduced <- node_kcycle$cycle.comemb
node_kcycle_reduced <- node_kcycle_reduced[which(rowSums(node_kcycle_reduced) > 0), which(colSums(node_kcycle_reduced) > 0)]

gplot(node_kcycle_reduced,
      label.cex = 0.5, 
      vertex.cex = 0.75,
      displaylabels = TRUE,
      edge.col = "grey")


node_clique <- clique.census(adjacency, mode = "graph", tabulate.by.vertex = TRUE, clique.comembership = "sum")
edge_clique <- clique.census(adjacency, mode = "graph", tabulate.by.vertex = FALSE, clique.comembership = "sum")
edge_clique$clique.count
##   1   2   3 
##   0  74 105
node_clique_reduced <- node_clique$clique.comemb
node_clique_reduced <- node_clique_reduced[which(rowSums(node_clique_reduced) > 0), which(colSums(node_clique_reduced) > 0)]

gplot(node_clique_reduced,
      label.cex = 0.5, 
      vertex.cex = 0.75,
      displaylabels = TRUE,
      edge.col = "grey")



vcol <- rep("grey80", vcount(union_graph_undir))

# highlight first of largest cliques
vcol[unlist(largest_cliques(union_graph_undir)[[1]])] <- "red"

plot(union_graph_undir,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(union_graph_undir)$name),
     vertex.shape = V(union_graph_undir)$shape,
     vertex.color = vcol, 
     vertex.size = 5, 
     vertex.frame.color = "gray", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.8,
     edge.width = 2,
     edge.arrow.size = 0.5,
     edge.color = E(union_graph_undir)$color,
     edge.lty = E(union_graph_undir)$lty)


ceb <- cluster_edge_betweenness(union_graph_undir)
modularity(ceb)
## [1] 0.8359884
plot(ceb,
     union_graph_undir,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(union_graph_undir)$name),
     vertex.shape = V(union_graph_undir)$shape,
     vertex.size = (V(union_graph_undir)$popularity + 0.5) * 5, 
     vertex.frame.color = "gray", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.8)


clp <- cluster_label_prop(union_graph_undir)

plot(clp,
     union_graph_undir,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(union_graph_undir)$name),
     vertex.shape = V(union_graph_undir)$shape,
     vertex.size = (V(union_graph_undir)$popularity + 0.5) * 5, 
     vertex.frame.color = "gray", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.8)


library(NetIndices)
graph.properties <- GenInd(adjacency)
graph.properties

library(network)
adj_network <- network(adjacency, directed = TRUE)
adj_network

network.dyadcount(adj_network)
## [1] 43056
network.edgecount(adj_network)
## [1] 652
network.size(adj_network)

ec <- equiv.clust(adj_network, mode = "graph", cluster.method = "average", plabels = network.vertex.names(adj_network))
ec

ec$cluster$labels <- ec$plabels
plot(ec)

gden(adjacency)
## [1] 0.01514307
grecip(adjacency)
## Mut 
##   1
grecip(adjacency, measure = "edgewise")

sessionInfo()
