library(igraph)
library(ggplot2)
library(RColorBrewer)
library(rgl)

N <- 1000 # nolint: object_name_linter.
g <- erdos.renyi.game(N, p = 0.1 / N, directed = FALSE)

lay <- layout_with_fr(g)
plot(g, vertex.color="steelblue", vertex.label=NA, edge.curved=0.2, 
    layout=lay, vertex.size=2.5, edge.color="gray80", vertex.frame.color=NA)

clus <- clusters(g)
clus # do it in the command line

ggplot(data.frame((s=clus$csize), aes = s)) + theme_bw() + geom_histogram()

res <- data.frame()
for(p in seq(0.1/N, 2*log(N)/N, 0.02/N)){
    g <- erdos.renyi.game(N, p=p, directed = FALSE)
    clu <- clusters(g)
    res <- rbind(res, data.frame(p = p, n = clu$no, lcc = max(clu$csize)/N))
}

res # do it in the command line

ggplot(res, aes(p, n)) + theme_bw() + geom_line() + scale_x_log10()

ggplot(res, aes(p, lcc)) + theme_bw() + geom_line() + scale_x_log10()

ggplot(res, aes(p, lcc)) + theme_bw() + geom_line() + scale_x_log10() + geom_vline(xintercept = 1/N, color = 'tomato', linetype = 'dashed')

source("common.R")
mypal <- colorRampPalette(rev(c(brewer.pal(9, "YlGnBu"))))(20)

node_info <- read.csv("city_undir_nodes.csv", header = TRUE)
head(node_info)

edge_info <- read.csv("city_undir_edges.csv", header = TRUE)
head(edge_info)

nodes <- node_info[, c("osmid", "lat", "lon")]
edges <- data.frame(from = edge_info$u, to = edge_info$v)

g <- graph_from_data_frame(edges, directed = FALSE, vertices = as.character(nodes$osmid))
g # do it in the command line

g <- simplify(g)
layout <- matrix(0, vcount(g), 2)
layout[, 1] <- nodes$lon
layout[, 2] <- nodes$lat

v_centr <- closeness(g, normalized = FALSE)

colors <- vec2pal(v_centr, mypal)
sizes <- v_centr/max(v_centr)

plot(g, vertex.color=colors, vertex.label=NA, layout=layout, vertex.size=sizes, edge.color="gray80", vertex.frame.color=NA, edge_width = 0.5)

g <- graph_from_data_frame(edges, directed = FALSE, vertices = as.character(nodes$osmid))

# new dataset

node_info <- read.table("brain_meta.csv", header = TRUE, sep = ";")
head(node_info)

adj_info <- read.table("brain_adj.csv", header = TRUE, sep = ";")
head(adj_info)

node_info$nodeID <- node_info$nodeID + 1
adj_info  <- adj_info[1:800, 2:801]

g <- graph_from_adjacency_matrix(as.matrix(adj_info), mode = "undirected", weighted = TRUE, add.colnames = FALSE)

g <- simplify(g)
layout <- matrix(0, vcount(g), 3)
layout[,1] <- node_info$x
layout[,2] <- node_info$y
layout[,3] <- node_info$z

v_centr <- closeness(g, normalized = FALSE)

node_colors <- vec2pal(v_centr, mypal)
degs <- degree(g)
sizes <- degs/max(degs)

plot(g, vertex.color=node_colors, vertex.label=NA, layout=layout, vertex.size=5 * sizes, edge.color="gray80", vertex.frame.color=NA, edge_width = 0.1 * E(g)$weight)

rglplot(g, vertex.color=node_colors, vertex.label=NA, layout=layout, vertex.size=5 * sizes, edge.color="gray80", vertex.frame.color=NA, edge_width = 0.1 * E(g)$weight)
