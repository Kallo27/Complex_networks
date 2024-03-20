library(igraph)
library(ggplot2)
library(RColorBrewer)

g <- make_empty_graph()
summary(g)

g <- make_graph(edges = c(1, 2, 3, 4, 5, 6), n = 8, directed = TRUE)
summary(g)

is_directed(g)

g <- make_graph(~ 1--2,2--3,3--4,4--5,5--6,6--1,4,5,6,7,8)
get.edgelist(g)
get.adjacency(g)
as.matrix(get.adjacency(g))

laplacian_matrix(g)
laplacian_matrix(g,normalized = TRUE)
#colSums(laplacian_matrix(g,normalized = TRUE)) #sparse matrix doesn't work
colSums(laplacian_matrix(g,normalized = TRUE, sparse = FALSE))
colSums(as.matrix(laplacian_matrix(g,normalized = TRUE)))
colSums(as.matrix(laplacian_matrix(g,normalized = FALSE)))

V(g) # information about vertices
vcount(g)
V(g)$name
V(g)$label
V(g)$label <- letters[1:vcount(g)]
V(g)$color <- rainbow(vcount(g))
V(g)$color
g2<-add_vertices(g,3)
V(g2)$name #it doesnt't know the names of the last two
V(g2)$name <- 1:vcount(g2)
g3<-delete_vertices(g2,10)

ecount(g) #count edges
E(g)$weight
E(g)$weight<- rpois(ecount(g),2) #generatore numeri poissoniani
E(g)$weight<- rpois(ecount(g),2)+1

g<-add_edges(g, edges=c(2,7, 3,1)) #one way of adding edges
g<-g+edges(c(7,8)) #another way of adding edges
g<-g-edges(c(7,8)) #to delete an edge
delete_edges(g, c(1,2))
g


set.seed(12345)
my_edges<-data.frame(from=sample(letters,10), to=sample(letters,10), weight=runif(10) )
g<-graph_from_data_frame(my_edges, directed=T)
V(g)$name
plot(g)

n=200
my_edges<-data.frame(from=sample(letters,n, replace=T), to=sample(letters,n, replace=T), weight=runif(n) )
g<-graph_from_data_frame(my_edges, directed=T)
plot(g, edge_width=E(g)$weight, edge.arrow.size=0.4)

g<-make_graph("Zachary")
plot(g, vertex.color=rainbow(vcount(g)), vertex.label=NA)
plot(g, vertex.color="steelblue", vertex.label=NA, edge.curved=0.2)
lay<-layout_in_circle(g)
plot(g, vertex.color="steelblue", vertex.label=NA, edge.curved=0.2, layout=lay)
lay<-layout_with_kk(g) #reduce the overlap between nodes, nodes spread as much as possible
lay<-layout_with_fr(g) 

N<-100
g<-erdos.renyi.game(N, p=log(N)/N, directed=F)
lay<-layout_with_fr(g)
plot(g, vertex.color="steelblue", vertex.label=NA, edge.curved=0.2, layout=lay, vertex.size=2.5, edge.color="gray80", vertex.frame.color=NA)

g<-sample_grg(N, radius=0.2)
lay<-layout_with_fr(g)
plot(g, vertex.color="steelblue", vertex.label=NA, edge.curved=0.2, layout=lay, vertex.size=2.5, edge.color="gray80", vertex.frame.color=NA)

N<-500
g<-barabasi.game(N, m=1, directed=FALSE)
lay<-layout_with_fr(g)
plot(g, vertex.color="steelblue", vertex.label=NA, edge.curved=0.2, layout=lay, vertex.size=2.5, edge.color="gray80", vertex.frame.color=NA)

g<-barabasi.game(N, m=2, directed=FALSE)
lay<-layout_with_fr(g)
plot(g, vertex.color="steelblue", vertex.label=NA, edge.curved=0.2, layout=lay, vertex.size=2.5, edge.color="gray80", vertex.frame.color=NA)

degree(g)
plot(g, vertex.color="steelblue", vertex.label=NA, edge.curved=0.2, layout=lay, vertex.size=log(degree(g)), edge.color="gray80", vertex.frame.color=NA)

N<-1000
OM<-matrix(0.0003,ncol=4,nrow=4)
diag(OM)<-0.03
g<-sample_sbm(N, pref.matrix=OM, block.sizes = c(200,200,300,300))
lay<-layout_with_fr(g)
plot(g, vertex.color="steelblue", vertex.label=NA, edge.curved=0.2, layout=lay, vertex.size=4*log10(degree(g)), edge.color="gray80", vertex.frame.color=NA)