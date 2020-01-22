library(igraph)
my_data <- read.csv('../generated_data/output.csv', header = TRUE, row.names = 1)
my_matrix <- as.matrix(my_data)
graph <- graph.adjacency(weighted=TRUE,my_matrix, mode='undirected', diag = FALSE)

graph <- delete.vertices(graph, which(degree(graph)<15))

graph <- delete.vertices(graph, which(degree(graph)==0))
mymat<-as_adjacency_matrix(graph,attr="weight")
community <- walktrap.community(graph)
graph<-graph%>%
  set_edge_attr("label", value = E(graph)$weight)
par(cex=0.1)
E(graph)$label.cex=10
E(graph)$label.color="red"

plot(community, graph, vertex.label = NA,vertex.size=5)
par(cex=0.9)
plot_dendrogram(community)


