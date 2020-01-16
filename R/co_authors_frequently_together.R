library(igraph)
my_data <- read.csv('../generated_data/output.csv', header = TRUE, row.names = 1)
my_matrix <- as.matrix(my_data)
graph <- graph.adjacency(weighted=TRUE,my_matrix, mode='undirected', diag = FALSE)
graphSave<-graph
graph <- delete.edges(graph, which(E(graph)$weight<7))
graph<-delete.vertices(graph,which(degree(graph)<1))
community <- walktrap.community(graph)
graph<-graph%>%
  set_edge_attr("label", value = E(graph)$weight)

V(graph)$size=igraph::degree(graphSave)*2
V(graph)$label.color="green"
tkplot(graph,canvas.width = 1200, canvas.height = 600)
par(cex=0.1)
plot(Layout=layout_with_kk,community,graph)
par(cex=0.9)
plot_dendrogram(community)


