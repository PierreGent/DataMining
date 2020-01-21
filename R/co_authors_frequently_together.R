library(igraph)
my_data <- read.csv('../generated_data/output.csv', header = TRUE, row.names = 1)
my_matrix <- as.matrix(my_data)
graph <- graph.adjacency(weighted=TRUE,my_matrix, mode='undirected', diag = FALSE)
V(graph)$size<-degree(graph)
graph <- delete.edges(graph, which(E(graph)$weight<5))
graph<-delete.vertices(graph,which(degree(graph)<1))
community <- walktrap.community(graph)
graph<-graph%>%
  set_edge_attr("label", value = E(graph)$weight)%>%
  set_edge_attr("width", value = 2)%>%
  set_edge_attr("color", value = "black")

V(graph)$label.color<-"black"
V(graph)$color<-"white"

tkplot(vertex.label=V(graph)$weight,graph,canvas.width = 1200, canvas.height = 600)


