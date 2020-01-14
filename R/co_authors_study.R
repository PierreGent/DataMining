library(igraph)
my_data <- read.csv('../generated_data/output.csv', header = TRUE, row.names = 1)
my_matrix <- as.matrix(my_data)
graph <- graph.adjacency(my_matrix, mode='undirected', diag = FALSE)



isolated <- which(degree(graph)<15)
g2 <- delete.vertices(graph, isolated)

isolated2 <- which(degree(g2)==0)
g3 <- delete.vertices(g2, isolated2)

community <- walktrap.community(g3)

plot(community, g3, vertex.label = NA)
par(cex=0.6)
plot_dendrogram(community)
