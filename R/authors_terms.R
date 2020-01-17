library(igraph)
my_data <- read.csv('../generated_data/outputTerms.csv', header = TRUE, row.names = 1)
my_matrix <- as.matrix(my_data)
plot(my_matrix)


