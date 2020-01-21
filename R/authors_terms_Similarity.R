library(igraph)
library(tm)
library(SnowballC)
library(wordcloud)
library(slam)
library(proxy)
reviews = read.csv("../generated_data/AuthorAndTermsCleaned.csv", stringsAsFactors = T, row.names = 1)

dd<-data.frame(id=reviews$author,text=reviews$title_abstract)
colnames(dd) <- c("doc_id", "text") 
review_corpus <- VCorpus(DataframeSource(dd))

review_corpus = tm_map(review_corpus, content_transformer(tolower))
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords,  stopwords("french"))
review_corpus =  tm_map(review_corpus, stripWhitespace)
inspect(review_corpus[1])
meta(review_corpus[[1]])
review_tdm <- TermDocumentMatrix(review_corpus)
review_tdm

cosine_dist_mat <- crossprod_simple_triplet_matrix(review_tdm)/ (sqrt(col_sums(review_tdm^2) %*% t(col_sums(review_tdm^2))))*100

my_matrix <- as.matrix(cosine_dist_mat)
my_matrix[is.nan(my_matrix)] = 0
my_matrix
graph <- graph.adjacency(weighted=TRUE,my_matrix, mode='undirected', diag = FALSE)
E(graph)$weight
graph <- delete.edges(graph, which(E(graph)$weight<98))
graph <- delete.edges(graph, which(E(graph)$weight==100))
graph<-delete.vertices(graph,which(degree(graph)<1))
graph<-graph%>%
  set_edge_attr("label", value = E(graph)$weight)%>%
  set_edge_attr("width", value = 2)%>%
  set_edge_attr("color", value = "black")
V(graph)$label.color<-"black"
V(graph)$color<-"white"
tkplot(graph)
