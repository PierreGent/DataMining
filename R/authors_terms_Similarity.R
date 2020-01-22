library(igraph)
library(tm)
library(SnowballC)
library(wordcloud)
library(slam)
library(proxy)
reviews = read.csv("../generated_data/AuthorAndTermsCleaned.csv", stringsAsFactors = T, row.names = 1)

dd<-data.frame(id=reviews$author,text=reviews$title_abstract)

############################# Filtre de données #########################################
my_data <- read.csv('../generated_data/output.csv', header = TRUE, row.names = 1)
my_matrix <- as.matrix(my_data)
graph <- graph.adjacency(weighted=TRUE,my_matrix, mode='undirected', diag = FALSE)

graph <- delete.vertices(graph,  which(degree(graph)<11))

graph <- delete.vertices(graph, which(degree(graph)==0))
mymat<-as_adjacency_matrix(graph)
v<-rownames(mymat)
v<-gsub("\\.", " ", v)
colnames(dd) <- c("doc_id", "text") 
dd<-dd[(dd$doc_id %in% v),]

############################# Fin Filtre de données #####################################
############################# Création du corpus ########################################

review_corpus <- VCorpus(DataframeSource(dd))

review_corpus = tm_map(review_corpus, content_transformer(tolower))
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords,  stopwords("french"))
review_corpus =  tm_map(review_corpus, stripWhitespace)
inspect(review_corpus[1])
meta(review_corpus[[1]])
review_tdm <- TermDocumentMatrix(review_corpus,control = list( stemming = T))
inspect(review_tdm)

############################# Fin Création du corpus ######################################
############################# Association de mots #########################################
findAssocs(review_tdm, 'donné', 0.4)


my.df <- as.data.frame(inspect(review_tdm))
my.df.scale <- scale(my.df)

d <- dist(my.df.scale,method="euclidean")
fit <- hclust(d, method="ward.D2")
plot(fit)

############################# FIN Association de mots #####################################
############################# Similarité ##################################################
cosine_dist_mat <- round(crossprod_simple_triplet_matrix(review_tdm)/ (sqrt(col_sums(review_tdm^2) %*% t(col_sums(review_tdm^2))))*100,digits=2)
cosine_dist_mat["Pascal Poncelet","Anne Laurent"]
my_matrix <- as.matrix(cosine_dist_mat)
#my_matrix[is.nan(my_matrix)] = 0
#my_matrix[1,2]
graph <- graph.adjacency(weighted=TRUE,my_matrix, mode='undirected', diag = FALSE)
#E(graph)$weight
graph <- delete.edges(graph, which(E(graph)$weight<70))
graph <- delete.edges(graph, which(E(graph)$weight==100))
graph<-delete.vertices(graph,which(degree(graph)<1))
graph<-graph%>%
  set_edge_attr("label", value = E(graph)$weight)%>%
  set_edge_attr("width", value = 2)%>%
  set_edge_attr("color", value = "black")
V(graph)$label.color<-"black"
V(graph)$color<-"white"

par(cex=0.5)
community <- walktrap.community(graph,steps = 2)
plot(vertex.label=NA,community,graph,vertex.size=5)
par(cex=0.7)
plot_dendrogram(community)
tkplot(graph)
