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

cosine_dist_mat <- 1 - crossprod_simple_triplet_matrix(review_tdm)/(sqrt(col_sums(review_tdm^2) %*% t(col_sums(review_tdm^2))))
