library(igraph)
library(tm)
library(SnowballC)
library(wordcloud)
reviews = read.csv("../generated_data/AuthorAndTermsCleaned.csv", stringsAsFactors = T, row.names = 1)

dd<-data.frame(id=reviews$author,text=reviews$title_abstract)
colnames(dd) <- c("doc_id", "text") 
review_corpus <- VCorpus(DataframeSource(dd))
review_corpus = tm_map(review_corpus, content_transformer(tolower))
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords,  stopwords("french"))
review_corpus = tm_map(review_corpus, removeWords,  stopwords("english"))
review_corpus =  tm_map(review_corpus, stripWhitespace)
inspect(review_corpus[1])
meta(review_corpus[[1]])
############################# Normal, sans stemm#########################################
review_dtm <- DocumentTermMatrix(review_corpus, control = list(
                                                               stemming = F))
review_dtm

review_dtm = removeSparseTerms(review_dtm, 0.99)
review_dtm
inspect(review_dtm[15,1:20])

inspect(review_dtm[500:505, 500:505])

findFreqTerms(review_dtm, 1000)

freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))


############################# FIN Normal, sans stemm #####################################
############################# TF-IDF, sans stemm #########################################
review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf,
                                                                     stemming = F))
review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf
freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
freq
par(cex=0.6)
wordcloud(rownames(freq), freq[,1], max.words=40, colors=brewer.pal(1, "Dark2"))

############################# FIN TF-IDF, sans stemm #####################################
############################# TF-IDF, avec stemm #########################################
review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf,
                                                                     stemming = T))
review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf

# The first document
inspect(review_dtm_tfidf[1,1:20])

freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
freq
par(cex=0.6)
wordcloud(rownames(freq), freq[,1], max.words=40, colors=brewer.pal(1, "Dark2"))
############################# FIN TF-IDF, avec stemm #########################################




