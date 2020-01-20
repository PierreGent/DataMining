library(igraph)
library(tm)
library(SnowballC)
library(wordcloud)
reviews = read.csv("../generated_data/AuthorAndTermsCleaned.csv", stringsAsFactors = T, row.names = 1)
review_corpus = Corpus(VectorSource(reviews$title_abstract))
review_corpus = tm_map(review_corpus, content_transformer(tolower))
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords,  stopwords("french"))
review_corpus =  tm_map(review_corpus, stripWhitespace)
inspect(review_corpus[1])

review_dtm <- DocumentTermMatrix(review_corpus)
review_dtm
library(slam)
cosine_dist_mat <- 1 - crossprod_simple_triplet_matrix(review_dtm)/(sqrt(col_sums(review_dtm^2) %*% t(col_sums(review_dtm^2))))
cosine_dist_mat[1,1:20]
review_dtm = removeSparseTerms(review_dtm, 0.99)
review_dtm
inspect(review_dtm[15,1:20])

inspect(review_dtm[500:505, 500:505])

findFreqTerms(review_dtm, 1000)

freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))


review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf
# The first document
inspect(review_dtm_tfidf[1,1:20])

freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

my_data <- read.csv('../generated_data/outputTerms.csv', header = TRUE, row.names = 1)
my_matrix <- as.matrix(my_data)
plot(my_matrix)


