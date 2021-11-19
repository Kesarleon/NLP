#load required pacakges
if(!require("readtext"))
  install.packages("readtext")
library(readtext)

if(!require("tm"))
  install.packages("tm")
library(tm)

if(!require("stringr"))
  install.packages("stringr")
library(stringr)

if(!require("qdap"))
  install.packages("qdap")
library(qdap)

if(!require("slam"))
  install.packages("slam")
library(slam)

#data files are uploaded at below location:
#https://github.com/sureshgorakala/machinelearning/tree/master/data

#load all content files
#news_docs = readtext("*.txt")
news_docs = readtext("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/*.txt")
news_list = lapply(news_docs[,2],function(x) genX(x, " [", "]"))
N.docs = length(news_list)
names(news_list) = news_docs[,1]


# Crear query

texto <- c('Barack Obama', 'Hillary', 'visit us')
write.table(texto, 
            file = "C:/Users/Cesar/Documents/Cesar/Kesar/NLP/query_prueba.txt",
            quote = F,
            row.names = F,
            col.names = F)


#load search queries
#search_queries = readtext("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/query.txt",dvsep = "\n")
search_queries = readtext("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/query_prueba.txt",dvsep = "\n")

queries_list = unlist(strsplit(search_queries[1,2],"\n"))
N.query = length(queries_list)
names(queries_list) = paste0("query", c(1:N.query))

#preprocess data news content
#append both content and search queries together, convert the lists to VectorSource
newscorpus = VectorSource(c(news_list,queries_list))
newscorpus$Names = c(names(news_list),names(queries_list))
#convert to corpus format
newscorpus_preproc = Corpus(newscorpus)
#cleaning the data
newscorpus_preproc = tm_map(newscorpus_preproc,stripWhitespace)
newscorpus_preproc = tm_map(newscorpus_preproc,removePunctuation)
newscorpus_preproc = tm_map(newscorpus_preproc,content_transformer(tolower))
newscorpus_preproc = tm_map(newscorpus_preproc,removeWords,stopwords("english"))


#create tdm using weighted tfidf weightage
tdm = TermDocumentMatrix(newscorpus_preproc,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
tdm_mat = as.matrix(tdm)
colnames(tdm_mat) = c(names(news_list),names(queries_list))

#normalizing the term document matrix
tfidf_mat <- scale(tdm_mat, center = FALSE,scale = sqrt(colSums(tdm_mat^2)))

#seperating query tdm matrix and content tdm matrix
query.vectors <- tfidf_mat[, (N.docs + 1):(N.docs+N.query)]
tfidf_mat <- tfidf_mat[, 1:N.docs]

#calculating the similarity scores
doc.scores <- t(query.vectors) %*% tfidf_mat

results.df <- data.frame(querylist = queries_list,doc.scores)

#function to display the final results
showTopresults <- function(query){
  x = results.df[which(results.df$querylist == query),]
  yy =  data.frame(t(x),rownames(t(x)),row.names = NULL)[-1,]
  names(yy) = c("score","docs")
  yy$score = as.numeric(as.character(yy$score))
  yyy = yy[order(yy$score,decreasing = T),]
  
  return(yyy[which(yyy$score > 0),][1:3,])
}

#test the function
#showTopresults("narendra modi visit to washington")
showTopresults(queries_list[1])

##############################################################

library(geniusr)

# 1

temp <- scrape_lyrics_url(song_lyrics_url = "https://genius.com/Michael-kiwanuka-cold-little-heart-lyrics")
write.table(temp$line,
            file = paste0("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/Lyrics/",
                          str_replace_all(temp$song_name[1],
                                          ' ',
                                          '_'),
                          ".txt"
            ),
            quote = F,
            row.names = F,
            col.names = F)

# 2
temp <- scrape_lyrics_url(song_lyrics_url = "https://genius.com/Michael-kiwanuka-the-final-frame-lyrics")
write.table(temp$line,
            file = paste0("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/Lyrics/",
                          str_replace_all(temp$song_name[1],
                                          ' ',
                                          '_'),
                          ".txt"
                          ),
            quote = F,
            row.names = F,
            col.names = F)

# 3

temp <- scrape_lyrics_url(song_lyrics_url = "https://genius.com/Michael-kiwanuka-love-and-hate-lyrics")
write.table(temp$line,
            file = paste0("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/Lyrics/",
                          str_replace_all(temp$song_name[1],
                                          ' ',
                                          '_'),
                          ".txt"
            ),
            quote = F,
            row.names = F,
            col.names = F)

# 4

temp <- scrape_lyrics_url(song_lyrics_url = "https://genius.com/Michael-kiwanuka-one-more-night-lyrics")
write.table(temp$line,
            file = paste0("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/Lyrics/",
                          str_replace_all(temp$song_name[1],
                                          ' ',
                                          '_'),
                          ".txt"
            ),
            quote = F,
            row.names = F,
            col.names = F)

# 5

temp <- scrape_lyrics_url(song_lyrics_url = "https://genius.com/Karen-o-yo-my-saint-film-version-lyrics")
write.table(temp$line,
            file = paste0("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/Lyrics/",
                          str_replace_all(temp$song_name[1],
                                          ' ',
                                          '_'),
                          ".txt"
            ),
            quote = F,
            row.names = F,
            col.names = F)

news_lyrics = readtext("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/Lyrics/*.txt")

#load all content files

#news_docs = readtext("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/*.txt")
news_lyrics = readtext("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/Lyrics/*.txt")
news_list = lapply(news_lyrics[,2],function(x) genX(x, " [", "]"))
N.docs = length(news_list)
names(news_list) = news_lyrics[,1]


# Crear query

texto <- c('Heart', 'Love', 'Feelings')
write.table(texto, 
            file = "C:/Users/Cesar/Documents/Cesar/Kesar/NLP/Lyrics/query_prueba.txt",
            quote = F,
            row.names = F,
            col.names = F)


#load search queries
#search_queries = readtext("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/query.txt",dvsep = "\n")
search_queries = readtext("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/Lyrics/query_prueba.txt",dvsep = "\n")

queries_list = unlist(strsplit(search_queries[1,2],"\n"))
N.query = length(queries_list)
names(queries_list) = paste0("query", c(1:N.query))

#preprocess data news content
#append both content and search queries together, convert the lists to VectorSource
newscorpus = VectorSource(c(news_list,queries_list))
newscorpus$Names = c(names(news_list),names(queries_list))
#convert to corpus format
newscorpus_preproc = Corpus(newscorpus)
#cleaning the data
newscorpus_preproc = tm_map(newscorpus_preproc,stripWhitespace)
newscorpus_preproc = tm_map(newscorpus_preproc,removePunctuation)
newscorpus_preproc = tm_map(newscorpus_preproc,content_transformer(tolower))
newscorpus_preproc = tm_map(newscorpus_preproc,removeWords,stopwords("english"))


#create tdm using weighted tfidf weightage
tdm = TermDocumentMatrix(newscorpus_preproc,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
tdm_mat = as.matrix(tdm)
colnames(tdm_mat) = c(names(news_list),names(queries_list))

#normalizing the term document matrix
tfidf_mat <- scale(tdm_mat, center = FALSE,scale = sqrt(colSums(tdm_mat^2)))

#seperating query tdm matrix and content tdm matrix
query.vectors <- tfidf_mat[, (N.docs + 1):(N.docs+N.query)]
tfidf_mat <- tfidf_mat[, 1:N.docs]

#calculating the similarity scores
doc.scores <- t(query.vectors) %*% tfidf_mat

results.df <- data.frame(querylist = queries_list,doc.scores)

#function to display the final results
showTopresults <- function(query){
  x = results.df[which(results.df$querylist == query),]
  yy =  data.frame(t(x),rownames(t(x)),row.names = NULL)[-1,]
  names(yy) = c("score","docs")
  yy$score = as.numeric(as.character(yy$score))
  yyy = yy[order(yy$score,decreasing = T),]
  
  return(yyy[which(yyy$score > 0),][1:3,])
}

#test the function
#showTopresults("narendra modi visit to washington")
showTopresults(queries_list[3])

################################################################# Nach


temp <- scrape_lyrics_url(song_lyrics_url = "https://genius.com/Nach-tal-como-eres-lyrics")
write.table(temp$line,
            file = paste0("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/Nach/",
                          str_replace_all(temp$song_name[1],
                                          ' ',
                                          '_'),
                          ".txt"
            ),
            quote = F,
            row.names = F,
            col.names = F)

nach_lyrics = readtext("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/Nach/*.txt")

news_list = lapply(nach_lyrics[,2],function(x) genX(x, " [", "]"))
N.docs = length(news_list)
names(news_list) = nach_lyrics[,1]


# Crear query

texto <- c('amor', 'odio', 'humano', 'miedo', 'temor', 'ser', 'rap', 'niño')
write.table(texto, 
            file = "C:/Users/Cesar/Documents/Cesar/Kesar/NLP/Querys/query_nach.txt",
            quote = F,
            row.names = F,
            col.names = F)

search_queries = readtext("C:/Users/Cesar/Documents/Cesar/Kesar/NLP/Querys/query_nach.txt",dvsep = "\n")

queries_list = unlist(strsplit(search_queries[1,2],"\n"))
N.query = length(queries_list)
names(queries_list) = paste0("query", c(1:N.query))

#preprocess data news content
#append both content and search queries together, convert the lists to VectorSource
newscorpus = VectorSource(c(news_list,queries_list))
newscorpus$Names = c(names(news_list),names(queries_list))
#convert to corpus format
newscorpus_preproc = Corpus(newscorpus)
#cleaning the data
newscorpus_preproc = tm_map(newscorpus_preproc,stripWhitespace)
newscorpus_preproc = tm_map(newscorpus_preproc,removePunctuation)
newscorpus_preproc = tm_map(newscorpus_preproc,content_transformer(tolower))
newscorpus_preproc = tm_map(newscorpus_preproc,removeWords,stopwords("SPANISH"))


#create tdm using weighted tfidf weightage
tdm = TermDocumentMatrix(newscorpus_preproc,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
tdm_mat = as.matrix(tdm)
colnames(tdm_mat) = c(names(news_list),names(queries_list))

#normalizing the term document matrix
tfidf_mat <- scale(tdm_mat, center = FALSE,scale = sqrt(colSums(tdm_mat^2)))

#seperating query tdm matrix and content tdm matrix
query.vectors <- tfidf_mat[, (N.docs + 1):(N.docs+N.query)]
tfidf_mat <- tfidf_mat[, 1:N.docs]

#calculating the similarity scores
doc.scores <- t(query.vectors) %*% tfidf_mat

results.df <- data.frame(querylist = queries_list,doc.scores)

#function to display the final results
showTopresults <- function(query){
  x = results.df[which(results.df$querylist == query),]
  yy =  data.frame(t(x),rownames(t(x)),row.names = NULL)[-1,]
  names(yy) = c("score","docs")
  yy$score = as.numeric(as.character(yy$score))
  yyy = yy[order(yy$score,decreasing = T),]
  
  return(yyy[which(yyy$score > 0),][1:3,])
}

#test the function
#showTopresults("narendra modi visit to washington")
showTopresults(queries_list[3])



# Clusters

library(ggplot2)
library(car)
library(cluster)
library(factoextra)
library(clustertend)
library(clValid)


df <- data.frame(t(doc.scores))
head(df)
rownames(df)
summary(df)

# K-means

set.seed(20)
irisCluster <- kmeans(df[, -c(3,8)], 6)#, nstart = 20)
irisCluster

table(irisCluster$cluster)

irisCluster$cluster <- as.factor(irisCluster$cluster)


# Clusters Jerárquicos


clusters <- hclust(dist(df[, -c(3,8)])) # Method = complete por default
plot(clusters)

clusterCut <- cutree(clusters, 3)

table(clusterCut)

clusters <- hclust(dist(df[, -c(3,8)]), method = 'average')
plot(clusters)

clusterCut <- cutree(clusters, 3)
table(clusterCut)


#########################################################################################################
data <- df[, -c(3,8)]

# Matriz de disimilaridad
d <- dist(data, method = "euclidean")


hc1 <- hclust(d, method = "complete" )

plot(hc1, cex = 0.6, hang = -1)

######################################################

set.seed(123)
EstadisticoHopkins <- hopkins(data, 10)
EstadisticoHopkins

set.seed(123)
wss <- fviz_nbclust(data, kmeans, method = 'wss') +
  labs(subtitle = 'Elbow Method')
wss


silhouette <- fviz_nbclust(data, kmeans, method = 'silhouette') +
  labs(subtitle = 'Silhouette Method')
silhouette


evaluaciones <- clValid(data, nClust = 2:6, clMethods = 'kmeans', validation = 'internal')
summary(evaluaciones)

set.seed(123)
ClusterKmedias_2 <- kmeans(data, 2)#, nstart = 25)
fviz_cluster(ClusterKmedias_2, data)

ClusterKmedias_6 <- kmeans(data, 4)#, nstart = 25)
fviz_cluster(ClusterKmedias_6, data)


