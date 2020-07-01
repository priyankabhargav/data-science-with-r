library(quanteda)
library(stringr)
library(dplyr)
library(tidyverse)
library(tm)
library(tokenizers)
library(reticulate)
library(data.table)
library(textreg)
library(ggplot2)
library(rbokeh)
library(caret)  
library(Rtsne)
library(cluster)    
library(factoextra)
library(hash)
library(LDAvis)
library(servr)
library(ldatuning)
library(topicmodels)
install.packages('topicmodels')


#Loading the complete Dataset
load_dataset <- function(path_to_dataset){
  main_df <- read.csv(path_to_dataset)
  #Removing duplicate urls
  main_df <- main_df[!duplicated(main_df$url), ]
  #Removing duplicate content
  main_df <- main_df[!duplicated(main_df$content), ]
  #Removing rows having content as empty
  #main_df <- main_df %>% filter(!is.na(content)) %>% select(content)
  return (main_df)
}

#Pre-processing the Data
preprocess <- function(each_row){
  #Removing all teh url's from the content
  x <- gsub("(www|http)[^[:space:]]*", " ", each_row) 
  #Replacing the punctuations with blank space
  x <- str_replace_all(x, "[[:punct:]]" , "")
  x <- str_replace_all(x, "[\t\r\n]" , " ")
  x <- str_replace_all(x, "[^a-zA-Z]"," ")
  iconv(x, "latin1","UTF-8", sub=" ")
  #Creating vectorized corpus 
  corpus <- VCorpus(VectorSource(as.vector(x)))
  corpus <- corpus %>% tm_map(content_transformer(removePunctuation)) %>%
    tm_map(removeNumbers) %>% tm_map(content_transformer(tolower)) %>% tm_map(content_transformer(removeWords), stopwords("english")) %>%
    tm_map(stripWhitespace)
  return (convert.tm.to.character(corpus))
  #return (corpus) 
}

#Creating a Term Document Matrix with TF weighting
tf_extraction <- function(corpus){
  tdm <- TermDocumentMatrix(corpus,control = list(weighting = weightTf))
  tdm <- removeSparseTerms(tdm, 0.98)
  tdm_mat <- as.data.frame(as.matrix(tdm), stringsAsFactors=False)
  tdm_mat <- t(tdm_mat)
  return(tdm_mat)
}


path_to_dataset <- "C:\\Users\\calid\\Desktop\\full_dataset.csv"
main_df <- load_dataset(path_to_dataset)
main_df['preprocessed'] <- preprocess(main_df$content)
tdm_mat <- tf_extraction(preprocess(main_df$content))

#PCA
pc <- princomp(tdm_mat)
plot(pc)
pca_bow<- prcomp(tdm_mat)
pca_bow <- data.frame(
  PC1 = pca_bow$x[, 1],
  PC2 = pca_bow$x[, 2]
)
figure() %>%ly_points(pca_bow,  size =5)

#Find best no. of clusters for PCA
wss <- (nrow(pca_bow)-1)*sum(apply(pca_bow,2,var))
for (i in 1:22) wss[i] <- sum(kmeans(pca_bow,iter.max = 50L,centers=i)$withinss)
plot(1:22, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#kmeans with PCA
k2 <- kmeans(pca_bow, centers = 8, nstart = 25)
fviz_cluster(k2, pca_bow)
pca_bow['cluster_no']<-k2$cluster

#TSNE
tsne <- tsne <- Rtsne(tdm_mat, perplexity = 30, pca = TRUE, check_duplicates = FALSE)
X <- data.frame(tsne$Y)
X['URL'] <- main_df$URL
figure() %>% ly_points(x = X1, y = X2, data = X)

#TSNE Plot with ggplot
tsne_plot <- tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(tdm_mat)[1:count]) %>%
  ggplot(aes(x = V1, y = V2, label = word)) + 
  geom_text(size = 2)
tsne_plot
tsne_vals<- tsne$Y %>%as.data.frame() 

#TSNE with rbokeh
figure() %>%ly_points(x = X1, y = X2, data = X, hover = url, size =3)

#Find best no. of clusters for tsne
wss <- (nrow(tsne_vals)-1)*sum(apply(tsne_vals,2,var))
for (i in 1:20) wss[i] <- sum(kmeans(tsne_vals,iter.max = 50L,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#kmeans with TSNE
k3 <- kmeans(tsne_vals, centers = 8, nstart = 25)
fviz_cluster(k3, tsne_vals)
tsne_vals['cluster_no'] <- k3$cluster

#Grouping data by clusters
test_df_new <- main_df %>%mutate(tsne_cluster = k3$cluster) %>% group_by(tsne_cluster)
#Viewing distribution of documents in cluster
ggplot(test_df_new, aes(tsne_cluster)) +
  geom_bar(fill = "#0073C2FF") 

#split the data based on cluster
test_df_new <- test_df_new %>%group_by(tsne_cluster) 
test_df_clusters <- group_split(test_df_new)

#pca_bow : has pca values with cluster
#tsne_vals : has tsne values with clusters

#selct small set of files to test
test_df_new_1 <- test_df_new[1:50,]
test_df_new_1 <- test_df_new_1 %>% group_by(tsne_cluster) 
test_df_clusters1 <- group_split(test_df_new_1)

#perform LDA for each cluster
count = 0
terms_in_topics_in_cluster <- list()
top_5_keywords_per_topic_in_all_clusters <- list()

#LDA tuning
for (single_cluster in test_df_clusters[1]) {
  docs <- Corpus(VectorSource(single_cluster$preprocessed))
  params <- list(minDocFreq = 1,removeNumbers = TRUE,stopwords = TRUE,stemming = FALSE,weighting = weightTf) 
  dtm <- DocumentTermMatrix(docs, control = params)
  #Performing LDA tuning
  result <- FindTopicsNumber(
    dtm,
    topics = seq(from = 2, to = 15, by = 1),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = NA,
    verbose = TRUE
  )
  
  #plotting the metrics
  FindTopicsNumber_plot(result)
}

#Hash to store all lda vectors
all_ldas <- hash()

#Caluclate LDA for all clusters
for (single_cluster in test_df_clusters1[1]) {
  count = count+1
  
  docs <- Corpus(VectorSource(single_cluster$preprocessed))
  params <- list(minDocFreq = 1,removeNumbers = TRUE,stopwords = TRUE,stemming = FALSE,weighting = weightTf) 
  dtm <- DocumentTermMatrix(docs, control = params)
  freq <- colSums(as.matrix(dtm))
  print("cluster-"+as.String(single_cluster$tsne_cluster[[1]]))
  print("Total number of words"+as.String(length(freq)))
  
  # to force printing to take place during the loop
  flush.console()
  
  # Set parameters for Gibbs sampling
  burnin <- 4000
  iter <- 2000
  thin <- 500
  seed <-list(2003,5,63,100001,765)
  nstart <- 5
  best <- TRUE
  
  # Number of topics
  k <- 10
  
  # Run LDA using Gibbs sampling
  ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
  
  all_ldas[count] <- ldaOut
  # docs to topics
  ldaOut.topics <- as.matrix(topics(ldaOut))
  
  # Top 10 terms in each topic
  ldaOut.terms <- as.matrix(terms(ldaOut,10))
  
  terms_in_topics_in_cluster <- append(terms_in_topics_in_cluster, list(as.data.frame(ldaOut.terms)))
  cluster_top_50_words <- as.list(ldaOut.terms[1:5,])
  top_5_keywords_per_topic_in_all_clusters <- append(top_5_keywords_per_topic_in_all_clusters, list(cluster_top_50_words))
  
}

print(terms_in_topics_in_cluster[[1]])