install.packages(c("quanteda", "stringr"))
install.packages("reticulate")
install.packages("rbokeh")


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
library(tidytext)
library(topicmodels)
library(here)
library(rjson)
library(tictoc)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)


#Python environment
path_to_python <- "C:\\Users\\calid\\AppData\\Local\\R-MINI~1\\envs\\R-RETI~1\\python.exe"
use_python(path_to_python)

#Using python libraries in R
reticulate::use_python(path_to_python)
py_install("gensim")
gensim <- import("gensim")
builtins <- import_builtins()
Word2Vec <- gensim$models$Word2Vec # Extract the Word2Vec model
doc2vec <- gensim$models$doc2vec
TaggedDoc <- gensim$models$doc2vec$TaggedDocument

#Importing data
full <- read.csv("/Users/nandish21/Downloads/1-Masters/2nd-Sem/DSwR/git/git_version_3/44000 Records/Cases_4000.csv")
custom_words <- read_excel(here("data","Custom_Stopwords.xlsx"))
custom_words = custom_words$Stopwords
print(length(full$content))

#Removing duplicate urls
new_file <- full[!duplicated(full$url), ]
print(length(new_file$url))

#Final dataset 
test_df<- new_file[!duplicated(new_file$content), ]
print(length(test_df$content))


#Function for pre-processing
perform_preprocess <- function(row_cont){
  x <- gsub("(www|http)[^[:space:]]*", " ", row_cont)
  x <- str_replace_all(x, "[[:punct:]]" , "")
  x <- str_replace_all(x, "[\t\r\n]" , " ")
  x <- str_replace_all(x, "[^[:alnum:]]", " ")

  corpus <- VCorpus(VectorSource(as.vector(x)))
  corpus <- corpus %>% tm_map(content_transformer(removePunctuation)) %>% tm_map(removeNumbers) %>% tm_map(content_transformer(tolower))%>% tm_map(content_transformer(removeWords), c(stopwords("english"),custom_words)) %>%tm_map(stripWhitespace)
  return (convert.tm.to.character(corpus))
  #return (as.String(corpus))
}

#Pre-processing the entire dataset
test_df <- test_df %>%mutate(pre_process_content = perform_preprocess(content))


#Function to perform word2vec
perform_word2Vec <- function(row_cont){
  #tokenization
  tokens <- tokenize_words(row_cont, lowercase = TRUE, stopwords = "en", strip_punct = TRUE,strip_numeric = TRUE)
  
  #Model specs
  basemodel <- Word2Vec(min_count=1L,
                        window=10L,
                        size=100L,
                        alpha=0.01, 
                        min_alpha=0.00001, 
                        workers=1)
  #Building vocabulary
  basemodel$build_vocab(sentences = tokens,  progress_per=100)
  
  #Training
  basemodel$train(
    sentences = tokens,
    epochs = basemodel$iter, 
    total_examples = basemodel$corpus_count)
  
  #Table with words
  vec_table <- basemodel$wv$syn0
  rownames(vec_table) <- basemodel$wv$index2word
  
  #Averaging word vectors in a document 
  print(colSums(vec_table)/nrow(vec_table))
  return(colSums(vec_table)/nrow(vec_table))
  
}

#Length of the dataframe
count = length(test_df$content)

#Getting word vectors
word_vec_article <- data.frame(matrix(nrow = count, ncol = 100))
rownames(word_vec_article) <- 1:count
for (i in 1:count){
  print(i)
  word_vec_article[i,]<-c(perform_word2Vec(test_df$pre_process_content[[i]]))
}

#Plotting principal components 
pc <- princomp(word_vec_article)
plot(pc)

#Storing & plotting relevant principal components
pca_test_df_num <- prcomp(word_vec_article)
pca_test_df_num <- data.frame(
  PC1 = pca_test_df_num$x[, 1],
  PC2 = pca_test_df_num$x[, 2]
)
figure() %>%ly_points(pca_test_df_num,  size =5)

#Find best no. of clusters for PCA
wss <- (nrow(pca_test_df_num)-1)*sum(apply(pca_test_df_num,2,var))
for (i in 1:100) wss[i] <- sum(kmeans(pca_test_df_num,iter.max = 50L,centers=i)$withinss)
plot(1:100, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#kmeans with PCA
k2 <- kmeans(pca_test_df_num, centers = 6, nstart = 25)
fviz_cluster(k2, pca_test_df_num)

#tsne with ggplot
tsne<- Rtsne(word_vec_article[1:count,], perplexity = 30, pca = FALSE, check_duplicates = FALSE)
tsne_plot <- tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(word_vec_article)[1:count]) %>%
  ggplot(aes(x = V1, y = V2, label = word)) + 
  geom_text(size = 2)
tsne_plot
tsne_vals<- tsne$Y %>%as.data.frame() 

#tsne with rbokeh
tsne_rbokeh <- Rtsne(word_vec_article, perplexity = 2, pca = TRUE, check_duplicates = FALSE,max_iter = 1000)
X <- data.frame(tsne$Y)
X['File'] <- test_df$file
figure() %>%ly_points(x = X1, y = X2, data = X, hover = File, size =3)

#Find best no. of clusters for tsne
wss <- (nrow(tsne_vals)-1)*sum(apply(tsne_vals,2,var))
for (i in 1:100) wss[i] <- sum(kmeans(tsne_vals,iter.max = 50L,centers=i)$withinss)
plot(1:100, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#kmeans on tsne
k2 <- kmeans(tsne_vals, centers = 8, nstart = 25, iter.max = 10000)
fviz_cluster(k2,a)
k2$cluster











# read the cluster info and plot
tsne_output <- read.csv("/Users/nandish21/Downloads/1-Masters/2nd-Sem/ATML/Project/git/git_version_3/data-science-with-r/tsne_perplex30_pcaFalse_kmeans8.csv")
head(tsne_output)
install.packages("psych")
library(psych)
pairs.panels(tsne_output[2:3],
             gap = 0,
             bg = c("red","yellow","blue","green","cyan","violet","pink","orange")[tsne_output$cluster],
             pch = 21)



#append the cluster column
test_df_new <- test_df %>%mutate(cluster = k2$cluster) %>% group_by(cluster)
abc <- test_df %>%
  group_by(cluster) %>%
  summarise(counts = n())

ggplot(test_df_new, aes(cluster)) +
  geom_bar(fill = "#0073C2FF") 

#split the data based on cluster
test_df_new <- test_df_new %>%
  group_by(cluster) 
test_df_clusters <- group_split(test_df_new)

#selct small set of files to test
test_df_new_1 <- test_df_new[1:70,]
test_df_new_1 <- test_df_new_1 %>%
  group_by(cluster) 
test_df_clusters <- group_split(test_df_new_1)

#perform LDA for each cluster
count = 0
terms_in_topics_in_cluster <- list()

for (single_cluster in test_df_clusters) {
  count = count+1
  
  docs <- Corpus(VectorSource(single_cluster$pre_process_content))
  params <- list(minDocFreq = 1,removeNumbers = TRUE,stopwords = TRUE,stemming = FALSE,weighting = weightTf) 
  dtm <- DocumentTermMatrix(docs, control = params)
  freq <- colSums(as.matrix(dtm))
  print("cluster-"+as.String(single_cluster$cluster[[1]]))
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
  
  # docs to topics
  ldaOut.topics <- as.matrix(topics(ldaOut))
  
  # Top 10 terms in each topic
  ldaOut.terms <- as.matrix(terms(ldaOut,10))
  
  terms_in_topics_in_cluster <- append(terms_in_topics_in_cluster, list(as.data.frame(ldaOut.terms)))
}

print(terms_in_topics_in_cluster)

# create the visualization(it will create for the last cluster in the above loop)
ap_topics <- tidy(ldaOut, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()





