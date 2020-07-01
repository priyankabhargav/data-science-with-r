library(SnowballC)
library(tidyverse)
library(readxl)
library(hash)
library(stringr)
library(gsubfn)
library(textreg)
library(wordcloud)
library(tidytext)
library(factoextra)
library(tm)
library(SnowballC)
library(tidyverse)
library(readxl)
library(hash)
library(stringr)
library(gsubfn)
library(textreg)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(widyr)
library(Rtsne)
library(ggplot2)
library(plotly)
library(rbokeh)
library(dbscan)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(mapproj)
library(purrr)
library(tm)
library(cluster)

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
  return (corpus) 
  }

#Creating a Term Document Matrix with Tf-Idf weighting
tfidf_extraction <- function(corpus){
  tdm <- TermDocumentMatrix(corpus,control = list(weighting = weightTfIdf))
  #Removing terms that do not appear in even a single document
  tdm <- removeSparseTerms(tdm, 0.99)
  #Creating a sparse term document matrix
  tdm_mat <- as.data.frame(as.matrix(tdm), stringsAsFactors=False)
  #Transposing the above matrix in order to get rows as documents and columns as terms.
  tdm_mat <- t(tdm_mat)
  return(tdm_mat)
}

#Reducing teh dimension of the tdm matrix via Tsne projection 
tsne_projection <- function(tdm_mat){
  tsne <- Rtsne(tdm_mat, perplexity = 30, pca = TRUE, check_duplicates = FALSE)
  return(tsne)
}

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#Execution starts from here
path_to_dataset <- "D:\\DKE-sem\\sem2\\DWR\\project\\files\\Final_scrapped_data\\full_dswr.csv"
#Load the Dataset
main_df <- load_dataset(path_to_dataset)
#Pre-process teh data and create a tdm matrix
tdm_mat <- tfidf_extraction(preprocess(main_df$content[c(1:10)]))
#tsne projection
tsne <- tsne_projection(tdm_mat)

#Plotting the tsne projection###############
X <- data.frame(tsne$Y)
#X['URL'] <- main_df$URL[c(1:10)]

figure() %>%
  ly_points(x = X1, y = X2,
            data = X)
#Plotting the tsne projection###############

#Load the tsne projected values to feed to k means
tsne_from_csv_df <- read.csv("D:\\DKE-sem\\sem2\\DWR\\project\\files\\Final_scrapped_data\\tfidf_tsne_pca_complete-dataset.csv")

#ct <- data.frame(X$X1, X$X2)
df_no_cluster_label <- tsne_from_csv_df[,c(-1,-4)]

#Dearest k means calculation
k2 <- kmeans(df_no_cluster_label, centers = 8, nstart = 25)

#Mutating the cluster label to teh dataframe
df_with_cluster_label <- df_no_cluster_label %>% bind_cols(cluster = k2$cluster) %>%  mutate(cluster = factor(cluster))
colnames(df_with_cluster_label) <- c("X","Y","cluster")


#Calculating the silhouette coefficient
sil <- silhouette(k2$cluster, dist(df_with_cluster_label[,-3]))
#plotting teh silhouette coefficient
fviz_silhouette(sil)

#Different visualizations for k means
fviz_cluster(k2, df_with_cluster_label[,-3],ellipse.type = "norm")
fviz_cluster(k2, df_with_cluster_label[,-3],geom = "point")
fviz_cluster(k2, df_with_cluster_label[, -3],
             palette = "Set2", ggtheme = theme_minimal())

