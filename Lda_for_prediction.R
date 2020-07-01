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
library(MASS)
library(quanteda)
library(tokenizers)
library(reticulate)
library(data.table)
library(textreg)
library(caret)  
library(topicmodels)
library(here)
library(rjson)
library(tictoc)
library(readr)
library(LDAvis)
library(servr)
library(ldatuning)
library(reshape2)
library(lattice)
library(zeallot)
library(htmlwidgets)
library(networkD3)
library(circlize)
library(networkD3)
library(textmineR)
library(topicmodels)
library(GGally)
library(viridis)
library(hrbrthemes)
library(textmineR)
custom_stp <- read_excel("D:\\DKE-sem\\sem2\\DWR\\project\\files\\Final_scrapped_data\\Custom_Stopwords.xlsx")
custom_stp <- custom_stp$Stopwords
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
  x <- str_replace_all(x, "[^[:alnum:]]"," ")
  iconv(x, "latin1","UTF-8", sub=" ")
  #Creating vectorized corpus 
  corpus <- VCorpus(VectorSource(as.vector(x)))
  corpus <- corpus %>% tm_map(content_transformer(removePunctuation)) %>%
    tm_map(removeNumbers) %>% tm_map(content_transformer(tolower)) %>% tm_map(content_transformer(removeWords), c(custom_stp,stopwords("english"))) %>%
    tm_map(stripWhitespace)
  return (corpus) 
}

#Creating a Term Document Matrix with BOW weighting
feature_extraction <- function(corpus,weighting_scheme){
  if(weighting_scheme == "weightTfIdf"){
    params_tfidf <- list(minDocFreq = 1,removeNumbers = TRUE,stopwords = TRUE,stemming = FALSE,weighting = function(x)weightTfIdf(x, normalize =FALSE)) 
    dtm <- DocumentTermMatrix(corpus, control = params_tfidf)
    dtm <- removeSparseTerms(dtm, 0.99)
    rowTotals <- apply(dtm, 1, sum) #Find the sum of words in each Document
    dtm  <- dtm[rowTotals> 0, ]
    return(dtm)
  }
  else{
    params_bow <- list(minDocFreq = 1,removeNumbers = TRUE,stopwords = TRUE,stemming = FALSE,weighting = weightTf)
    dtm <- DocumentTermMatrix(corpus, control = params_bow)
    dtm <- removeSparseTerms(dtm, 0.99)
    rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
    dtm <- dtm[rowTotals> 0, ]
    return(dtm)
  }
  
  
}

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#Execution starts from here
path_to_dataset <- "D:\\DKE-sem\\sem2\\DWR\\project\\files\\Final_scrapped_data\\full_dswr.csv"
#Load the Dataset
main_df <- load_dataset(path_to_dataset)

#main_df <- main_df[0:3,]

#Pre-process teh data and create a dtm matrix with BOW
dtm_mat_bow <- feature_extraction(preprocess(main_df$content),"weightTf")
dtm_mat_bow
#Pre-process teh data and create a dtm matrix with tfidf
dtm_mat_tfidf <- feature_extraction(preprocess(main_df$content),"weightTfIdf")
dtm_mat_tfidf


set.seed(12345)
######## Using LDA, Predicting  Probability Distribution with BOW as FE technique
#Creating a sparse matrix from a dtm
sparse_matrix_dtm_bow <-  Matrix::sparseMatrix(i=dtm_mat_bow$i, 
                            j=dtm_mat_bow$j, 
                            x=dtm_mat_bow$v, 
                            dims=c(dtm_mat_bow$nrow, dtm_mat_bow$ncol),
                            dimnames = dtm_mat_bow$dimnames)
# raining set is 60 percent of the records in the corpus
LDA_model_bow <- FitLdaModel(dtm = sparse_matrix_dtm_bow[1:21700,], k = 15,
                 iterations = 200, burnin = 175)

# predict on held-out documents using gibbs sampling "fold in"
p1_bow <- predict(LDA_model_bow, sparse_matrix_dtm_bow[21701:nrow(sparse_matrix_dtm_bow),], method = "gibbs",iterations = 200, burnin = 175)
# predict on held-out documents using the dot product method
p2_bow <- predict(LDA_model_bow, sparse_matrix_dtm_bow[21701:nrow(sparse_matrix_dtm_bow),], method = "dot")
# compare the methods
#barplot(rbind(p1_bow[1,],p2_bow[1,]), beside = TRUE, col = c("red", "blue"))
write.csv(p1_bow,"D:\dwr\p1_bow.csv")
write.csv(p2_bow,"D:\dwr\p2_bow.csv")
write.csv(LDA_model_bow$theta,"D:\dwr\p2_bow.csv")


######## Using LDA, Predicting  Probability Distribution with TFIDF as FE technique
set.seed(12345)
sparse_matrix_dtm_tfidf <-  Matrix::sparseMatrix(i=dtm_mat_tfidf$i,
                                               j=dtm_mat_tfidf$j,
                                               x=dtm_mat_tfidf$v,
                                               dims=c(dtm_mat_tfidf$nrow, dtm_mat_tfidf$ncol),
                                               dimnames = dtm_mat_tfidf$dimnames)
# raining set is 60 percent of the records in the corpus
LDA_model_tfidf <- FitLdaModel(dtm = sparse_matrix_dtm_tfidf[1:21700,], k = 15,
                         iterations = 200, burnin = 175)

# predict on held-out documents using gibbs sampling "fold in"
p1_tfidf <- predict(LDA_model_tfidf, sparse_matrix_dtm_tfidf[21701:nrow(sparse_matrix_dtm_tfidf),], method = "gibbs",iterations = 200, burnin = 175)
# predict on held-out documents using the dot product method
p2_tfidf <- predict(LDA_model_tfidf, sparse_matrix_dtm_tfidf[21701:nrow(sparse_matrix_dtm_tfidf),], method = "dot")
# compare the methods
#barplot(rbind(p1_tfidf[1,],p2_tfidf[1,]), beside = TRUE, col = c("red", "blue"))

write.csv(p1_tfidf,"D:\dwr\p1_bow.csv")
write.csv(p2_tfidf,"D:\dwr\p2_bow.csv")
write.csv(LDA_model_tfidf$theta,"D:\dwr\p2_bow.csv")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #Visualization not required for now
# bow_theta_model1 <- data.frame(LDA_model_bow$theta)
# colnames(bow_theta_model1) <- paste("T_",c(1:15),sep="")
# bow_theta_model1['type'] <- rep("train",60)
# bow_theta_model1
# 
# bow_test_predictions <- data.frame(p1_bow)
# colnames(bow_test_predictions) <- paste("T_",c(1:15),sep="")
# bow_test_predictions['type'] <- rep("test",41)
# 
# bow_test_train_model1 <- rbind(bow_theta_model1,bow_test_predictions)
# #Plot
# 
# ggparcoord(bow_test_train_model1,
#            columns = 1:15, groupColumn = 16,
#            scale = 'uniminmax',
#            showPoints = TRUE,
#            title = "Parallel Coordinate Plot For Model 1 with Bow as FE",
#            alphaLines = 0.5
# ) + scale_color_viridis(discrete=TRUE) +
#   theme_ipsum()+
#   theme(
#     plot.title = element_text(size=13))
# 
# 
# 
# 
# 
# 
# 
# 
