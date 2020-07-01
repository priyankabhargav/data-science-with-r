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
  #iconv(x, "latin1","UTF-8", sub=" ")
  #Creating vectorized corpus 
  corpus <- VCorpus(VectorSource(as.vector(x)))
  corpus <- corpus %>% tm_map(content_transformer(removePunctuation)) %>%
    tm_map(removeNumbers) %>% tm_map(content_transformer(tolower)) %>% tm_map(content_transformer(removeWords), c(custom_stp,stopwords("english"))) %>%
    tm_map(stripWhitespace)
  return (corpus) 
}

#Creating a Term Document Matrix with Tf-Idf weighting
tfidf_extraction <- function(corpus){
  params <- list(minDocFreq = 1,removeNumbers = TRUE,stopwords = TRUE,stemming = FALSE,weighting = weightTf) 
  dtm <- DocumentTermMatrix(corpus, control = params)
  dtm <- removeSparseTerms(dtm, 0.99)
  return(dtm)
}

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#Execution starts from here
path_to_dataset <- "D:\\DKE-sem\\sem2\\DWR\\project\\files\\Final_scrapped_data\\full_dswr.csv"
#Load the Dataset
main_df <- load_dataset(path_to_dataset)

main_df <- main_df[0:100,]

#Pre-process teh data and create a tdm matrix
dtm_mat <- tfidf_extraction(preprocess(main_df$content))
dtm_mat
rowTotals <- apply(dtm_mat , 1, sum) #Find the sum of words in each Document
dtm_mat  <- dtm_mat[rowTotals> 0, ]
set.seed(12345)
#Creating a sparse matrix from a dtm
sparse_matrix_dtm <-  Matrix::sparseMatrix(i=dtm_mat$i, 
                            j=dtm_mat$j, 
                            x=dtm_mat$v, 
                            dims=c(dtm_mat$nrow, dtm_mat$ncol),
                            dimnames = dtm_mat$dimnames)
# raining set is 60 percent of the records in the corpus
LDA_model <- FitLdaModel(dtm = sparse_matrix_dtm[0:60,], k = 15,
                 iterations = 200, burnin = 175)

# predict on held-out documents using gibbs sampling "fold in"
p1 <- predict(LDA_model, sparse_matrix_dtm[60:100,], method = "gibbs",iterations = 200, burnin = 175)
# predict on held-out documents using the dot product method
p2 <- predict(m, sparse_matrix_dtm[60:100,], method = "dot")
# compare the methods
barplot(rbind(p1[1,],p2[1,]), beside = TRUE, col = c("red", "blue"))

club <- data.frame(LDA_model$theta)
club
colnames(club) <- paste("T_",c(1:15),sep="")
nrow(club)
club['type'] <- rep("train",60)
club

club2 <- data.frame(p1)
colnames(club2) <- paste("T_",c(1:15),sep="")
nrow(club2)
club2['type'] <- rep("test",41)

final_c <- rbind(club,club2)
nrow(final_c)
# Plot

ggparcoord(final_c,
           columns = 1:15, groupColumn = 16,
           scale = 'uniminmax',
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot",
           alphaLines = 0.5
) + scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=13)
  )
