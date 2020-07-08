#Below are the Rdata objects that can be loaded instead of running the below script since the process
#takes time for  20k articles
#load(file = here("Object","best-topics","dtm_mat_bow_finalsub.Rdata"))
#load(file = here("Object","best-topics","excluded_rows_bow_finalsub.Rdata")
#load(file = here("Object","best-topics","dtm_mat_tfidf_finalsub.Rdata")
#load(file = here("Object","best-topics","excluded_rows_tfidf_finalsub.Rdata")
#load(file = here("Object","best-topics","sparse_matrix_dtm_bow.Rdata")
#load(file = here("Object","best-topics","sparse_matrix_dtm_tfidf.Rdata")


#Function for loading and Cleaning the data by removing any duplicate values if exists
load_dataset <- function(path_to_dataset){
  main_df <- read.csv(path_to_dataset)
  #Removing duplicate urls
  main_df <- main_df[!duplicated(main_df$url), ]
  #Removing duplicate content
  main_df <- main_df[!duplicated(main_df$content), ]
  #Removing rows having content as empty
  return (main_df)
}

#Pre-processing  each article
preprocess <- function(each_row){
  #Removing all teh url's from the content
  x <- gsub("(www|http)[^[:space:]]*", " ", each_row) 
  #Replacing the punctuations
  x <- str_replace_all(x, "[[:punct:]]" , "")
  x <- str_replace_all(x, "[\t\r\n]" , " ")
  x <- str_replace_all(x, "[^[:alnum:]]"," ")
  #replace Latin characters to UTF-8
  iconv(x, "latin1","UTF-8", sub=" ")
  #Creating vectorized corpus 
  corpus <- VCorpus(VectorSource(as.vector(x)))
  corpus <- corpus %>% tm_map(content_transformer(removePunctuation)) %>%
    tm_map(removeNumbers) %>% tm_map(content_transformer(tolower)) %>% tm_map(content_transformer(removeWords), c(custom_stp,stopwords("english"))) %>%
    tm_map(stripWhitespace)
  return (corpus) 
}

# Function to create DcoumentTermMatrix
feature_extraction <- function(corpus,weighting_scheme){
  #Creating a Document Term  Matrix with BOW TF-IDF  weighting
  if(weighting_scheme == "weightTfIdf"){
    params_tfidf <- list(minDocFreq = 1,removeNumbers = TRUE,stopwords = TRUE,stemming = FALSE,weighting = function(x)weightTfIdf(x, normalize =FALSE)) 
    dtm <- DocumentTermMatrix(corpus, control = params_tfidf)
    #Find the sum of words in each Document
    rowTotals <- apply(dtm, 1, sum) 
    #Check and save all excluded records
    excluded_rows <- rownames(dtm[rowTotals <= 0,])
    dtm <- dtm[rowTotals> 0, ]
    return(list(dtm,excluded_rows))
  }
  #Creating a Document Term  Matrix with BOW TF weighting
  else{
    params_bow <- list(minDocFreq = 1,removeNumbers = TRUE,stopwords = TRUE,stemming = FALSE,weighting = weightTf)
    dtm <- DocumentTermMatrix(corpus, control = params_bow)
    dtm <- removeSparseTerms(dtm, 0.99)
    #Find the sum of words in each Document
    rowTotals <- apply(dtm, 1, sum)
    #Check and save all excluded records
    excluded_rows <- rownames(dtm[rowTotals <= 0,])
    dtm <- dtm[rowTotals> 0, ]
    return(list(dtm,excluded_rows))
  }
  
  
}

#Path to the main dataset having around 20k news articles
path_to_dataset <- here("docs","20000_full_dataset.csv")
#Loading a list of custom stopwords that we intend to remove from the Corpus


custom_stp <- read_excel(here("docs","Custom_Stopwords.xlsx"))
custom_stp <- custom_stp$Stopwords


#Loading the complete Dataset
main_df <- load_dataset(path_to_dataset)
main_df <- main_df[0:20,]
#Re-arranging the index of the dataframe in ascending order
row.names(main_df) <- NULL

#Retrieving BOW with TF along with excluded rows
c(dtm_mat_bow,excluded_rows_bow) %<-% feature_extraction(preprocess(main_df$content),"weightTf")


#Retrieving BOW with TF-IDF along with excluded rows
c(dtm_mat_tfidf,excluded_rows_tfidf) %<-% feature_extraction(preprocess(main_df$content),"weightTfIdf")

#Creating Sparse matrix for BOW using TF
sparse_matrix_dtm_bow <-  Matrix::sparseMatrix(i=dtm_mat_bow$i,
                                               j=dtm_mat_bow$j,
                                               x=dtm_mat_bow$v,
                                               dims=c(dtm_mat_bow$nrow, dtm_mat_bow$ncol),
                                               dimnames = dtm_mat_bow$dimnames)
#Creating a sparse matrix from a dtm for tfidf
sparse_matrix_dtm_tfidf <-  Matrix::sparseMatrix(i=dtm_mat_tfidf$i,
                                                 j=dtm_mat_tfidf$j,
                                                 x=dtm_mat_tfidf$v,
                                                 dims=c(dtm_mat_tfidf$nrow, dtm_mat_tfidf$ncol),
                                                 dimnames = dtm_mat_tfidf$dimnames)

