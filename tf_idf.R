
library(tm)
library(SnowballC)
library(tidyverse)
library(readxl)
library(hash)
library(stringr)
library(gsubfn)
library(textreg)
library(wordcloud)

# all_file_names <- list.files(path = "J:\\Sem 2\\Data Science with R\\scrapped.zip (Unzipped Files)", pattern = "*.xlsx", full.names = T)
# print(all_file_names)
# 
# fil_n_count <- hash()
# 
# for (fil_names in c(all_file_names)){
#   Sample_file <- read_excel(fil_names)
#   colm_name <- colnames(Sample_file)
#   d <- hash()
#   for (names in c(colm_name)){
#     a <- get_rows(Sample_file,names)
#     d[names] <- a
#   }
#   fil_n_count[Sample_file$category[[1]]] <- d
# }
# 
# print(fil_n_count)
# 
# get_rows <- function(df,colm_names){
#   file_contents = df %>% select(colm_names)
#   file_contents_without_na <- na.omit(file_contents)
#   return (nrow(file_contents_without_na))
# }


Sample_file <- read_excel("D:\\DKE-sem\\sem2\\DWR\\project\\files\\Final_scrapped_data\\panic_test.xlsx")
colm_name <- colnames(Sample_file)
head(Sample_file)

new_df = Sample_file %>% filter(!is.na(content)) %>% select(content)
head(new_df[[1]][[2]])

perform_preprocess <- function(row_cont){
  x <- gsub("(www|http)[^[:space:]]*", " ", row_cont)
  x <- str_replace_all(x, "[[:punct:]]" , "")
  x <- str_replace_all(x, "[\t\r\n]" , " ")
  corpus <- VCorpus(VectorSource(as.vector(x)))
  corpus <- corpus %>% tm_map(content_transformer(removePunctuation)) %>%
    tm_map(removeNumbers) %>% tm_map(content_transformer(tolower)) %>% tm_map(content_transformer(removeWords), stopwords("english")) %>%
    tm_map(stripWhitespace)
  return (corpus) }

pre_proces_res <- Sample_file %>%
  mutate(pre_process_content = convert.tm.to.character(perform_preprocess(content)))

pre_proces_res$pre_process_content[[4]]

tfidf_mat <- function( my_corp){
  
  tfidf <- DocumentTermMatrix(my_corp,control = list(weighting = weightTfIdf))
  tfidf <- removeSparseTerms(tfidf, 0.95)
  return(tfidf)
}

mat_tfidf <- tfidf_mat(perform_preprocess(new_df$content))

inspect(mat_tfidf[1,1:20])


freq = data.frame(sort(colSums(as.matrix(mat_tfidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))
