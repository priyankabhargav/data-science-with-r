library(tm)
library(SnowballC)
library(tidyverse)
library(readxl)
library(hash)
library(stringr)
library(gsubfn)
library(textreg)

all_file_names <- list.files(path = "J:\\Sem 2\\Data Science with R\\scrapped.zip (Unzipped Files)", pattern = "*.xlsx", full.names = T)
print(all_file_names)

fil_n_count <- hash()

for (fil_names in c(all_file_names)){
  Sample_file <- read_excel(fil_names)
  colm_name <- colnames(Sample_file)
  d <- hash()
  for (names in c(colm_name)){
    a <- get_rows(Sample_file,names)
    d[names] <- a
  }
  fil_n_count[Sample_file$category[[1]]] <- d
}

print(fil_n_count)

get_rows <- function(df,colm_names){
  file_contents = df %>% select(colm_names)
  file_contents_without_na <- na.omit(file_contents)
  return (nrow(file_contents_without_na))
}


Sample_file <- read_excel("J:\\Sem 2\\Data Science with R\\scrapped.zip (Unzipped Files)\\Testing_scrapped.xlsx")
colm_name <- colnames(Sample_file)
head(Sample_file)

new_df = Sample_file %>% select(content)
head(new_df[[1]][[2]])

perform_preprocess <- function(row_cont){
  x <- gsub("(www|http)[^[:space:]]*", " ", row_cont)
  x <- str_replace_all(x, "[[:punct:]]" , "")
  x <- str_replace_all(x, "[\t\r\n]" , " ")
  corpus <- VCorpus(VectorSource(as.vector(x)))
  corpus <- corpus %>% tm_map(content_transformer(removePunctuation)) %>%
    tm_map(removeNumbers) %>% tm_map(content_transformer(tolower)) %>% tm_map(content_transformer(removeWords), stopwords("english")) %>%
    tm_map(stripWhitespace)
  return (convert.tm.to.character(corpus)) }

pre_proces_res <- Sample_file %>%
  mutate(pre_process_content = perform_preprocess(content))

pre_proces_res$pre_process_content[[35]]