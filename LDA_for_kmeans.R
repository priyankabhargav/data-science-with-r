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

#Performing LDA on whole corpus and returns all relevant dataframes
corpus_lda <- function(dtm_mat){
  documents_lda <- LDA(dtm_mat, k = 15, control = list(seed = 1234))
  
  #create a term topic dataframe
  term_topics_beta <- tidy(documents_lda, matrix = "beta")
  #finding top terms with respect to each topic
  top_terms <- term_topics_beta %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  #create a Document, topic dataframe
  docs_gamma <- tidy(documents_lda, matrix = "gamma")
  doc_topics_gamma <- docs_gamma %>% group_by(document) %>% 
    spread(topic,gamma) %>% 
    mutate(document = as.numeric(document)) %>%
    arrange(document)
  
  return (list(term_topics_beta,top_terms,doc_topics_gamma))
}
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#Execution starts from here
path_to_dataset <- "D:\\DKE-sem\\sem2\\DWR\\project\\files\\Final_scrapped_data\\full_dswr.csv"
#Load the Dataset
main_df <- load_dataset(path_to_dataset)

#main_df <- main_df[0:200,]
#Pre-process teh data and create a tdm matrix
dtm_mat <- tfidf_extraction(preprocess(main_df$content))
dtm_mat
dtm_mat
rowTotals <- apply(dtm_mat , 1, sum) #Find the sum of words in each Document
dtm_mat  <- dtm_mat[rowTotals> 0, ]

c(term_topics_beta,top_terms,doc_topics_gamma) %<-% corpus_lda(dtm_mat) 
# The above matrices retrieved can be used for visualizations
#######################################################
#########################################################
#######################################################
#######################################################
########################################################
##########################################################
############################################################



# Visualization of topics and associated dominant terms in them.
links <- data.frame(
  source = top_terms$topic,
  target = top_terms$term,
  value = top_terms$beta
)

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
                   sinksRight=FALSE,fontSize = 14,height = 900,width = 800,nodePadding = 8, fontFamily = "arial")
p
#save the widget
saveWidget(p, file=paste0("D:\\DKE-sem\\sem2\\DWR\\project\\files\\Final_scrapped_data\\outputs\\sankeyBasic1.html"))


#Reducing the dimensions via tsne
tsne <- Rtsne(doc_topics_gamma[,-1], perplexity = 1, pca = FALSE, check_duplicates = FALSE)
X <- data.frame(tsne$Y)
X
#Find best no. of clusters for PCA
wss <- (nrow(X)-1)*sum(apply(X,2,var))
for (i in 1:40) wss[i] <- sum(kmeans(X,iter.max = 50L,centers=i)$withinss)
plot(1:40, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#clustering via k means
k3 <- kmeans(X,centers = 10, nstart = 25)

sil <- silhouette(k3$cluster, dist(X))
fviz_silhouette(sil)
ggplot(X, aes(x = X1, y = X2, color = as.factor(k3$cluster))) + geom_point()

fviz_cluster(k3,X)
fviz_cluster(k3, X,ellipse.type = "norm")
fviz_cluster(k3, X,geom = "point")

df <- doc_topics_gamma

df['cluster'] <- k3$cluster
col_names <- paste('topic',seq(1:15), sep="")
colnames(df) <- c('doc',col_names,'cluster_label')

new_df <- df %>% group_by(cluster_label) %>% 
  summarise(Topic1 = sum(topic1)/n(),Topic2 = sum(topic2)/n(),Topic3 = sum(topic3)/n(),
            Topic4 = sum(topic4)/n(), Topic5 = sum(topic5)/n(), Topic6 = sum(topic6)/n(), 
            Topic7 = sum(topic7)/n(), Topic8 = sum(topic8)/n(),Topic9 = sum(topic9)/n(),
            Topic10 = sum(topic10)/n(),Topic11 = sum(topic11)/n(),Topic12 = sum(topic12)/n(),
            Topic13 = sum(topic13)/n(),Topic14 = sum(topic14)/n(),Topic15 = sum(topic15)/n())
new_df
new_df <- data.table(new_df)
new_df <- new_df[,-1]

transposed_df <- t(new_df) 
transposed_df
#c("cluster1","cluster2","cluster3","cluster4","cluster5","cluster6","cluster7","cluster8")
colnames(transposed_df) <- paste("cluster",seq(1:10),sep="")

transposed_df <- transposed_df %>% as.data.frame()
#c("Topic1","Topic2","Topic3","Topic4","Topic5","Topic6","Topic7","Topic8")
transposed_df['topics'] <- paste("Topic",seq(1:15),sep="")
transposed_df
view(transposed_df)
temp = reshape(transposed_df, direction="long", varying=1:10, sep="")
view(temp)
bwplot(cluster ~ topics | paste0("cluster ", time), data = temp,ylab = 'Gamma') 
transposed_df
# Circlualar viz for clusters and related topicsin those clusters
new_v <- transposed_df[,-11]
colnames(new_v) <- paste("Cluster",seq(1:10),sep = "")
new_df_v <- data.frame(from = rep(rownames(new_v), times = ncol(new_v)),
                       to = rep(colnames(new_v), each = nrow(new_v)),
                       value = as.vector(new_v),
                       stringsAsFactors = FALSE)
new_v <- new_v %>% as.matrix()
circos.par(canvas.ylim=c(-1.5,1.5), # edit  canvas size 
           #gap.after = gaps, # adjust gaps between regions
           #track.margin = c(0.01, 0.05), # adjust bottom and top margin
           # track.margin = c(0.01, 0.1)
           #track.height = 0.05)
)
chordDiagram(new_v,big.gap = 20,directional = 1, direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow", diffHeight = -mm_h(2),
             # plot only grid (no labels, no axis)
             #annotationTrack = c("name","grid"),
             # annotationTrack = NULL, 
             #preAllocateTracks = 1, 
             
             # adjust grid width and spacing
             #annotationTrackHeight = c(0.02, 0.01), )
)
circos.clear()
