# load(file = "D:\\DKE-sem\\sem2\\DWR\\project\\github-rep-test\\data-science-with-r\\Object_files\\LDA-kmeans\\documents_lda_25topics.Rdata")
# load(file = "D:\\DKE-sem\\sem2\\DWR\\project\\github-rep-test\\data-science-with-r\\Object_files\\LDA-kmeans\\doc_topics_gamma_final25topics.Rdata")
# load(file = "D:\\DKE-sem\\sem2\\DWR\\project\\github-rep-test\\data-science-with-r\\Object_files\\LDA-kmeans\\term_topics_beta_final25topics.Rdata")
# load(file = "D:\\DKE-sem\\sem2\\DWR\\project\\github-rep-test\\data-science-with-r\\Object_files\\LDA-kmeans\\top_terms_final25topics.Rdata")



#Performing LDA on whole corpus and returns all relevant dataframes
corpus_lda <- function(dtm_mat){
  
  #Performing LDA using number of topics as 25
  documents_lda <- LDA(dtm_mat, k = 25, control = list(seed = 1234))
  
  #create a term topic dataframe
  term_topics_beta <- tibble::as_tibble(tidy(documents_lda, matrix = "beta"))
  
  #finding top terms 5 with respect to each topic
  top_terms <- term_topics_beta %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  #create a Document and topic dataframe
  docs_gamma <- tibble::as_tibble(tidy(documents_lda, matrix = "gamma"))
  doc_topics_gamma <- docs_gamma %>% group_by(document) %>% 
    spread(topic,gamma) %>% 
    mutate(document = as.numeric(document)) %>%
    arrange(document)
  
  return (list(term_topics_beta,top_terms,doc_topics_gamma,documents_lda))

  

}


c(term_topics_beta,top_terms,doc_topics_gamma,documents_lda) %<-% corpus_lda(dtm_mat_bow) 
# The above matrices retrieved can be used for visualizations







#Reducing the dimensions via tsne and Clustering via K means
##############################################################################################################
tsne <- Rtsne(doc_topics_gamma[,-1], perplexity = 30, pca = FALSE, check_duplicates = FALSE)
X <- data.frame(tsne$Y)


#Find best no. of clusters for PCA which will have Topics as subclusters under them
wss <- (nrow(X)-1)*sum(apply(X,2,var))
for (i in 1:100) wss[i] <- sum(kmeans(X,iter.max = 500000L,centers=i)$withinss)
plot(1:100, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


#clustering via k means after the optimal number of clusters came out to be 8.
k3 <- kmeans(X,centers = 8, nstart = 5,iter.max = 100000L)

#Evaluating via silhouette coefficient for 8 clusters
sil <- silhouette(k3$cluster, dist(X))

#PLotting teh silhouette graph for 8 clusters
fviz_silhouette(sil)


#Plotting teh 8 clusters
fviz_cluster(k3, X,geom = "point")





# Reshaping and constructing teh temp dataframe for plotting Topic distributing in each of the 8 clusters.
##############################################################################################################
# Removing the first column and re-structing the dataframe
df <- doc_topics_gamma[,-1]
#appending the cluster label and changing the column names
df['cluster'] <- k3$cluster
col_names <- paste('t',seq(1:25), sep="")
colnames(df) <- c(col_names,'cluster_label')
#Grouping all the topics in the clusters and averaging the probability values.
new_df <- df %>% group_by(cluster_label) %>% 
  summarise(T1 = sum(t1)/n(),T2 = sum(t2)/n(),T3 = sum(t3)/n(),
            T4 = sum(t4)/n(), T5 = sum(t5)/n(), T6 = sum(t6)/n(), 
            T7 = sum(t7)/n(), T8 = sum(t8)/n(),T9 = sum(t9)/n(),
            T10 = sum(t10)/n(),T11 = sum(t11)/n(),T12 = sum(t12)/n(),
            T13 = sum(t13)/n(),T14 = sum(t14)/n(),T15 = sum(t15)/n(),
            T16 = sum(t16)/n(),T17 = sum(t17)/n(),T18 = sum(t18)/n(),
            T19 = sum(t19)/n(),T20 = sum(t20)/n(),T21 = sum(t21)/n(),
            T22 = sum(t22)/n(),T23 = sum(t23)/n(),T24 = sum(t24)/n(),
            T25 = sum(t25)/n())

new_df <- data.table(new_df)
new_df <- new_df[,-1]
transposed_df <- t(new_df) 
#Transformations on the temp dataframe which will be used for plotting Topic Distribution in each cluster
colnames(transposed_df) <- paste("cluster",seq(1:8),sep="")

transposed_df <- transposed_df %>% as.data.frame()
transposed_df['topics'] <- paste("T_",seq(1:25),sep="")

temp <- reshape(transposed_df, direction="long", varying=1:8, sep="")
temp <- arrange(temp, topics)
temp$topics <- as.factor(temp$topics)
temp$time <- factor(temp$time)
colnames(temp) <- c("Topic_Number","Cluster_Number","Topic_Probability","id")
bp <- ggplot(temp, aes(x= Topic_Number,y=Topic_Probability, group = 1)) + 
  geom_line(color = "steelblue",size = 2) + geom_point(size = 2) +
  labs(title = "Topic Distribution in each Cluster",
       y = "Average Probability", x = "Topic Number")
bp +facet_grid(Cluster_Number ~ .)




#Circular viz for clusters and association of topics in those clusters
##############################################################################################################
#Removing the topic label
new_v <- transposed_df[,-9] 
#Changing the column names 
colnames(new_v) <- paste("Cluster",seq(1:8),sep = "")
tryCatch({
  #Re-structing the Data-Frame
  new_df_v <- data.frame(from = rep(rownames(new_v), times = ncol(new_v)),
                         to = rep(colnames(new_v), each = nrow(new_v)),
                         value = as.vector(new_v),
                         stringsAsFactors = FALSE)},warning=function(w){print("")} )

#Changing the dataframe to matrix
new_v <- new_v %>% as.matrix()
#Chord Diagram for Topics and Clusters
chordDiagram(new_v,big.gap = 10,directional = 1, direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow", diffHeight = -mm_h(1),
             grid.col = c("violet", "blue4", "blue","green", "yellow", 
                          "tomato", "red","cyan4","deeppink","cyan3","chocolate4",
                          "darkslategrey","darksalmon","chartreuse","darkorchid2","deepskyblue1",
                          "lightcoral", "palegreen4", "paleturquoise2","palevioletred", "peru", 
                          "pink4", "purple2","sienna1","skyblue2","seagreen2","rosybrown",
                          "plum3","slateblue2","orange3","darkgoldenrod2","salmon2",
                          "pink2")
        )
circos.clear()




# Visualization of topics and associated dominant terms in them with Sankey network
##############################################################################################################
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
sankey_plot <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
                   sinksRight=FALSE,fontSize = 16,height = 1400,width = 1200,
                   nodePadding = 8, fontFamily = "arial",unit = "Letter(s)")
#Plot teh sankey network
sankey_plot
