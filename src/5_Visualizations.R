

#Plotting Parallel Co-ordinates plot for LDA prediction.....BOw Gibbs sampling
##############################################################################################################
bow_theta_model1 <- data.frame(LDA_model_bow$theta)
colnames(bow_theta_model1) <- paste("T_",c(1:25),sep="")
bow_theta_model1['type'] <- rep("train",16000)

#load(file = here("Object","LDA_predict","p1_bow25.Rdata"))
bow_test_predictions <- data.frame(p1_bow)
colnames(bow_test_predictions) <- paste("T_",c(1:25),sep="")
#2769

bow_test_predictions['type'] <- rep("test",nrow(p1_bow))
bow_test_train_model1 <- rbind(bow_theta_model1,bow_test_predictions)
ggparcoord(bow_test_train_model1,
           columns = 1:25, groupColumn = 26,
           scale = 'uniminmax',
           showPoints = TRUE,
           title = "Parallel Coordinate Plot of LDA with Gibb's Sampling,BOW as Feature Extraction method.",
           alphaLines = 0.1
) + scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(plot.title = element_text(size=8))




#Plotting Parallel Co-ordinates plot for LDA prediction.....Dot Product
##############################################################################################################
bow_theta_model2 <- data.frame(LDA_model_bow$theta)

colnames(bow_theta_model2) <- paste("T_",c(1:25),sep="")
bow_theta_model2['type'] <- rep("train",16000)

#load(file = "Object","LDA_predict","p2_bow25.Rdata")
bow_test_predictions2 <- data.frame(p2_bow)
colnames(bow_test_predictions2) <- paste("T_",c(1:25),sep="")
bow_test_predictions2['type'] <- rep("test",nrow(p2_bow))
bow_test_train_model2 <- rbind(bow_theta_model2,bow_test_predictions2)
ggparcoord(bow_test_train_model2,
           columns = 1:25, groupColumn = 26,
           scale = 'uniminmax',
           showPoints = TRUE,
           title = "Parallel Coordinate Plot of LDA with Dot Product method,BOW as Feature Extraction method.",
           alphaLines = 0.1
) + scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(plot.title = element_text(size=8))


#R bokeh plot for documents with similar topics
##############################################################################################################
#load(file = "Object","LDA-sim",sparse_matrix_dtm_bow_docsim.Rdata")
#load(file = "Object","LDA-sim","p2_bow_docsim.Rdata")
#load(file = "Object","LDA-sim","p1_bow_docsim.Rdata")
#load(file = "Object","LDA-sim","LDA_model_bow_docsim.Rdata")
#load(file = "Object","LDA-sim","excluded_rows_bow_dcosim.Rdata")
#load(file = "Object","LDA-sim","doc_sim_df.Rdata")

bow_theta_model1_rbokeh <- data.frame(LDA_model_bow$theta)
bow_theta_model1_rbokeh <- data.table(bow_theta_model1_rbokeh)
bow_theta_model1_rbokeh[, col_max := colnames(.SD)[max.col(.SD, ties.method = "first")]]
colnames(bow_theta_model1_rbokeh) <- c(paste("T_",c(1:25),sep=""),"col_max")
bow_theta_model1_rbokeh <- data.frame(bow_theta_model1_rbokeh)
bow_theta_model1_rbokeh['type'] <- rep("train",16000)
max_topic <- p1_bow
max_topic <- data.table(max_topic)
max_topic[, col_max := colnames(.SD)[max.col(.SD, ties.method = "first")]]
bow_test_predictions <- data.frame(max_topic)
colnames(bow_test_predictions) <- c(paste("T_",c(1:25),sep=""),"col_max")
bow_test_predictions['type'] <- rep("test",nrow(p1_bow))
bow_test_train_model1 <- rbind(bow_theta_model1_rbokeh,bow_test_predictions)

a <- c(3572,8466,10314,16655,18651)
#load(file = "Object","LDA-sim","main.Rdata")
rep_df <- main_df
#Removing above Null records from the main dataset
if (length(excluded_rows_bow) !=0){
  rep_df <- rep_df[-as.numeric(c(excluded_rows_bow)),]
  row.names(rep_df) <- NULL
}

bow_test_train_model1 <- cbind(bow_test_train_model1,rep_df$url,rep_df$title, rep_df$keywords)
colnames(bow_test_train_model1) <- c(paste("Topic",c(1:25),sep=" "),'Dominant_topic','type','url','title','keywords')
tsne <- Rtsne(bow_test_train_model1[,1:25], perplexity = 2, pca = FALSE, check_duplicates = FALSE)
df_tsne_values <- tsne$Y
colnames(df_tsne_values) <- c("X1","X2")
bow_test_train_model1 <- cbind(df_tsne_values,bow_test_train_model1)
#load(file = "Object_files","LDA-sim","doc_sim_df.Rdata")

#Grouping via Topic names
figure(title = "Rbokeh plot representing Documents and topics", width = 1200, height = 600) %>%
  
  ly_points(x = X1, y = X2,data = bow_test_train_model1,color = type,
            hover=c(Dominant_topic,title,url,keywords),size = 3
  ) %>%
  
  set_palette(discrete_color = pal_color(c( "red", "skyblue")))


#Grouping via Topics and plotting the dominant topic with respect to each other.
figure(title = "Rbokeh plot representing Documents and topics", width = 1200, height = 600) %>%
  
  ly_points(x = X1, y = X2,data = bow_test_train_model1,color = Dominant_topic,
            hover=c(Dominant_topic,title,url),size = 5) 
  

