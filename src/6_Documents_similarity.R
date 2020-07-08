#Block to Calculate the similarity
doc_sim_df<- bow_test_train_model1
bow_model <- bow_test_train_model1
req_model <- bow_model
train_model <- req_model %>% filter(req_model$type=="train")

test_model <- req_model %>% filter(req_model$type=="test")

#slicing only 10 records from the test set
test_model <- test_model[1:10,]
x <- rbind(train_model,test_model)

#Using Jensen Shannon distance to compute the similarity
dist_val <- JSD(as.matrix(x))

x1 <- x[16001:16010,]

#To retrieve top 10 similar docs from the training set
test_rows <- dist_val[16001:16010,1:16000]

df_test_rows <- data.frame(test_rows)

foo <- function(x, names) {
  ord <- order(x)[1:10]
  names[ord]
}
nams <- colnames(df_test_rows)
top_docs_matrix <- t(apply(df_test_rows, 1, foo, names = nams))
#Here we'll calculated the top 10 documents for each of the 10 test records
top_docs_matrix <- cbind(top_docs_matrix,x1)

bow_model1 <- bow_model
#Lookup table for Mapping the id of each document to it's corresponding title name
bow_model1 <- bow_model1[,3:27] 

colnames(bow_model1) <- top$Final
bow_model1$best_top <- colnames(bow_model1)[apply(bow_model1,1,which.max)]
bow_model1$ID <- seq.int(nrow(bow_model1))
# Remove Ascii characters in the title
lookup_tab <- bow_test_train_model1
perform_preprocess <- function(row_cont){
  x <- gsub('[^\x20-\x7E]', '', row_cont)
  return (x)
}

# Loading topic names
topic_names <- read_excel(here("docs","topics.xlsx"))

lookup_tab <- as_tibble(lookup_tab) %>% mutate(doc_num = paste('v',row_number(),sep=""))
lookup_tab <- lookup_tab %>%mutate(pre_process_content = perform_preprocess(title))
lookup_tab$pre_process_content
lookup_tab <- lookup_tab[-c(1,2,26,28,29)]
req_model <- top_docs_matrix[-c(11:40)]
req_model <- req_model[-12]
req_model1 <- req_model[-11]
req_model2 <- req_model[11]
new <- req_model1
new[] <- lookup_tab$pre_process_content[match(unlist(req_model1), lookup_tab$doc_num)]
req_model <- cbind(new,req_model2)


probs_model <- top_docs_matrix[-c(11:40)]
probs_model <- probs_model[-12]
probs_model <- as_tibble(t(probs_model))

probs_model <- rbind(probs_model,colnames(probs_model))
probs_model <- probs_model[-11, ]

for (i in names(probs_model)){
  vals = probs_model[,i]
  #test_files[i] <- as_tibble(merge(vals,lookup_tab,by.x = i,by.y = "doc_num"))
  vals1 <- vals[-11,]
  vals2 <- vals[11,]
  df1 <- merge(vals1,lookup_tab,by.x = i,by.y = "doc_num")
  df2 <- merge(vals2,lookup_tab,by.x = i,by.y = "doc_num")
  df <- rbind(df1,df2)
  df <- df[-c(27,28)]
}
df <- df[-1]

#Generating Graphs for explaining the similarity reason behind similar documents,
matplot(t(df), type = 'b', pch=18)
legend('topright', rownames(df),col = seq_along(df), fill = seq_along(df))
axis(1, at = seq_along(df), labels = colnames(df))


test_df$ID <- seq.int(nrow(test_df))
all_data <- inner_join(test_df1,bow_model1,by="ID")
