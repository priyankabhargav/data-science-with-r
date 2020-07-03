library(readr)
library(caret)
library(randomForest)

# read word vectors with clusters identified
word_vec_cluster <- read_csv("/Users/nandish21/Downloads/1-Masters/2nd-Sem/DSwR/git/git_version_10/word_vec_cluster.csv")
set.seed(123)
# partition the dataset using stratified method
train.index <- createDataPartition(word_vec_cluster$cluster, p = .7, list = FALSE)
train <- word_vec_cluster[ train.index,]
test  <- word_vec_cluster[-train.index,]
# plot the distribution of clusters in Training and Test dataset
ggplot(aes(x=cluster), data=train) +
  geom_histogram(fill='dark blue') +
  ggtitle('Histogram of cluster Frequency in Training Set') +
  xlab('Cluster Number') +
  ylab('Frequency in Training Data')

ggplot(aes(x=cluster), data=test) +
  geom_histogram(fill='light blue') +
  ggtitle('Histogram of cluster Frequency in Test Set') +
  xlab('Cluster Number') +
  ylab('Frequency in Training Data')

# Train Candidate Models
mod_rf <- train(factor(cluster) ~.,
                data=train, 
                method='rf', 
                trControl=trainControl(method="cv", 
                                       number=4, 
                                       allowParallel=TRUE, 
                                       verboseIter=TRUE)) # Random Forrest

mod_tree <- train(factor(cluster) ~.,
                data=train, 
                method='rpart', 
                trControl=trainControl(method="cv", 
                                       number=4, 
                                       allowParallel=TRUE, 
                                       verboseIter=TRUE)) # Trees

mod_lda <- train(factor(cluster) ~., 
                 data=train, 
                 method='lda',
                 trControl=trainControl(method="cv", 
                                        number=4, 
                                        allowParallel=TRUE, 
                                        verboseIter=TRUE)) # Linear Discriminant Analysis

mod_nb <- train(factor(cluster) ~., 
                data=train, 
                method='nb',
                trControl=trainControl(method="cv", 
                                       number=4, 
                                       allowParallel=TRUE, 
                                       verboseIter=TRUE)) # Naive Bayes

# Predictions on test set
pred_rf <- predict(mod_rf,test[,1:101])
cm_rf <- confusionMatrix(table(pred_rf, test$cluster))

pred_tree <- predict(mod_tree,test[,1:101])
cm_tree <- confusionMatrix(table(pred_tree, test$cluster))

pred_lda <- predict(mod_lda,test[,1:101])
cm_lda <- confusionMatrix(table(pred_lda, test$cluster)) 

pred_nb <- predict(mod_nb,test[,1:101])
cm_nb <- confusionMatrix(table(pred_nb, test$cluster)) 

model_compare <- data.frame(Model = c('Random Forest',
                                      'Trees',
                                      'Lda',
                                      'Naive Bayes'),
                            Accuracy = c(cm_rf$overall[1],
                                         cm_tree$overall[1],
                                         cm_lda$overall[1],
                                         cm_nb$overall[1]))

ggplot(aes(x=Model, y=Accuracy), data=model_compare) +
  geom_bar(stat='identity', fill = 'skyblue') +
  ggtitle('Comparative Accuracy of Models on Cross-Validation Data') +
  xlab('Models') +
  ylab('Overall Accuracy')

