#load(file = here("Object","best-topics","sparse_matrix_dtm_bow.Rdata")
#load(file = here("Object","best-topics","sparse_matrix_dtm_tfidf.Rdata")

Optimal_topics_tuning <- function(sparse_matrix_dtm_bow,sparse_matrix_dtm_tfidf){
  li <- seq(from = 10, to = 30, by = 3)
  p_bow_val <- c()
  p_tfidf_val <- c()
  for (i in li){
    
    ######## Modelling Using  BOW TF Feature Set ######## 
    
    # training set is 60 percent of the records in the corpus
    LDA_model_bow <- FitLdaModel(dtm = sparse_matrix_dtm_bow[1:16,], k = as.integer(i),
                                 iterations = 200, burnin = 175)
    # predict on held-out documents using gibbs sampling "fold in"
    p1_bow <- predict(LDA_model_bow, sparse_matrix_dtm_bow[17:nrow(sparse_matrix_dtm_bow),], 
                      method = "gibbs",iterations = 200, burnin = 175)
    # predict on held-out documents using the dot product method
    p2_bow <- predict(LDA_model_bow, sparse_matrix_dtm_bow[17:nrow(sparse_matrix_dtm_bow),], method = "dot")
  
    
    
    
    ######## Modelling Using BOW TF-IDF Feature Set ######## 
    
    # training set is 60 percent of the records in the corpus
    LDA_model_tfidf <- FitLdaModel(dtm = sparse_matrix_dtm_tfidf[1:16,], k = as.integer(i),
                                   iterations = 200, burnin = 175)
    # predict on held-out documents using gibbs sampling "fold in"
    p1_tfidf <- predict(LDA_model_tfidf, sparse_matrix_dtm_tfidf[17:nrow(sparse_matrix_dtm_tfidf),], 
                        method = "gibbs",iterations = 200, burnin = 175)
    # predict on held-out documents using the dot product method
    p2_tfidf <- predict(LDA_model_tfidf, sparse_matrix_dtm_tfidf[17:nrow(sparse_matrix_dtm_tfidf),], 
                        method = "dot")
    
    
    
    #Calculating Perplexity for BOW TF
    p_bow <- text2vec::perplexity(sparse_matrix_dtm_bow[1:16,], 
                                  LDA_model_bow$phi, 
                                  LDA_model_bow$theta)
    #Calculating Perplexity for BOW TF-IDF
    p_tfidf <- text2vec::perplexity(sparse_matrix_dtm_tfidf[1:16,], 
                                    LDA_model_tfidf$phi, 
                                    LDA_model_tfidf$theta)
    p_bow_val_f <- append(p_bow_val,p_bow)
    p_bow_val <- p_bow_val_f
    p_tfidf_f <- append(p_tfidf_val,p_tfidf)
    p_tfidf_val <- p_tfidf_f
  
    
  }
  return(list(p_bow_val,p_tfidf_val))
}



LDA_prediction <- function(topics_par,sparse_matrix_dtm_bow){

  #Bag of words with Term Frequency#####################
  
  # raining set is 60 percent of the records in the corpus
  LDA_model_bow <- FitLdaModel(dtm = sparse_matrix_dtm_bow[1:16,], k = topics_par,
                               iterations = 200, burnin = 175)

  # predict on held-out documents using gibbs sampling "fold in"
  p1_bow <- predict(LDA_model_bow, sparse_matrix_dtm_bow[17:nrow(sparse_matrix_dtm_bow),], 
                    method = "gibbs",iterations = 200, burnin = 175)

  # predict on held-out documents using the dot product method
  p2_bow <- predict(LDA_model_bow, sparse_matrix_dtm_bow[17:nrow(sparse_matrix_dtm_bow),], method = "dot")

  # Calculating MAximum Likelihood for evaluating the final  choosen model
  likelihood_bow <- textmineR::CalcLikelihood(sparse_matrix_dtm_bow[1:16,], 
                                              LDA_model_bow$phi, LDA_model_bow$theta)
  return(list(LDA_model_bow,p1_bow,p2_bow,likelihood_bow))

}

#Finding the optimal number of topics by running LDA and evaluating for values ranging from 10 to 30 topics
c(perplexity_bow_val, perplexity_tfidf_val) %<-%  Optimal_topics_tuning(sparse_matrix_dtm_bow,sparse_matrix_dtm_tfidf) 

#We've choosen 25 topics as per the analysis to be the optimal number of topics
topics_par <- 25

#Running LDA for Final Prediction
c(LDA_model_bow,p1_bow,p2_bow,likelihood_bow) %<-% LDA_prediction(topics_par,sparse_matrix_dtm_bow)

