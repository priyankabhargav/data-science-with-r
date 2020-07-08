library(shiny)
library(LDAvis)
library(topicmodels)
library(Rtsne)
library(tsne)
library(tidytext)
library(dplyr)
library(ggplot2)
library(reshape2)
library(rbokeh)

load("www/documents_lda_25topics.Rdata")
load("www/post.Rdata")
# load("req_model.Rdata")
load("www/top_10_model.Rdata")

load("www/doc_sim_df.Rdata")



ldaOut <- documents_lda
svd_tsne <- function(ldaOut) tsne(svd(ldaOut)$u)

post <- topicmodels::posterior(ldaOut)
if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
mat <- ldaOut@wordassignments


#topic terms
ap_topics <- tidy(ldaOut, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(n = 5) %>%
  ungroup() %>%
  arrange(topic, -beta)

print("printing graphs")
dev.new(width=5, height=4)
plots <- ap_top_terms %>%
  mutate(term = reorder_within(term, beta,topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 5, scales = "free_y") + coord_flip() + scale_x_reordered()



topic_rBokeh <- figure(title = "Rbokeh plot representing Documents and topics", width = 1050, height = 800) %>%
  ly_points(x = X1, y = X2,data = bow_test_train_model1,color = topic_highest_prob,
            hover=c(topic_highest_prob,title,url),size = 3
  ) %>%
  theme_legend(label_width = 5)
# topic_rBokeh.add_layout(Legend(), 'right')


Articles <- req_model$title


server <- function(input, output, session) {
  
  getProPage<-function() {
    return(includeHTML("pro_pro.html"))
  }
  getreferencehtml<-function() {
    return(includeHTML("references.html"))
  }
  getrelatedworkhtml<-function() {
    return(includeHTML("related_work.html"))
  }
  getobjectivehtml<-function() {
    return(includeHTML("objectives.html"))
  }
  getdatahtml<-function() {
    return(includeHTML("Dataset.html"))
  }
  getexplohtml<-function() {
    return(includeHTML("Exploratory Analysis.html"))
  }
  getfinalhtml<-function() {
    return(includeHTML("Final Analysis.html"))
  }
 
  
  output$proPage<-renderUI({getProPage()})
  output$referencePage<-renderUI({getreferencehtml()})
  output$relatedPage<-renderUI({getrelatedworkhtml()})
  output$objectivePage<-renderUI({getobjectivehtml()})
  output$dataPage<-renderUI({getdatahtml()})
  output$exploPage<-renderUI({getexplohtml()})
  output$finalPage<-renderUI({getfinalhtml()})
  
  output$myChart <- renderVis({
    if(!is.null(input$nTerms)){
      with(ldaOut,
           LDAvis::createJSON(
             phi = post[["terms"]], 
             theta = post[["topics"]],
             vocab = colnames(post[["terms"]]),
             doc.length = slam::row_sums(mat, na.rm = TRUE),
             mds.method = svd_tsne,
             term.frequency = slam::col_sums(mat, na.rm = TRUE),
             R = input$nTerms))
    }
  })
  

  output$topicTermsPlot <- renderPlot(
    {
      ap_top_terms <- ap_topics %>%
        group_by(topic) %>%
        top_n(n = input$nTopicTerms) %>%
        ungroup() %>%
        arrange(topic, -beta)
      plot( ap_top_terms %>%
              mutate(term = reorder_within(term, beta,topic)) %>%
              ggplot(aes(term, beta, fill = factor(topic))) +
              geom_col(show.legend = FALSE) +
              facet_wrap(~ topic, ncol = 5, scales = "free_y") + coord_flip() + scale_x_reordered()
      )
    }
  )
  
  output$rbokeh <- renderRbokeh({
    topic_rBokeh
    
  })
  
 
  updateSelectizeInput(session, 'articleInput',
                       choices = Articles,
                       server = TRUE
  )
  
  output$similar_articles <- renderUI({
    HTML(paste("", 
               select(req_model %>% filter(title == input$articleInput) , -11),
               sep ="<br/><br/>"))
  })
  
  output$image <- renderUI({
    list(src="github-mark.png")
  }
  )
  
  output$tb <- renderUI({
    # if(input$articleInput=="Any decision on extending coronavirus lockdown must be based on extreme circumspection, realistic cost-benefit analysis") Leg<-"prediction/plot v16001 .jpeg"
    # if(input$articleInput=="Bay City added to drive-thru testing sites") Leg<-"prediction/plot v16002 .jpeg"
    # if(input$articleInput=="Coronavirus India Highlights (April 13): 9,352 COVID-19 cases in India, 324 deaths") Leg<-"prediction/plot v16003 .jpeg"
    # if(input$articleInput=="'We have thrown 15 years of institutional learning out the window': Leaked emails show top public health experts raised alarm about the Trump administration's botched coronavirus response") Leg<-"prediction/plot v16004 .jpeg"
    # if(input$articleInput=="A 'sombre day' as more than 10,000 people die in hospital after testing positive for coronavirus") Leg<-"prediction/plot v16005 .jpeg"
    # if(input$articleInput=="A 'sombre day' as more than 10,000 people die in hospital after testing positive for coronavirus") Leg<-"prediction/plot v16006 .jpeg"
    # if(input$articleInput=="The latest numbers on COVID-19 in Canada") Leg<-"prediction/plot v16007 .jpeg"
    # if(input$articleInput=="PM Modi to address the nation tomorrow at 10 am") Leg<-"prediction/plot v16008 .jpeg"
    # if(input$articleInput=="Trump sent Arizona a fraction of the ventilators it sought. Republicans still framed it as a big win.") Leg<-"prediction/plot v16009 .jpeg"
    # if(input$articleInput=="Switzerland closes its schools to slow virus spread") Leg<-"prediction/plot v16010 .jpeg"
    # 
    Leg<-"prediction/plot v16001 .jpeg"
    
    if(grepl("Any decision on extending ", input$articleInput)) Leg<-"prediction/plot v16001 .jpeg"
    if(grepl("Bay City added to drive-",input$articleInput)) Leg<-"prediction/plot v16002 .jpeg"
    if(grepl("Coronavirus India Highli",input$articleInput)) Leg<-"prediction/plot v16003 .jpeg"
    if(grepl("'We have thrown 15 years ",input$articleInput)) Leg<-"prediction/plot v16004 .jpeg"
    if(grepl("A 'sombre day' as more t",input$articleInput)) Leg<-"prediction/plot v16005 .jpeg"
    if(grepl("A 'sombre day' as more ",input$articleInput)) Leg<-"prediction/plot v16006 .jpeg"
    if(grepl("The latest numbers o",input$articleInput)) Leg<-"prediction/plot v16007 .jpeg"
    if(grepl("PM Modi to address the ",input$articleInput)) Leg<-"prediction/plot v16008 .jpeg"
    if(grepl("Trump sent Arizona a ",input$articleInput)) Leg<-"prediction/plot v16009 .jpeg"
    if(grepl("Switzerland closes its ",input$articleInput)) Leg<-"prediction/plot v16010 .jpeg"
    
    
    
    tags$img(src=Leg, width ="750", height="600")
  })

  # output$github_repo <- renderUI({
  #   # tags$a(imageOutput("github-mark.png"),href="https://www.google.com")
  #   tags$a(img(src="github-mark.png"), href="https://github.com/Bohdan-Khomtchouk/Microscope")
  # })
  # output$similar_articles <- renderText({
  #   paste("You chose", input$select_article)
  # })

  # observe({
  #   updateSelectInput(session, "similar_articles",
  #                     choices = req_model[which(req_model$title == input$select_article)]
  #                     )
  # })

}
