library(shiny)
library(shinythemes)
library(shinydashboard)
library(markdown)
library(LDAvis)
library(topicmodels)
library(Rtsne)
library(tsne)
library(tidytext)
library(dplyr)
library(ggplot2)
library(reshape2)
library(rbokeh)
library(dashboardthemes)

# load(file = "top_10_model.Rdata")

#UI elements
ui<-
  dashboardPage( 
    
    
    # skin = "purple", 
                 #“blue”, “black”, “purple”, “green”, “red”, “yellow”, "blue_gradient"
                 dashboardHeader(title =  shinyDashboardLogo(
                   theme = "purple_gradient",
                   boldText = "Classification of Covid-19 news articles",
                   mainText = "",
                   badgeText = "Data Science with R"
                 )),
                 title = "Classification of Covid-19 news articles",
                 dashboardSidebar(
                   tags$script(HTML("$('body').addClass('fixed');")),
                   tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                   ),
                   # title = "Data Science with R",
                   
                   sidebarMenu(
                     menuItem("Overview and Motivation", tabName = "overview", icon = icon("th")),
                     menuItem("Related Work", tabName = "related_work", icon = icon("hands-helping")),
                     menuItem("Objective", tabName = "objective", icon = icon("bullseye")),
                     # menuItem("The Team", tabName = "team", icon = icon("users")),
                     menuItem("Data", tabName = "data", icon = icon("database")),
                     menuItem("Exploratory Analysis", tabName = "exploratory_analysis", icon = icon("table")),
                     menuItem("Demo", tabName = "demo", icon = icon("bar-chart-o")
                              # menuSubItem("AP(Mill) Mumbai-MH",
                              #             tabName = "dashboard3")
                              ,menuSubItem("LDA Visualization",tabName = "ldaVis_dashboard")
                              ,menuSubItem("Topic-Terms Plot",tabName = "termTopic_dashboard")
                              ,menuSubItem("Bokeh Plot",tabName = "bokeh_dashboard")
                              ,menuSubItem("Prediction", tabName = "prediction")
                     ),
                     # menuItem("Prediction", tabName = "prediction", icon = icon("chart-bar")),
                     menuItem("Final Analysis", tabName = "final_analysis", icon = icon("list-alt")),
                     menuItem("Process Notebook", tabName = "markdown", icon = icon("file-alt")),
                     menuItem(" Repository", tabName = "repository", icon = icon("code-branch")),
                     menuItem("References", tabName = "references", icon = icon("link"))
                   )
                 ),
                 dashboardBody( shinyDashboardThemes(
                   theme = "blue_gradient"
                 ),
                 tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }'
                                           ))),
                  
                   tabItems(
                     # Introduction
                     tabItem(tabName = "overview",
                             h1("Overview and Motivation",align="center"),
                             fluidRow(column(12, wellPanel(includeText("text content/text.txt")))),
                             box(align="center", width = "100%",
                                 tags$iframe(width="100%", height= 425, src="https://www.youtube.com/embed/T1-k7VYwsHg", frameborder="2", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=TRUE)
                             )
                     ),
                     
                     # Related Work
                     tabItem(tabName = "related_work",
                             h1("Related Work", align="center"),
                             htmlOutput("relatedPage")
                             
                     ),
                     
                     # Objective
                     tabItem(tabName = "objective",
                             h1("Objective", align="center"),
                             htmlOutput("objectivePage")
                             
                     ),
                     
                     # Data
                     tabItem(tabName = "data",
                             h1("Data", align="center"),
                             htmlOutput("dataPage")
                     ),
                     #Exploratory Analysis
                     tabItem(tabName = "exploratory_analysis",
                             h1("Exploratory Analysis", align="center"),
                             htmlOutput("exploPage")
                     ),
                     
                     tabItem(tabName = "demo",
                             h1("Demo", align="center")
                     ),
                     
                     #LDA Visualization
                     tabItem(tabName = "ldaVis_dashboard",
                             titlePanel("LDA Visualization"),
                             fluidRow(sliderInput("nTerms", "Number of terms to display", min = 5, max = 50, value = 10),
                                      textOutput("termClicked"),
                                      textOutput("topicClicked"),
                                      visOutput('myChart')
                             ),
                      ),
                     
                     #Topic-Terms Plot
                     tabItem(tabName = "termTopic_dashboard",
                             titlePanel("Topic-Terms Plot"),
                             fluidRow(sliderInput("nTopicTerms", "Number of terms to display", min = 5, max = 25, value = 10),
                                      plotOutput("topicTermsPlot", height = 1000, width = 1200)
                             ),
                     ),
                     
                     #Bokeh Plot
                     tabItem(tabName = "bokeh_dashboard",
                             titlePanel("Bokeh Plot"),
                             fluidRow(rbokehOutput("rbokeh", width = 1000, height = 800),
                             ),
                     ),
                     #Prediction
                     tabItem(tabName = "prediction",
                              titlePanel("Find the similar articles"),
                             fluidPage(
                               tags$style(type='text/css', ".selectize-input { font-size: 13px; line-height: 13px; } 
                 .selectize-dropdown { font-size: 13px; line-height: 13px; }
                 .form-group, .selectize-control {margin-bottom:-10px;  max-height: 200px !important;}
                 .box-body {
          padding-bottom: 0px; 
      }"),
                             sidebarLayout(
                               sidebarPanel(
                                 selectizeInput("articleInput", "Select an article to view the similar articles", 
                                                choices=NULL, 
                                                selected=NULL,),
                                 htmlOutput("similar_articles")),
                               mainPanel(
                                 uiOutput("tb", width="50px")
                               )
                             )
                             )
                     ),
                     
                     # Process Markdown
                     tabItem(tabName = "final_analysis",
                             h1("Final Analysis", align="center"),
                             htmlOutput("finalPage")
                             

                     ),
                     
                     # Process Markdown
                     tabItem(tabName = "markdown",
                             h1("Process Notebook", align="center"),
                             fluidPage(
                               mainPanel(
                                 htmlOutput("proPage")
                               )
                             )
                             
                     ),
                     
                     # Repository
                     tabItem(tabName = "repository",
                             h1("Repository", align="center"),
                             fluidRow(column(12, wellPanel(includeText("text content/repository.txt")))),
                             box(align="center", width = "10%", height="10%",
                                 tags$a(img(src="https://github.githubassets.com/images/modules/open_graph/github-mark.png",
                                            height="50%", width="50%", align="center"), 
                                        href="https://github.com/CalidaPereira/data-science-with-r")
                             )
                             
                     ),
                     
                     # References
                     tabItem(tabName = "references",
                             h1("References", align="center"),
                             htmlOutput("referencePage")
                             
                             
                     )
                   )
                 )
  )