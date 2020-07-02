
library(shinythemes)

fluidPage(theme = shinytheme("flatly"),
                   
                   
                   navbarPage("Data Science with R",position = "fixed-top", div(tags$style(type="text/css", "body {padding-top: 70px;}")),selected="Overview",
                              
                              
                              tabPanel("Overview",
                                       fluidRow(column(10,offset=2,wellPanel( includeHTML("content/b.html"))))
                                       
                                       
                              ),
                              tabPanel("Analysis",
                                       
                                       navlistPanel(fluid = TRUE,well = TRUE, widths = c(2, 10),
                                                                  tabPanel("Related Work",
                                                                           h1("Related Work", align="center"),
                                                                           fluidRow(column(12,wellPanel((includeText("content/text.txt")))) )
                                                                           
                                                                  ),
                                                               
                                                                  tabPanel("Data",
                                                                           h1("Data", align="center"),
                                                                           fluidRow(column(12,wellPanel((includeText("content/text.txt")))) )
                                                                                    
                                                                           ),
                                                                  tabPanel("Repository",
                                                                          h1("Repository", align="center"),
                                                                          fluidRow(column(12,wellPanel((includeText("content/mot.txt")))) )
                                                                  ),
                                                                 
                                                                 tabPanel("Reference",
                                                                          h1("Reference", align="center")
                                                                 )
                                                     )
                                                    
                                       ),
                              tabPanel("Visualization",),
                              tabPanel("Markdown",)
                   )
)


