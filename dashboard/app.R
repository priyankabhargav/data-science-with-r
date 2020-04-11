library(shiny)
library(shinythemes)
library(shinydashboard)
library(markdown)

#UI elements
ui<-fluidPage(
    dashboardPage( skin = "red",
        dashboardHeader(title = "Data Science with R"),
        dashboardSidebar(
                         
                         sidebarMenu(
                             menuItem("Introduction", tabName = "introduction", icon = icon("th")),
                             menuItem("Motivation", tabName = "motivation", icon = icon("hands-helping")),
                             menuItem("Objective", tabName = "objective", icon = icon("bullseye")),
                             menuItem("The Team", tabName = "team", icon = icon("users")),
                             menuItem("Data", tabName = "data", icon = icon("database")),
                             menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar")),
                             menuItem("Process Markdown", tabName = "markdown", icon = icon("file-alt")),
                             menuItem(" Repository", tabName = "repository", icon = icon("code-branch")),
                             menuItem("References", tabName = "references", icon = icon("link"))
                         )
        ),
        dashboardBody(
           includeCSS("www/style.css"),
            tabItems(
                # Introduction
                tabItem(tabName = "introduction",
                            h1("Introduction",align="center"),
                            fluidRow(column(12, wellPanel(includeText("text.txt")))),
                            box(align="center", width = "100%",
                                tags$iframe(width="100%", height=350, src="https://www.youtube.com/embed/T1-k7VYwsHg", frameborder="2", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=TRUE)
                            )
                ),
                
                # Motivation
                tabItem(tabName = "motivation",
                           h1("Motivation", align="center")
                        
                ),
                
                # Objective
                tabItem(tabName = "objective",
                        h1("Objective", align="center")
                        
                ),
                
                # The Team
                tabItem(tabName = "team",
                        h1("The Team", align="center")
                ),
                
                # Data
                tabItem(tabName = "data",
                        h1("Data", align="center")
                ),
                
                # Analysis
                tabItem(tabName = "analysis",
                        h1("Analysis", align="center")
                ),
                
                # Process Markdown
                tabItem(tabName = "markdown",
                        h1("Process Notebook", align="center"),
                        includeMarkdown("test.Rmd")
                ),
                
                # Repository
                tabItem(tabName = "repository",
                        h1("Repository", align="center")
                ),
                
                # References
                tabItem(tabName = "references",
                        h1("References", align="center")
                )
            )
        )
    )
)


server <- function(input, output) {
}

shinyApp(ui, server)
