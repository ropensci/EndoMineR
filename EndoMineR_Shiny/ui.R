#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(EndoMineR)
library(shinydashboard)
library(shinydashboardPlus)


dashboardPagePlus(
  dashboardHeaderPlus(title = 'EndoMineR',
                      fixed = TRUE,
                      enable_rightsidebar = TRUE,
                      dropdownMenu(
                        type = "tasks",
                        badgeStatus = "danger",
                        taskItem(value = 20, color = "aqua", "Refactor code"),
                        taskItem(value = 40, color = "green", "Design new layout"),
                        taskItem(value = 60, color = "yellow", "Another task"),
                        taskItem(value = 80, color = "red", "Write documentation")
                      )),
  dashboardSidebar(box(
    width = NULL,
    title = "Links and Tutorials",
    status = NULL,
    appButton(
      url = "https://ropensci.github.io/EndoMineR/articles/EndoMineRPrinciples.html",
      label = "Tutorials for under the hood", 
      icon = "fa fa-users", 
      enable_badge = TRUE, 
      badgeColor = "purple", 
      badgeLabel = 891
    ),
    appButton(
      label = "Gastro data science", 
      icon = "fas fa-info-circle", 
      enable_badge = FALSE, 
      badgeColor = NULL, 
      badgeLabel = NULL
    ),
    appButton(
      label = "Likes", 
      icon = "fa fa-heart-o", 
      enable_badge = TRUE, 
      badgeColor = "red", 
      badgeLabel = 3
    ),
    socialButton(
      url = "https://twitter.com/GastroDS",
      type = "twitter"
    ),
    socialButton(
      url = "http://github.com/sebastiz",
      type = "github"
    )
  )
  ),
  dashboardBody(
  

    
    
  tabsetPanel(type = "tabs",
                        
  tabPanel("Endoscopy Dataset", verbatimTextOutput("summary"),
           textInput("caption", "Caption", "Data Summary"),
           actionButton("textPrep",label = "textPrep"),
           actionButton("EndoscEndoscopist",label = "EndoscEndoscopist"),
           actionButton("EndoscMeds",label = "EndoscMeds"),
           actionButton("EndoscInstrument",label = "EndoscInstrument"),
           DT::dataTableOutput("endotable")
  ),
  
  tabPanel("Pathology Dataset", tableOutput("table"),
           textInput("captionpath", "Caption", "Data Summary"),
           actionButton("textPrepPath",label = "textPrepPath"),
           actionButton("NumBx",label = "NumBx"),
           actionButton("BxSize",label = "BxSize"),
           actionButton("EndoscInstrument",label = "EndoscInstrument"),
           DT::dataTableOutput("pathTable")
  ),
  tabPanel("Merged Dataset", tableOutput("table2"),
           actionButton("Endomerge2",label = "Endomerge2"),
           actionButton("MetricByEndoscopist",label = "MetricByEndoscopist"),
           actionButton("CategoricalByEndoscopist",label = "CategoricalByEndoscopist"),
           actionButton("HowManyOverTime",label = "HowManyOverTime"),
           DT::dataTableOutput("mergedTable"),
           verbatimTextOutput('foo'),
           DT::dataTableOutput("metricTable"),
           plotOutput("plot")
  ),
  tabPanel("Barrett's", tableOutput("table5"),
           actionButton("PragueScore",label = "PragueScore"),
           actionButton("PathStage",label = "PathStage"),
           actionButton("FollowUpType",label = "FollowUpType"),
           actionButton("AllTheFunctions",label = "AllTheFunctions"),
           actionButton("SurveillanceTime",label = "SurveillanceTime"),
           actionButton("SurveillanceLastTest",label = "SurveillanceLastTest"),
           actionButton("SurveillanceFirstTest",label = "SurveillanceFirstTest")
  ),
  tabPanel("Polyps", tableOutput("table3"),
           actionButton("GRS",label = "GRS"),
           DT::dataTableOutput("polypTable")
  )
  )

  )
)
