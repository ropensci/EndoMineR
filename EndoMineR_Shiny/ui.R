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


dashboardPage(
  
  dashboardHeader(title = 'EndoMineR'),
 
  dashboardSidebar(
    
    sidebarMenu(
        menuItem("Dashboard"),
        fileInput("endoscopy",
                  label="Upload Endoscopy Dataset here",
                  multiple = FALSE),
        fileInput("pathology",
                  label="Upload Pathology Dataset here",
                  multiple = FALSE),
        fileInput("merged",
                  label="Upload Merged Dataset here",
                  multiple = FALSE)
        )),


 
  dashboardBody(
    tabsetPanel(type = "tabs",
                tabPanel("Clean your Dataset", verbatimTextOutput("summary"),
                                   
    bsCollapse(id = "collapseExample", open = "Panel 1",
               bsCollapsePanel("Endoscopy Data", "Upload data to get the endoscopy dataset", style = "info",
                               textInput("caption", "Caption", "Data Summary"),
                               actionButton("textPrep",label = "textPrep"),
                               actionButton("EndoscEndoscopist",label = "EndoscEndoscopist"),
                               actionButton("EndoscMeds",label = "EndoscMeds"),
                               actionButton("EndoscInstrument",label = "EndoscInstrument"),
                               DT::dataTableOutput("endotable")),
               bsCollapsePanel("Pathology Data", "Upload data to get the pathology dataset", style = "info",
                               textInput("caption", "Caption", "Data Summary"),
                               actionButton("textPrepPath",label = "textPrepPath"),
                               actionButton("NumBx",label = "NumBx"),
                               actionButton("BxSize",label = "BxSize"),
                               DT::dataTableOutput("pathTable")),
               bsCollapsePanel("Merged Data", "Upload data to get the Merged dataset", style = "info",
                               textInput("caption", "Caption", "Data Summary"),
                               actionButton("Endomerge2",label = "Endomerge2"),
                               actionButton("MergeWithImages",label = "MergeWithImages"),
                               actionButton("CategoricalByEndoscopist",label = "CategoricalByEndoscopist"),
                               actionButton("HowManyOverTime",label = "HowManyOverTime"),
                               DT::dataTableOutput("mergedTable")))
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