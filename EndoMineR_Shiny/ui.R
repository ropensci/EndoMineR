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

  fluidPage(tabsetPanel(type = "tabs",
                        
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
           DT::dataTableOutput("mergedTable")
  ),
  tabPanel("Barrett's", tableOutput("table5"),
           actionButton("PragueScore",label = "PragueScore"),
           actionButton("PathStage",label = "PathStage"),
           actionButton("FollowUpType",label = "FollowUpType"),
           actionButton("AllTheFunctions",label = "DataViz4")
  ),
  tabPanel("Polyps", tableOutput("table3"),
           actionButton("GRS",label = "GRS"),
           DT::dataTableOutput("polypTable")
  ),
  tabPanel("General Analysis", tableOutput("table4"),
           actionButton("Analysis1",label = "Analysis1"),
           actionButton("Analysis2",label = "Analysis2"),
           actionButton("Analysis3",label = "Analysis3"),
           actionButton("Analysis4",label = "Analysis4")
  )
  )
)
