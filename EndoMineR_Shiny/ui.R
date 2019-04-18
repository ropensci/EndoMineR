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
           actionButton("textPrep",label = "textPrep"),
           actionButton("EndoscEndoscopist",label = "EndoscEndoscopist"),
           actionButton("EndoscMeds",label = "EndoscMeds"),
           actionButton("EndoscInstrument",label = "EndoscInstrument"),
           DT::dataTableOutput("endotable")
  ),
  
  tabPanel("Pathology Dataset", tableOutput("table"),
           actionButton("textPrepPath",label = "textPrepPath"),
           actionButton("NumBx",label = "NumBx"),
           actionButton("BxSize",label = "BxSize"),
           actionButton("EndoscInstrument",label = "EndoscInstrument"),
           DT::dataTableOutput("pathTable")
  ),
  tabPanel("Merged Dataset", tableOutput("table2"),
           actionButton("Endomerge2",label = "Endomerge2")
  ),
  tabPanel("General Extrapolation", tableOutput("table3"),
           actionButton("Extrapolation1",label = "Extrapolation1"),
           actionButton("Extrapolation2",label = "Extrapolation2"),
           actionButton("Extrapolation3",label = "Extrapolation3"),
           actionButton("Extrapolation4",label = "Extrapolation4")
  ),
  tabPanel("General Analysis", tableOutput("table4"),
           actionButton("Analysis1",label = "Analysis1"),
           actionButton("Analysis2",label = "Analysis2"),
           actionButton("Analysis3",label = "Analysis3"),
           actionButton("Analysis4",label = "Analysis4")
  ),
  tabPanel("Data Visualisation", tableOutput("table5"),
           actionButton("DataViz1",label = "DataViz1"),
           actionButton("DataViz2",label = "DataViz2"),
           actionButton("DataViz3",label = "DataViz3"),
           actionButton("DataViz4",label = "DataViz4")
  )
  )
)
