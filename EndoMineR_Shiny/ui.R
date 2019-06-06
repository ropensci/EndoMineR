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
library(shinythemes)

fluidPage(theme=shinytheme("spacelab"),
 # box(width=12,
      h3(strong("Endoscopy labelled Image Picker"),align="center"),
      hr(),
   # column(6,offset = 6,
             HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
             actionButton(inputId = "Del_row_head",label = "Delete selected rows"),
             HTML('</div>'),
     # ),
      
      #column(12,dataTableOutput("Main_table")),
      tags$script(HTML('$(document).on("click", "input", function () {
                       var checkboxes = document.getElementsByName("row_selected");
                       var checkboxesChecked = [];
                       for (var i=0; i<checkboxes.length; i++) {
                       if (checkboxes[i].checked) {
                       checkboxesChecked.push(checkboxes[i].value);
                       }
                       }
                       Shiny.onInputChange("checked_rows",checkboxesChecked);
                       })')),
      tags$script("$(document).on('click', '#Main_table button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                  Shiny.onInputChange('lastClick', Math.random())
                  });"),

  #),

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
                               actionButton("DateStandardiserEndo",label = "DateStandardiserEndo"),
                               DT::dataTableOutput("endotable")),
               bsCollapsePanel("Pathology Data", "Upload data to get the pathology dataset", style = "info",
                               textInput("captionPath", "Caption", "Data Summary"),
                               actionButton("textPrepPath",label = "textPrepPath"),
                               actionButton("NumBx",label = "NumBx"),
                               actionButton("BxSize",label = "BxSize"),
                               actionButton("DateStandardiserEPath",label = "DateStandardiserEPath"),
                               DT::dataTableOutput("pathTable")),
               bsCollapsePanel("Merged Data", "Upload data to get the Merged dataset", style = "info",
                               textInput("captionMerge", "Caption", "Data Summary"),
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
)