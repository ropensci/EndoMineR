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
library(shinyFiles)
library(shinyBS)
library(dplyr)

fluidPage(theme=shinytheme("spacelab"),

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
        menuItem("Dashboard")
       
       
       
        )),


 
  dashboardBody(
    tabsetPanel(type = "tabs",
                tabPanel("Clean your Dataset", verbatimTextOutput("summary"),
                                   
    bsCollapse(id = "collapseExample", open = "Panel 1",
               bsCollapsePanel("Endoscopy Data", "", style = "info",
                               fileInput("endoscopy",
                                         label="Upload Endoscopy Dataset here",
                                         multiple = FALSE),
                               textInput("caption", "", "Enter the comma separated headers here"),
                               actionButton("textPrep",label = "textPrep"),
                               actionButton("EndoscEndoscopist",label = "EndoscEndoscopist"),
                               actionButton("EndoscMeds",label = "EndoscMeds"),
                               actionButton("EndoscInstrument",label = "EndoscInstrument"),
                               actionButton("DateStandardiserEndo",label = "DateStandardiserEndo"),
                               DT::dataTableOutput("endotable")),
               bsCollapsePanel("Pathology Data", "", style = "info",
                               fileInput("pathology",
                                         label="Upload Pathology Dataset here",
                                         multiple = FALSE),
                               textInput("captionPath", "", "Enter the comma separated headers here"),
                               actionButton("textPrepPath",label = "textPrepPath"),
                               actionButton("NumBx",label = "NumBx"),
                               actionButton("BxSize",label = "BxSize"),
                               actionButton("DateStandardiserEPath",label = "DateStandardiserEPath"),
                               DT::dataTableOutput("pathTable"))
               # bsCollapsePanel("Merged Data", "", style = "success",
               #                 fileInput("merged",
               #                           label="Upload Merged Dataset here",
               #                           multiple = FALSE),
               #                 textInput("captionMerge", "", "Enter the comma separated headers here"),
               #                 actionButton("Endomerge2",label = "Endomerge2"),
               #                 actionButton("MergeWithImages",label = "MergeWithImages"),
               #                 actionButton(inputId = "Del_row_head",label = "Delete selected rows"),
               #                 bsModal("modalExample", "Data Table", "MergeWithImages", size = "large",
               #                         shinyFilesButton("Btn_GetFile", "Choose a file" ,
               #                                          title = "Please select a file:", multiple = FALSE,
               #                                          buttonType = "default", class = NULL),
               #                         textOutput("txt_file"),    
               #                         textInput("captionDelim", "Which word separates the procedures", "delimiting word"),
               #                         textInput("captionImgFolder", "Get the Image folder", "Get the Image folder"),
               #                         shinyDirButton('folder', 'Folder select', 'Please select a folder', FALSE),
               #                         textOutput("folder_file"),    
               #                         actionButton("MergeImages",label = "Merge the images with your dataset")),
               #                 actionButton("CategoricalByEndoscopist",label = "CategoricalByEndoscopist"),
               #                 actionButton("HowManyOverTime",label = "HowManyOverTime"),
               #                 DT::dataTableOutput("mergedTable"))
               )
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
                    ),
    tabPanel("merged2", tableOutput("table6"),
             fileInput("merged",
                       label="Upload Merged Dataset here",
                       multiple = FALSE),
             textInput("captionMerge", "", "Enter the comma separated headers here"),
             actionButton("Endomerge2",label = "Endomerge2"),
             actionButton("MergeWithImages",label = "MergeWithImages"),
             actionButton(inputId = "Del_row_head",label = "Delete selected rows"),
             actionButton("CategoricalByEndoscopist",label = "CategoricalByEndoscopist"),
             actionButton("HowManyOverTime",label = "HowManyOverTime"),
             DT::dataTableOutput("mergedTable")
    )
    )
  )
)
)