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
library(esquisse)
library(jsmodule)
library(GGally)

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
      tags$style("html, body {overflow: visible !important;"),

dashboardPage(
  
  dashboardHeader(title = 'EndoMineR'),
 
  dashboardSidebar(
  
    
    sidebarMenu(
       
      tb1moduleUI("tb1")
       
        )),

 
  dashboardBody(
    tabsetPanel(type = "tabs",
                tabPanel("1. Clean your Dataset", verbatimTextOutput("summary"),
                                   
    bsCollapse(id = "collapseExample", open = "Panel 1",
               bsCollapsePanel("Endoscopy Data", "", style = "info",
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "A. Upload data",
                                   fileInput("FileIn_endoscopy",label="",multiple = FALSE)),
                               
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "B. Split the data",
                               textInput("caption", "", "Enter the comma separated headers here"),
                               actionButton("textPrep",label = "textPrep")),
                               
                               box(status = "warning", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "C. Clean columns",br(), br(),
                               actionButton("DateStandardiserEndo",label = "", icon = icon("far fa-calendar-alt")),
                               actionButton("HospitalNumberExtractorEndo",label = "", icon = icon("fas fa-barcode")),
                               actionButton("CategoricalDataEndo",label = "", icon = icon("far fa-flushed")),
                               actionButton("AlphaNumericDataEndo",label = "", icon = icon("fas fa-font")),
                               actionButton("NumericDataEndo",label = "", icon = icon("fab fa-neos"))
                               ),
                              
                               box(status = "warning", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "D. Derive new columns",br(), br(),
                               actionButton("EndoscEndoscopist",label = "", icon = icon("user-md custom")),
                               actionButton("EndoscMeds",label = "",icon = icon("fas fa-pills")),
                               actionButton("EndoscInstrument",label = "",icon = icon("stethoscope custom"))),
                               DT::dataTableOutput("endotable")),
               
               bsCollapsePanel("Pathology Data", "", style = "info",
                               fluidRow(
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "A. Upload  data",
                                   fileInput("pathology",label="",multiple = FALSE)),
                               
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "B. Split data",
                               textInput("captionPath", "", "Enter the comma separated headers here"),
                               actionButton("textPrepPath",label = "textPrepPath")),
                               
                               box(status = "warning", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "C. Clean columns",br(), br(),
                               actionButton("DateStandardiserEPath",label = "", icon = icon("far fa-calendar-alt")),
                               actionButton("HospitalNumberExtractorPath",label = "", icon = icon("fas fa-barcode")),
                               actionButton("CategoricalDataPath",label = "", icon = icon("far fa-flushed")),
                               actionButton("AlphaNumericDataPath",label = "", icon = icon("fas fa-font")),
                               actionButton("NumericDataPath",label = "", icon = icon("fab fa-neos"))),
                               
                               box(status = "warning", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "Derive new columns",br(), br(),
                               actionButton("NumBx",label = "",icon = icon("fas fa-microscope")),
                               actionButton("BxSize",label = "",icon = icon("fas fa-sort-numeric-up"))
                               )),
                               
                               DT::dataTableOutput("pathTable"))
               )
                ),
           
    tabPanel("2. Merge Your Datasets", tableOutput("table6"),
             
             box(status = "warning", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "Upload data anew",br(), br(),
             fileInput("merged",
                       label="Upload Merged Dataset here",
                       multiple = FALSE),
             textInput("captionMerge", "", "Enter the comma separated headers here")),
             
             box(status = "warning", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "Merging Functions",br(), br(),
             actionButton("Endomerge2",label = "Endomerge2"),
             actionButton("MergeWithImages",label = "MergeWithImages")),
             actionButton(inputId = "Del_row_head",label = "Delete selected rows"),

             
             bsModal("modalExampleImages", "Data Table1", "MergeWithImages", size = "large",
                     shinyFilesButton("Btn_GetFile", "Choose a file" ,
                                      title = "Please select a file:", multiple = FALSE, buttonType = "default", class = NULL),
                     textOutput("txt_file"),
                     textInput("captionDelim", "Which word separates the procedures", "delimiting word"),
                     textInput("captionImgFolder", "Get the Image folder", "Get the Image folder"),
                     
                     shinyDirButton('folder', 'Folder select', 'Please select a folder', FALSE),
                     textOutput("folder_file"),
                     actionButton("MergeImages",label = "Merge the images with your dataset")),     
                     DT::dataTableOutput("mergedTable")
    ),
    tabPanel("Barrett's", tableOutput("table5"),
             box(status = "warning", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "Derive Barrett's data",br(), br(),
             actionButton("PragueScore",label = "PragueScore"),
             actionButton("PathStage",label = "PathStage"),
             actionButton("FollowUpType",label = "FollowUpType"),
             actionButton("AllTheFunctions",label = "AllTheFunctions"),
             actionButton("SurveillanceTime",label = "far fa-clock")),
             DT::dataTableOutput("BarrettsTable")
    ),
    tabPanel("Polyps", tableOutput("table3"),
             actionButton("GRS",label = "GRS"),
             DT::dataTableOutput("polypTable")
    ),
    tabPanel("Visualise", tableOutput("table7"),
             
    radioButtons(
      inputId = "data",
      label = "Data to use:",
      choices = c("Endoscopy","Pathology","Merged data","Barretts"),
      inline = TRUE
    ),
    tags$div(
      style = "height: 700px;", # needs to be in fixed height container
      esquisserUI(
        id = "esquisse",
        header = FALSE, # dont display gadget title
        choose_data = FALSE # dont display button to change data
      )
    )
    ) ,
    tabPanel("2. Merge Your Datasets", tableOutput("table16"),
            # tb1moduleUI("tb1"),
            DT::dataTableOutput("table1")
    )
    )
  )
)
)