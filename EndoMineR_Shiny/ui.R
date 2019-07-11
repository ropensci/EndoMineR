

library(shiny)
library(rpivotTable)
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





fluidPage(theme=shinytheme("cosmo"),

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
     sidebarPanel(
       tb1moduleUI("tb1")
     ), 
     tags$style(".left-side, .main-sidebar {padding-top: 60px}")
  ),

 
  dashboardBody(
    tabsetPanel(type = "tabs",
                tabPanel("Clean and Merge", verbatimTextOutput("summary"),
                                          
                         
                                   
    bsCollapse(id = "collapseExample", open = "Panel 1",
               bsCollapsePanel("Endoscopy Data", "", style = "info",
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "A. Upload data",
                                   fileInput("FileIn_endoscopy",label="",multiple = FALSE),br()),
                               
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "B. Split the data",
                               textInput("caption", "", "Enter the comma separated headers here"),
                               actionButton("textPrep",label = "textPrep"),
                               bsTooltip("textPrep", "Enter the headers from the text separated by a comma to split according to the headers", placement = "bottom", trigger = "hover",
                                         options = NULL)),
                               
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "C. Clean columns",br(), br(),
                               actionButton("DateStandardiserEndo",label = "", icon = icon("far fa-calendar-alt")),
                               bsTooltip("DateStandardiserEndo", "Select only one date column then press the button", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("HospitalNumberExtractorEndo",label = "", icon = icon("fas fa-barcode")),
                               bsTooltip("HospitalNumberExtractorEndo", "Select only one hospital number column then press the button to standardise", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("CategoricalDataEndo",label = "", icon = icon("far fa-flushed")),
                               bsTooltip("CategoricalDataEndo", "Select only one categorical column then press the button to standardise", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("AlphaNumericDataEndo",label = "", icon = icon("fas fa-font")),
                               bsTooltip("AlphaNumericDataEndo", "Select only one alphanumeric column then press the button to standardise", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("NumericDataEndo",label = "", icon = icon("fab fa-neos")),
                               bsTooltip("NumericDataEndo", "Select only one numeric column then press the button to standardise", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("NegExEndo",label = "Negex"),
                               bsTooltip("NegExEndo", "Select only one text column to exclude all sentences with negative expressions", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("Radiant",label = "Negex"),
                               bsTooltip("Radiant", "Perform further analyses", placement = "bottom", trigger = "hover",
                                         options = NULL)
                               ),
                              
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "D. Derive new columns",br(), br(),
                               actionButton("EndoscEndoscopist",label = "", icon = icon("user-md custom")),
                               bsTooltip("EndoscEndoscopist", "Standardise the endoscopist column", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("EndoscMeds",label = "",icon = icon("fas fa-pills")),
                               bsTooltip("EndoscMeds", "Select the medication column to extract medications", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("EndoscInstrument",label = "",icon = icon("stethoscope custom")),
                               bsTooltip("EndoscInstrument", "Select the Instrument column to clean instrument names", placement = "bottom", trigger = "hover",
                                         options = NULL)),
                               DT::dataTableOutput("endotable")),
               
               bsCollapsePanel("Pathology Data", "", style = "info",
                               fluidRow(
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "A. Upload  data",
                                   fileInput("pathology",label="",multiple = FALSE),br()),
                               
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "B. Split data",
                               textInput("captionPath", "", "Enter the comma separated headers here"),
                               actionButton("textPrepPath",label = "textPrepPath"),
                               bsTooltip("textPrepPath", "Enter the headers from the text separated by a comma to split according to the headers", placement = "bottom", trigger = "hover",
                                         options = NULL)),
                               
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "C. Clean columns",br(), br(),
                               actionButton("DateStandardiserEPath",label = "", icon = icon("far fa-calendar-alt")),
                               bsTooltip("DateStandardiserEPath", "Select only one date column then press the button", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("HospitalNumberExtractorPath",label = "", icon = icon("fas fa-barcode")),
                               bsTooltip("HospitalNumberExtractorPath", "Select only one hospital number column then press the button to standardise", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("CategoricalDataPath",label = "", icon = icon("far fa-flushed")),
                               bsTooltip("CategoricalDataPath", "Select only one categorical column then press the button to standardise", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("AlphaNumericDataPath",label = "", icon = icon("fas fa-font")),
                               bsTooltip("AlphaNumericDataPath", "Select only one alphanumeric column then press the button to standardise", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("NumericDataPath",label = "", icon = icon("fab fa-neos")),
                               bsTooltip("NumericDataPath", "Select only one numeric column then press the button to standardise", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("NegExPath",label = "Negex" ),
                               bsTooltip("NegExPath", "Select only one text column to exclude all sentences with negative expressions", placement = "bottom", trigger = "hover",
                                         options = NULL)),
                               
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "Derive new columns",br(), br(),
                               actionButton("NumBx",label = "",icon = icon("fas fa-microscope")),
                               bsTooltip("NumBx", "Select column (usually a macroscopic description column from pathology) to extract the total number of biopsies", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("BxSize",label = "",icon = icon("fas fa-sort-numeric-up")),
                               bsTooltip("BxSize", "Select column (usually a macroscopic description column from pathology) to extract the average biopsy size ", placement = "bottom", trigger = "hover",
                                         options = NULL)
                               )),
                               
                               DT::dataTableOutput("pathTable")),
               bsCollapsePanel("Merged and select rows and columns", "", style = "info",
                               fluidRow(
                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "A. Upload  data",
                                     fileInput("inFile_merged",label="",multiple = FALSE),br()),
                                 
                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "B. Split data",
                                     textInput("captionMerge", "", "Enter the comma separated headers here"),
                                     actionButton("textPrepMerge",label = "textPrepMerge"),
                                     bsTooltip("textPrepMerge", "Enter the headers from the text separated by a comma to split according to the headers", placement = "bottom", trigger = "hover",
                                               options = NULL)),
                                 
                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "C. Clean columns",br(), br(),
                                     actionButton("DateStandardiserMerge",label = "", icon = icon("far fa-calendar-alt")),
                                     bsTooltip("DateStandardiserMerge", "Select only one date column then press the button", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     actionButton("HospitalNumberExtractorMerge",label = "", icon = icon("fas fa-barcode")),
                                     bsTooltip("HospitalNumberExtractorMerge", "Select only one hospital number column then press the button to standardise", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     actionButton("CategoricalDataMerge",label = "", icon = icon("far fa-flushed")),
                                     bsTooltip("CategoricalDataMerge", "Select only one categorical column then press the button to standardise", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     actionButton("AlphaNumericDataMerge",label = "", icon = icon("fas fa-font")),
                                     bsTooltip("AlphaNumericDataMerge", "Select only one alphanumeric column then press the button to standardise", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     actionButton("NumericDataMerge",label = "", icon = icon("fab fa-neos")),
                                     bsTooltip("NumericDataMerge", "Select only one numeric column then press the button to standardise", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     actionButton("NegExMerge",label = "Negex" ),
                                     bsTooltip("NegExMerge", "Select only one text column to exclude all sentences with negative expressions", placement = "bottom", trigger = "hover",
                                               options = NULL)),
                                 
                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "Derive new columns",br(), br(),
                                     actionButton("NumBxMerge",label = "",icon = icon("fas fa-microscope")),
                                     bsTooltip("NumBxMerge", "Select column (usually a macroscopic description column from pathology) to extract the total number of biopsies", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     actionButton("BxSizeMerge",label = "",icon = icon("fas fa-sort-numeric-up")),
                                     bsTooltip("BxSizeMerge", "Select column (usually a macroscopic description column from pathology) to extract the average biopsy size ", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     
                                     actionButton("EndoscEndoscopistMerge",label = "", icon = icon("user-md custom")),
                                     bsTooltip("EndoscEndoscopistMerge", "Standardise the endoscopist column", placement = "bottom", trigger = "hover",
                                                   options = NULL),
                                     actionButton("EndoscMedsMerge",label = "",icon = icon("fas fa-pills")),
                                     bsTooltip("EndoscMedsMerge", "Select the medication column to extract medications", placement = "bottom", trigger = "hover",
                                                   options = NULL),
                                     actionButton("EndoscInstrumentMerge",label = "",icon = icon("stethoscope custom")),
                                     bsTooltip("EndoscInstrumentMerge", "Select the Instrument column to clean instrument names", placement = "bottom", trigger = "hover",
                                                   options = NULL)
                                 ),
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=FALSE,title = "Merging Functions",br(), br(),
                                   actionButton("Endomerge2",label = "Endomerge2"),
                                   bsTooltip("Endomerge2", "Make sure you have standarised both the date and hospital column in both the endoscopy and the pathology datasets, then press this button to get the datasets merged.", placement = "bottom", trigger = "hover",options = NULL),
                                   actionButton("MergeWithImages",label = "MergeWithImages"), 
                                   bsTooltip("MergeWithImages", "Press here to merge with images. The images must be from a html export with hospital numbers and dates so they can be merged.", placement = "bottom", trigger = "hover",options = NULL),
                                   actionButton(inputId = "Del_row_head",label = "Delete selected rows"),
                                   bsTooltip("Del_row_head", "Select individual rows with the checkbox and then press here to delete from the dataset", placement = "bottom", trigger = "hover",options = NULL)
                               ),
                                
                               bsModal("modalExampleImages", "Data Table1", "MergeWithImages", size = "large",
                                       shinyFilesButton("Btn_GetFile", "Choose a file" ,
                                                        title = "Please select a file:", multiple = FALSE, buttonType = "default", class = NULL),
                                       textOutput("txt_file"),
                                       textInput("captionDelim", "Which word separates the procedures", "delimiting word"),
                                       textInput("captionImgFolder", "Get the Image folder", "Get the Image folder"),
                                       
                                       shinyDirButton('folder', 'Folder select', 'Please select a folder', FALSE),
                                       textOutput("folder_file"),
                                       actionButton("MergeImages",label = "Merge the images with your dataset")),  
                               
                               DT::dataTableOutput("mergedTable"))
               ),
               bsCollapsePanel("Data from the trimmed Merged Table above", "", style = "info",
                               fluidRow(
                                 DT::dataTableOutput("trimTable")
                               )
               )
               )
                ),
    tabPanel("Visualise and Explore Trimmed data", tableOutput("table7"),
             bsCollapsePanel("Visualise", "", style = "info",
             radioButtons(
               inputId = "data",
               label = "Data to use:",
               choices = c("Endoscopy","Pathology","Merged data","Trimmed","Barretts"),
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
             ),
             bsCollapsePanel("Explore", "", style = "info",
                             navbarPage("App Title",
                                        tabPanel("Basic Stats",
                                                 selectInput(inputId = "datasetBasicStats",
                                                             label = "Choose a dataset:",
                                                             choices = c("Endoscopy", "Pathology","Merged","Trimmed","Barretts")),
                                                 
                                                 # Input: Numeric entry for number of obs to view ----
                                                 numericInput(inputId = "obs",
                                                              label = "Number of observations to view:",
                                                              value = 10),
                                                 
                                                 
                                                 # Output: Text for data summary ----
                                                 fluidRow(
                                                   column(12,
                                                   
                                                   uiOutput("ibox")
                                                 )),
                                                 
                                                 # Output: HTML table with requested number of observations ----
                                                 tableOutput("viewBasicStats")
                                                 
                                        ),
                                        tabPanel("Cross Tabulate", style="overflow: visible",
                                                 fluidRow(rpivotTableOutput("OverallPivot"))),
                                        tabPanel("Patient Flow"),
                                        navbarMenu("More",
                                                   tabPanel("Summary"),
                                                   "----",
                                                   "Section header",
                                                   tabPanel("Table")
                                        )
                             )
                             )
    ),
           
    tabPanel("Barrett's", tableOutput("table5"),
          bsCollapsePanel("Barrett's Data", "", style = "info",
             box(status = "warning", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "Derive Barrett's data",br(), br(),
             actionButton("PragueScore",label = "PragueScore"),
             bsTooltip("PragueScore", "Select two columns with endoscopic findings to generate C and M stage where possible", placement = "bottom", trigger = "hover",options = NULL),
             
             actionButton("PathStage",label = "PathStage"),
             bsTooltip("PathStage", "Select two columns with pathological findings to generate the worst pathological grade where possible", placement = "bottom", trigger = "hover",options = NULL),
             
             actionButton("FollowUpType",label = "FollowUpType"),
             bsTooltip("FollowUpType", "Only press once the Prague score and Path stage buttons have extracted the relevant columns", placement = "bottom", trigger = "hover",options = NULL),
             
             actionButton("AllTheFunctions",label = "AllTheFunctions"),
             actionButton("SurveillanceTime",label = "far fa-clock"),
             bsTooltip("SurveillanceTime", "Select the hospital number and the date of the procedure columns in order to get the time since the last test", placement = "bottom", trigger = "hover",options = NULL)
             ),
             DT::dataTableOutput("BarrettsTable")
          ),
          
          
          mainPanel(
            
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Quality Metrics", sidebarPanel(
                          #Select the column that has the endoscopists listed:
                          uiOutput("endoscopistCol"),
                          # Select the specific endoscopist:
                          uiOutput("endoscopistPick"),
                          # Select the column with the worst grade:
                          uiOutput("worstGradeCol")
                          ),mainPanel(plotOutput("plotBarrQM"))),
                        
                        
                        
                        
                        
                        
                        tabPanel("Endoscopic Quality", sidebarPanel(
                          #Select the column that has the endoscopists listed:
                          uiOutput("endoscopistCol_documentqual"),
                          # Select the specific endoscopist:
                          uiOutput("endoscopistPick_documentqual"),
                          # Select the column with the worst grade:
                          uiOutput("endoDoc_documentqual")
                        ),mainPanel(plotOutput("plotBarrEQ"))),
                        
                        
                        
                        
                        
                        
                        
                        tabPanel("Endo-pathological Quality", sidebarPanel(
                          # Select variable for y-axis
                          selectInput(inputId = "y", label = "Y-axis:",
                                      choices = colnames(df)
                          )
                        ),mainPanel(plotOutput("plotBarrEPQ"))),
                        
                        tabPanel("Patient Flow", sidebarPanel(
                          # Select variable for y-axis
                          selectInput(inputId = "y", label = "Y-axis:",
                                      choices = colnames(df)
                          )
                        ),mainPanel(plotOutput("plotBarrPF"))),
                        
                        tabPanel("Patient Theograph", sidebarPanel(
                          # Select variable for y-axis
                          selectInput(inputId = "y", label = "Y-axis:",
                                      choices = colnames(df)
                          )
                        ),mainPanel(plotOutput("plotBarrPT"))),
                        
                        tabPanel("Time Series Analysis", sidebarPanel(
                          # Select variable for y-axis
                          selectInput(inputId = "y", label = "Y-axis:",
                                      choices = colnames(df)
                          )
                        ),mainPanel(plotOutput("plotBarrTSA"))),
                        
                        fluidRow(
                          tabBox(
                            title = "First tabBox",
                            # The id lets us use input$tabset1 on the server to find the current tab
                            id = "tabset1", height = "250px",
                            tabPanel("Tab1", "First tab content"),
                            tabPanel("Tab2", "Tab content 2")
                          ),
                          tabBox(
                            side = "right", height = "250px",
                            selected = "Tab3",
                            tabPanel("Tab1", "Tab content 1"),
                            tabPanel("Tab2", "Tab content 2"),
                            tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
                          )
                        )
            )
            
          )
             
    ),
    tabPanel("Polyps", tableOutput("table3"),
        bsCollapsePanel("Polyp Data", "", style = "info",
             box(status = "warning", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "Derive Polyp data",br(), br(),
             actionButton("GRS",label = "GRS")
             ),
             DT::dataTableOutput("polypTable")
        ),
        mainPanel(
          
          # Output: Tabset w/ plot, summary, and table ----
          tabsetPanel(type = "tabs",
                      tabPanel("Quality Metrics", plotOutput("plotPolypQM")),
                      tabPanel("Endoscopic Quality", plotOutput("plotPolypEQ")),
                      tabPanel("Endo-pathological Quality", plotOutput("plotPolypEPQ")),
                      tabPanel("Patient Flow", plotOutput("plotPolypPF")),
                      tabPanel("Patient Theograph", plotOutput("plotPolypTheo")),
                      tabPanel("Time Series Analysis", plotOutput("plotPolypTSA")),
                      fluidRow(
                        tabBox(
                          title = "First tabBox",
                          # The id lets us use input$tabset1 on the server to find the current tab
                          id = "tabset1", height = "250px",
                          tabPanel("Tab1", "First tab content"),
                          tabPanel("Tab2", "Tab content 2")
                        ),
                        tabBox(
                          side = "right", height = "250px",
                          selected = "Tab3",
                          tabPanel("Tab1", "Tab content 1"),
                          tabPanel("Tab2", "Tab content 2"),
                          tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
                        )
                      )
          )
          
        )
    ),
    tabPanel("IBD", tableOutput("tableIBD"),
             
        bsCollapsePanel("IBD Data", "", style = "info",
             box(status = "warning", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "Derive IBD data",br(), br(),
                 actionButton("IBD",label = "IBD")
              ),
             DT::dataTableOutput("IBD")
        ),
        mainPanel(
          
          # Output: Tabset w/ plot, summary, and table ----
          tabsetPanel(type = "tabs",
                      tabPanel("Quality Metrics", plotOutput("plotIBDQM")),
                      tabPanel("Endoscopic Quality", plotOutput("plotIBDEQ")),
                      tabPanel("Endo-pathological Quality", plotOutput("plotIBDEPQ")),
                      tabPanel("Patient Flow", plotOutput("plotIBDPF")),
                      tabPanel("Patient Theograph", plotOutput("plotIBDTheo")),
                      tabPanel("Time Series Analysis", plotOutput("plotIBDTSA")),
                      fluidRow(
                        tabBox(
                          title = "First tabBox",
                          # The id lets us use input$tabset1 on the server to find the current tab
                          id = "tabset1", height = "250px",
                          tabPanel("Tab1", "First tab content"),
                          tabPanel("Tab2", "Tab content 2")
                        ),
                        tabBox(
                          side = "right", height = "250px",
                          selected = "Tab3",
                          tabPanel("Tab1", "Tab content 1"),
                          tabPanel("Tab2", "Tab content 2"),
                          tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
                        )
                      )
          )
 
        )
    )
    )
  )
)
)