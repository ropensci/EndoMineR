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
library(plotly)





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
      tags$style("html, body {overflow: visible !important;.rpivotTable{ overflow-x: scroll; }"),

dashboardPage(
  
  dashboardHeader(title = 'EndoMineR',
                  tags$li( class="dropdown",a(href = 'https://ropensci.github.io/EndoMineR/articles/EndoMineRPrinciples.html',
                            icon("fa-li fa-bullseye"))),
                  tags$li(class="dropdown",a(href = 'https://twitter.com/GastroDS',
                            icon("fa-li fa fa-twitter"))),
                  tags$li(class="dropdown",a(href = 'https://github.com/ropensci/EndoMineR',
                            icon("fa-li fa fa-github fa-lg"))),
                  tags$li(class="dropdown",a(href = 'https://sebastiz.github.io/gastrodatascience/',
                            icon("fa-li fa fa-book"))),
                  tags$li(class="dropdown",a(href = ' https://sebastiz.github.io/gastroDS3',
                                             icon("fa-li fa fa-book")))
                  
                 
                                          
  ),
 
   dashboardSidebar(collapsed = TRUE
     
  ),

 
  dashboardBody(
    tabsetPanel(type = "tabs",
                tabPanel("Clean and Merge", verbatimTextOutput("summary"),
                                          
                         
# Dataset 1 ----------------------------------------------------

    bsCollapse(id = "collapseExample", open = "Panel 1",
               bsCollapsePanel("Dataset 1", "", style = "info",
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "A. Upload data",
                                   fileInput("FileIn_endoscopy",label="",multiple = FALSE),br()),
                               
                               
                               
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "B. Split the data",
                               textInput("caption", "", "Enter the comma separated headers here"),
                               actionButton("textPrep",label = "textPrep"),
                                bsPopover ("textPrep", "Enter the headers from the text separated by a comma to split according to the headers", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("DateStandardiserEndo",label = "", icon = icon("far fa-calendar-alt")),
                               bsPopover ("DateStandardiserEndo", "Select only one date column then press the button", placement = "bottom", trigger = "hover",
                                          options = NULL),
                               actionButton("HospitalNumberExtractorEndo",label = "", icon = icon("fas fa-barcode")),
                               bsPopover ("HospitalNumberExtractorEndo", "Select only one hospital number column then press the button to standardise", placement = "bottom", trigger = "hover",
                                          options = NULL)),
                               DT::dataTableOutput("endotable")),

# Dataset 2 ----------------------------------------------------              
               
                              
               bsCollapsePanel("Dataset 2", "", style = "info",
                               fluidRow(
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "A. Upload  data",
                                   fileInput("pathology",label="",multiple = FALSE),br()),

                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "B. Split data",
                               textInput("captionPath", "", "Enter the comma separated headers here"),
                               actionButton("textPrepPath",label = "textPrepPath"),
                                bsPopover ("textPrepPath", "Enter the headers from the text separated by a comma to split according to the headers", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               actionButton("DateStandardiserEPath",label = "", icon = icon("far fa-calendar-alt")),
                               bsPopover ("DateStandardiserEPath", "Select only one date column then press the button", placement = "bottom", trigger = "hover",
                                          options = NULL),
                               actionButton("HospitalNumberExtractorPath",label = "", icon = icon("fas fa-barcode")),
                               bsPopover ("HospitalNumberExtractorPath", "Select only one hospital number column then press the button to standardise", placement = "bottom", trigger = "hover",
                                          options = NULL))),

                               DT::dataTableOutput("pathTable")),

# Final Dataset  ----------------------------------------------------

               bsCollapsePanel("Final Dataset", "", style = "info",
                               fluidRow(
                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "A. Upload  data",
                                     fileInput("inFile_merged",label="",multiple = FALSE),br()),

                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "B. Split data",
                                     textInput("captionMerge", "", "Enter the comma separated headers here"),
                                     actionButton("textPrepMerge",label = "textPrepMerge"),
                                      bsPopover  ("textPrepMerge", "Enter the headers from the text separated by a comma to split according to the headers", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     actionButton("DateStandardiserMerge",label = "", icon = icon("far fa-calendar-alt")),
                                     bsPopover ("DateStandardiserMerge", "Select only one date column then press the button", placement = "bottom", trigger = "hover",
                                                options = NULL),
                                     actionButton("HospitalNumberExtractorMerge",label = "", icon = icon("fas fa-barcode")),
                                     bsPopover ("HospitalNumberExtractorMerge", "Select only one hospital number column then press the button to standardise", placement = "bottom", trigger = "hover",
                                                options = NULL)),

                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "C. Clean columns",br(), br(),
                                     actionButton("CategoricalDataMerge",label = "", icon = icon("far fa-flushed")),
                                      bsPopover ("CategoricalDataMerge", "Select only one categorical column then press the button to standardise", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     actionButton("NumericDataMerge",label = "", icon = icon("fab fa-neos")),
                                      bsPopover ("NumericDataMerge", "Select only one numeric column then press the button to standardise", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     actionButton("NegExMerge",label = "Negex" ),
                                      bsPopover ("NegExMerge", "Select only one text column to exclude all sentences with negative expressions", placement = "bottom", trigger = "hover",
                                               options = NULL)),
                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=FALSE,title = "Merging Functions",br(), br(),
                                     actionButton("Endomerge2",label = "Endomerge2"),
                                     bsPopover ("Endomerge2", "Make sure you have standarised both the date and hospital column in both the endoscopy and the pathology datasets, then press this button to get the datasets merged.", placement = "bottom", trigger = "hover",options = NULL),
                                     actionButton("MergeWithImages",label = "MergeWithImages"),
                                     bsPopover ("MergeWithImages", "Press here to merge with images. The images must be from a html export with hospital numbers and dates so they can be merged.", placement = "bottom", trigger = "hover",options = NULL),
                                     actionButton(inputId = "Del_row_head",label = "Delete selected rows"),
                                     bsPopover ("Del_row_head", "Select individual rows with the checkbox and then press here to delete from the dataset", placement = "bottom", trigger = "hover",options = NULL)
                                 ),

                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "Derive new columns",br(), br(),
                                     actionButton("NumBxMerge",label = "",icon = icon("fas fa-microscope")),
                                      bsPopover ("NumBxMerge", "Select column (usually a macroscopic desc ription column from pathology) to extract the total number of biopsies", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     # bsModal("NumBxModal", "Delimiter", "NumBxMerge", size = "small",
                                     #         textInput("new_name", "which word is your delimiter- ie which word is used to call the number of specimens. Some use 
                                     #                   'There are 4 pieces of tissue' or '8 specimens' in which case it would be specimen or piece (always use the singular)", "")
                                     # ),
                                     bsModal("NumBxModal", "Change name", "NumBxMerge", size = "small",
                                             textInput("new_name", "Enter the delimiter here:", "") ,
                                             footer = tagList(
                                               modalButton("Cancel"),
                                               actionButton("NumBxModal_ok", "OK")
                                             )
                                     ),
                                  
                                     actionButton("BxSizeMerge",label = "",icon = icon("fas fa-sort-numeric-up")),
                                      bsPopover ("BxSizeMerge", "Select column (usually a macroscopic description column from pathology) to extract the average biopsy size ", placement = "bottom", trigger = "hover",
                                               options = NULL),

                                     actionButton("EndoscEndoscopistMerge",label = "", icon = icon("user-md custom")),
                                      bsPopover ("EndoscEndoscopistMerge", "Standardise the endoscopist column", placement = "bottom", trigger = "hover",
                                                   options = NULL),
                                     actionButton("EndoscMedsMerge",label = "",icon = icon("fas fa-pills")),
                                      bsPopover ("EndoscMedsMerge", "Select the medication column to extract medications", placement = "bottom", trigger = "hover",
                                                   options = NULL),
                                     actionButton("EndoscInstrumentMerge",label = "",icon = icon("stethoscope custom")),
                                      bsPopover ("EndoscInstrumentMerge", "Select the Instrument column to clean instrument names", placement = "bottom", trigger = "hover",
                                                   options = NULL),
                                     actionButton("EndoEvent",label = "EndoEvent",icon = icon("fas fa-sort-numeric-up")),
                                     bsPopover ("EndoEvent", "Select: Endoscopic Findings,ProcedurePerformed,Macroscopicdescription and Histology text", placement = "bottom", trigger = "hover",
                                                options = NULL),
                                     actionButton("Regex",label = "",icon = icon("fas fa-sort-numeric-up")),
                                     bsPopover ("Regex", "Put in a regular expression or a keyowrd to derive a new column with those elements extracted so you can filter on them", placement = "bottom", trigger = "hover",
                                                options = NULL),
                                     bsModal("RegexColAdder", "Change name", "Regex", size = "small",
                                             textInput("regexSearch", "Enter the search term here", "") ,
                                             footer = tagList(
                                               modalButton("Cancel"),
                                               actionButton("regexSearch_ok", "OK")
                                             )
                                     )
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
               )

               )
                ),





# Custom ----------------------------------------------------  


tabPanel("Custom", tableOutput("table53"),
         
         bsCollapsePanel("Select columns from the Final Dataset and click to display here", "", style = "info",
                         fluidRow(
                           DT::dataTableOutput("CustomTable")
                         )
         ),
         mainPanel(width = 100,
                   navbarPage("Your analytics",
                              tabPanel("Visualise",
                                       tags$div(
                                         style = "height: 700px;", # needs to be in fixed height container
                                         esquisserUI(
                                           id = "esquisseCustom",
                                           header = FALSE, # dont display gadget title
                                           choose_data = FALSE # dont display button to change data
                                         )
                                       )),

                              tabPanel("Cross Tabulate", style="overflow: visible",
                                       fluidRow(rpivotTableOutput("OverallPivot"))),
                              tabPanel("Endoscopy Utilisation", style="overflow: visible",
                                       sidebarPanel(
                                       uiOutput("Date_endoscopyutilisationCustom"),
                                       uiOutput("endoscopicEventCustom"),width = 2),
                                       mainPanel(verticalLayout(plotlyOutput("endoscopyUse_EndoscopyUseCustom"),
                                                                plotlyOutput("plotCustomTSA"))
                                       )),
                              tabPanel("Theograph", sidebarPanel(width = 2,
                                                                 # Select variable for the hospital number
                                                                 uiOutput("HospNumCustomTheo"),
                                                                 # Select variable for the dates
                                                                 uiOutput("DatesCustomTheo")
                              ),mainPanel(plotlyOutput("plotCustomPT")))
 )
         )
         
),
         
         
         
         
# Barrett's ----------------------------------------------------  


    tabPanel("Barrett's", tableOutput("table5"),
          bsCollapsePanel("Barrett's Data", "", style = "info",
             box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "Derive Barrett's data",br(), br(),
             actionButton("PragueScore",label = "PragueScore"),
              bsPopover ("PragueScore", "Select two columns with endoscopic findings to generate C and M stage where possible", placement = "bottom", trigger = "hover",options = NULL),
             
             actionButton("PathStage",label = "PathStage"),
              bsPopover ("PathStage", "Select two columns with pathological findings to generate the worst pathological grade where possible", placement = "bottom", trigger = "hover",options = NULL),
             
             actionButton("FollowUpType",label = "FollowUpType"),
              bsPopover ("FollowUpType", "Only press once the Prague score and Path stage buttons have extracted the relevant columns", placement = "bottom", trigger = "hover",options = NULL),
             
             actionButton("AllTheFunctions",label = "AllTheFunctions"),
             actionButton("SurveillanceTime",label="Surveillance Time", icon = icon("far fa-clock")),
              bsPopover ("SurveillanceTime", "Select the hospital number and the date of the procedure columns in order to get the time since the last test", placement = "bottom", trigger = "hover",options = NULL)
             ),
             DT::dataTableOutput("BarrettsTable")
          ),
          
          
          mainPanel(width = 100,
                    navbarPage("Barretts analytics",
                        tabPanel("Quality Metrics", sidebarPanel(width = 2,
                          #Select the column that has the endoscopists listed:
                          uiOutput("endoscopistCol"),
                          # Select the column with the worst grade:
                          uiOutput("worstGradeCol"),
                          # Select the column with the endoscopy documentation quality:
                          uiOutput("endoDoc_documentqual")
                          ),mainPanel(splitLayout(plotlyOutput("plotBarrQM"),plotlyOutput("plotBarrEQ")))),
                        tabPanel("Visualise",
                                 tags$div(
                                   style = "height: 700px;", # needs to be in fixed height container
                                   esquisserUI(
                                     id = "esquisseBarr",
                                     header = FALSE, # dont display gadget title
                                     choose_data = FALSE # dont display button to change data
                                   )
                                 )),
                        tabPanel("Cross Tabulate", style="overflow: visible",
                                 fluidRow(rpivotTableOutput("BarrPivot"))),
                        tabPanel("Endoscopy Utilisation", style="overflow: visible",
                                 sidebarPanel(
                                   uiOutput("Date_endoscopyutilisationBarr"),
                                   uiOutput("endoscopicEventBarr"),width = 2),
                                 mainPanel(verticalLayout(plotlyOutput("endoscopyUse_EndoscopyUseBarr"),
                                          plotlyOutput("plotBarrTSA")))),

                        tabPanel("Theograph", sidebarPanel(width = 2,
                          # Select variable for the hospital number
                          uiOutput("HospNumBarrTheo"),
                          # Select variable for the dates
                          uiOutput("DatesBarrTheo")
                        ),mainPanel(plotlyOutput("plotBarrPT")))
                    )
                        
            )
            
          ),

# Polyps  ----------------------------------------------------
    tabPanel("Polyps", tableOutput("table3"),
        bsCollapsePanel("Polyp Data", "", style = "info",
             DT::dataTableOutput("polypTable")
        ),
        mainPanel(width = 100,
          
          navbarPage("Polyp analytics",
                      tabPanel("Quality metrics(ADR)",  DT::dataTableOutput("GRS_Table")),
                     tabPanel("Visualise",
                              tags$div(
                                style = "height: 700px;", # needs to be in fixed height container
                                esquisserUI(
                                  id = "esquissePolyp",
                                  header = FALSE, # dont display gadget title
                                  choose_data = FALSE # dont display button to change data
                                )
                              )),

                     tabPanel("Cross Tabulate", style="overflow: visible",
                              fluidRow(rpivotTableOutput("OverallPivotPolyp"))),
                     tabPanel("Endoscopy Utilisation", style="overflow: visible",
                              sidebarPanel(
                                uiOutput("Date_endoscopyutilisationPolyp"),
                                uiOutput("endoscopicEventPolyp"),width = 2),
                              mainPanel(
                              fluidRow(plotlyOutput("endoscopyUse_EndoscopyUsePolyp"),
                                       plotlyOutput("plotPolypTSA")))),
                      tabPanel("Theograph", plotOutput("plotPolypPF"))
          )
        )
    ),

# IBD  ----------------------------------------------------
    tabPanel("IBD", tableOutput("tableIBD"),
             
        bsCollapsePanel("IBD Data", "", style = "info",
             box(status = "warning", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "Derive IBD data",br(), br(),
                 actionButton("IBD",label = "IBD")
              ),
             DT::dataTableOutput("IBD")
        ),
        mainPanel(width = 100,
          

          navbarPage("IBD analytics",
                      tabPanel("Quality Metrics", plotOutput("plotIBDQM")),
                     tabPanel("Visualise",
                              radioButtons(
                                inputId = "dataIBD",
                                label = "Data to use:",
                                choices = c("Custom"),
                                inline = TRUE
                              ),
                              tags$div(
                                style = "height: 700px;", # needs to be in fixed height container
                                esquisserUI(
                                  id = "esquisseIBD",
                                  header = FALSE, # dont display gadget title
                                  choose_data = FALSE # dont display button to change data
                                )
                              )),

                     tabPanel("Cross Tabulate", style="overflow: visible",
                              fluidRow(rpivotTableOutput("OverallPivotIBD"))),
                     tabPanel("Endoscopy Utilisation", style="overflow: visible",
                              uiOutput("Date_endoscopyutilisationIBD"),
                              fluidRow(plotlyOutput("endoscopyUse_TimeSeriesIBD"))),
                      tabPanel("Theograph", plotOutput("plotIBDTheo")),
                      tabPanel("Time Series Analysis", plotOutput("plotIBDTSA"))
          )
 
        )
    )
    )
  )
)
)