library(shiny)
library(rpivotTable)
library(EndoMineR)
library(shinydashboard)
library(shinythemes)
library(shinyFiles)
library(shinyBS)
library(dplyr)
library(esquisse)
library(jsmodule)
library(GGally)
library(plotly)





ui <-function(request) {fluidPage(theme=shinytheme("cosmo"),

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
                               
                               #########]]]]] File upload- endotable#############
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "A. Upload data",
                                   fileInput("FileIn_endoscopy",label="",multiple = FALSE),br()),
                               
                               
                               ###########]]]]] Endotable events-textPrep ############                               
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "B. Split the data",
                               textInput("caption", "", "Enter the comma separated headers here"),
                               actionButton("textPrep",label = "textPrep"),
                                bsPopover ("textPrep", "Enter the headers from the text separated by a comma to split according to the headers", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               
                               ###########]]]]] Endotable events-Date standardiser ###########
                              
                               actionButton("DateStandardiserEndo",label = "", icon = icon("far fa-calendar-alt")),
                               bsPopover ("DateStandardiserEndo", "Select only one date column then press the button", placement = "bottom", trigger = "hover",
                                          options = NULL),
                               
                               ###########]]]]] Endotable events-HospNum standardiser ###########
                               actionButton("HospitalNumberExtractorEndo",label = "", icon = icon("fas fa-barcode")),
                               bsPopover ("HospitalNumberExtractorEndo", "Select only one hospital number column then press the button to standardise", placement = "bottom", trigger = "hover",
                                          options = NULL)),
                               

                               
                               #########]]]]] Table Create- endotable#############
                               DT::dataTableOutput("endotable")),

# Dataset 2 ----------------------------------------------------              
               
                              
               bsCollapsePanel("Dataset 2", "", style = "info",
                               fluidRow(
                                 
                                 #########]]]]] File upload- pathtable#############
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "A. Upload  data",
                                   fileInput("pathology",label="",multiple = FALSE),br()),

                               ###########]]]]] pathTable events- textPrep ############  
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "B. Split data",
                               textInput("captionPath", "", "Enter the comma separated headers here"),
                               actionButton("textPrepPath",label = "textPrepPath"),
                                bsPopover ("textPrepPath", "Enter the headers from the text separated by a comma to split according to the headers", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               
                               ###########]]]]] pathTable events-Date standardiser ###########
                               actionButton("DateStandardiserEPath",label = "", icon = icon("far fa-calendar-alt")),
                               bsPopover ("DateStandardiserEPath", "Select only one date column then press the button", placement = "bottom", trigger = "hover",
                                          options = NULL),
                               
                               ###########]]]]] pathTable events-HospitalNumber standardiser ###########
                               actionButton("HospitalNumberExtractorPath",label = "", icon = icon("fas fa-barcode")),
                               bsPopover ("HospitalNumberExtractorPath", "Select only one hospital number column then press the button to standardise", placement = "bottom", trigger = "hover",
                                          options = NULL))),

                               #########]]]]] Table Create- pathTable#############
                               DT::dataTableOutput("pathTable")),

# Final Dataset  ----------------------------------------------------

               bsCollapsePanel("Final Dataset", "", style = "info",
                               fluidRow(
                                 
                                 #########]]]]] File upload- mergedtable#############
                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "A. Upload  data",
                                     fileInput("inFile_merged",label="",multiple = FALSE),br()),

                                 ###########]]]]]  Button -textPrep ###########
                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "B. Split data",
                                     textInput("captionMerge", "", "Enter the comma separated headers here"),
                                     actionButton("textPrepMerge",label = "textPrepMerge"),
                                      bsPopover  ("textPrepMerge", "Enter the headers from the text separated by a comma to split according to the headers", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     
                                     ############]]]]]  Button - Date standardiser############
                                     actionButton("DateStandardiserMerge",label = "", icon = icon("far fa-calendar-alt")),
                                     bsPopover ("DateStandardiserMerge", "Select only one date column then press the button", placement = "bottom", trigger = "hover",
                                                options = NULL),
                                     
                                     ############]]]]]  Button - Hospital standardiser############
                                     actionButton("HospitalNumberExtractorMerge",label = "", icon = icon("fas fa-barcode")),
                                     bsPopover ("HospitalNumberExtractorMerge", "Select only one hospital number column then press the button to standardise", placement = "bottom", trigger = "hover",
                                                options = NULL)),

                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "C. Clean columns",br(), br(),
                                     
                                     
                                     ############]]]]]  Button - Categorical standardiser############
                                     actionButton("CategoricalDataMerge",label = "", icon = icon("far fa-flushed")),
                                      bsPopover ("CategoricalDataMerge", "Select only one categorical column then press the button to standardise", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     
                                     ############]]]]]  Button - Numeric standardiser############
                                     actionButton("NumericDataMerge",label = "", icon = icon("fab fa-neos")),
                                      bsPopover ("NumericDataMerge", "Select only one numeric column then press the button to standardise", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     
                                     ############]]]]]  Button - Negex############
                                     actionButton("NegExMerge",label = "Negex" ),
                                      bsPopover ("NegExMerge", "Select only one text column to exclude all sentences with negative expressions", placement = "bottom", trigger = "hover",
                                               options = NULL)),
                                 
                                 ############]]]]]  Button - EndoMerge############
                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=FALSE,title = "Merging Functions",br(), br(),
                                     actionButton("Endomerge2",label = "Endomerge2"),
                                     bsPopover ("Endomerge2", "Make sure you have standarised both the date and hospital column in both the endoscopy and the pathology datasets, then press this button to get the datasets merged.", placement = "bottom", trigger = "hover",options = NULL),
                                     actionButton("MergeWithImages",label = "MergeWithImages"),
                                     bsPopover ("MergeWithImages", "Press here to merge with images. The images must be from a html export with hospital numbers and dates so they can be merged.", placement = "bottom", trigger = "hover",options = NULL),
                                     actionButton(inputId = "Del_row_head",label = "Delete selected rows"),
                                     bsPopover ("Del_row_head", "Select individual rows with the checkbox and then press here to delete from the dataset", placement = "bottom", trigger = "hover",options = NULL)
                                 ),

                                 ############]]]]]  Button - Biopsy Number standardiser############
                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "Derive new columns",br(), br(),
                                     actionButton("NumBxMerge",label = "",icon = icon("fas fa-microscope")),
                                      bsPopover ("NumBxMerge", "Select column (usually a macroscopic desc ription column from pathology) to extract the total number of biopsies", placement = "bottom", trigger = "hover",
                                               options = NULL),

                                     bsModal("NumBxModal", "Change name", "NumBxMerge", size = "small",
                                             textInput("new_name", "Enter the delimiter here:", "") ,

                                             footer = tagList(
                                               modalButton("Cancel"),
                                               actionButton("NumBxModal_ok", "OK")
                                             )
                                     ),
                                  
                                     ############]]]]]  Button - Biopsy size standardiser############
                                     actionButton("BxSizeMerge",label = "",icon = icon("fas fa-sort-numeric-up")),
                                      bsPopover ("BxSizeMerge", "Select column (usually a macroscopic description column from pathology) to extract the average biopsy size ", placement = "bottom", trigger = "hover",
                                               options = NULL),

                                     ############]]]]]  Button - Endoscopist standardiser############
                                     actionButton("EndoscEndoscopistMerge",label = "", icon = icon("user-md custom")),
                                      bsPopover ("EndoscEndoscopistMerge", "Standardise the endoscopist column", placement = "bottom", trigger = "hover",
                                                   options = NULL),
                                     
                                     ############]]]]]  Button - Medication standardiser############
                                     actionButton("EndoscMedsMerge",label = "",icon = icon("fas fa-pills")),
                                      bsPopover ("EndoscMedsMerge", "Select the medication column to extract medications", placement = "bottom", trigger = "hover",
                                                   options = NULL),
                                     
                                     ############]]]]]  Button - Instrument standardiser############
                                     actionButton("EndoscInstrumentMerge",label = "",icon = icon("stethoscope custom")),
                                      bsPopover ("EndoscInstrumentMerge", "Select the Instrument column to clean instrument names", placement = "bottom", trigger = "hover",
                                                   options = NULL),
                                     
                                     ############]]]]]  Button - EndoEvent standardiser############
                                     actionButton("EndoEvent",label = "EndoEvent",icon = icon("fas fa-sort-numeric-up")),
                                     bsPopover ("EndoEvent", "Select in order: Endoscopic Findings, ProcedurePerformed, Macroscopicdescription and Histology text", placement = "bottom", trigger = "hover",
                                                options = NULL),
                                     
                                     ############]]]]]  Button - EndoEvent column select for modal ############
                                     bsModal("EndoEventModal", "Change name", "EndoEvent", size = "small",
                                             uiOutput("EndoEventColSelect_colEndoFindings"),
                                             uiOutput("EndoEventColSelect_colProcPerf"),
                                             uiOutput("EndoEventColSelect_colMacroDescript"),
                                             uiOutput("EndoEventColSelect_colHistol"),
                                             footer = tagList(
                                               modalButton("Cancel"),
                                               uiOutput("EndoEventcol_sel"),
                                               actionButton("EndoEventModalbtn", "OK")
                                             )
                                     ),
                                     
                                     ############]]]]]  Button - Regex############
                                     actionButton("Regex",label = "",icon = icon("fas fa-sort-numeric-up")),
                                     bsPopover ("Regex", "Put in a regular expression or a keyowrd to derive a new column with those elements extracted so you can filter on them", placement = "bottom", trigger = "hover",
                                                options = NULL),
                                     bsModal("RegexColAdder", "Change name", "Regex", size = "small",
                                             textInput("regexSearch", "Enter the search term here", "") ,
                                             footer = tagList(
                                               modalButton("Cancel"),
                                               actionButton("regexSearch_ok", "OK")
                                             )
                                     ),
                                     
                                     ############]]]]]  Button- Remove Duplicates############
                                     actionButton("RemoveDuplicates",label = "",icon = icon("faa fa-twins")),
                                     bsPopover ("RemoveDuplicates", "Press to remove rows where one procedure has pathology reported for two procedures (eg merged pathology report when OGD and colonoscopy performed", placement = "bottom", trigger = "hover",
                                                options = NULL),
                                     bsModal("Rem_DupModal", "Remove Duplicates", "RemoveDuplicates", size = "small",
                                             uiOutput("ProcPerf_RemDepsChooser"),
                                             uiOutput("LexiconChecker_RemDupsChooser"),
                                             footer = tagList(
                                               modalButton("Cancel"),
                                               actionButton("RemovDupsModal_Okbtn", "OK")
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

                               
                               #########]]]]] Table Create- mergedtable#############
                               DT::dataTableOutput("mergedTable"))
               )

               ),
bookmarkButton()),





# Custom ----------------------------------------------------  


tabPanel("Custom", tableOutput("table53"),
         
         #########]]]]]  Table Create- CustomTable############# 
         bsCollapsePanel("Select columns from the Final Dataset and click to display here", "", style = "info",
                         fluidRow(
                           DT::dataTableOutput("CustomTable")
                         )
         ),
         mainPanel(width = 100,
                   navbarPage("Your analytics",
                              
                              ############]]]]]  CustomTable Visualisation Esquiss ############
                              tabPanel("Visualise",
                                       tags$div(
                                         style = "height: 700px;", # needs to be in fixed height container
                                         esquisserUI(
                                           id = "esquisseCustom",
                                           header = FALSE, # dont display gadget title
                                           choose_data = FALSE # dont display button to change data
                                         )
                                       )),

                              ############]]]]]  CustomTable CrossTabulate############
                              tabPanel("Cross Tabulate", style="overflow: visible",
                                       fluidRow(rpivotTableOutput("OverallPivot"))),
                              tabPanel("Endoscopy Utilisation", style="overflow: visible",
                                       sidebarPanel(
                                         
                                         ############]]]]]  CustomTable Chooser - EndoUtilisation Date  ############
                                       uiOutput("Date_endoscopyutilisationCustom"),
                                       
                                       ############]]]]]  CustomTable Chooser - EndoUtilisation Event  ############
                                       uiOutput("endoscopicEventCustom"),width = 2),
                                       
                                       ############]]]]]  CustomTable Plot EndoUtilisation   ############
                                       mainPanel(verticalLayout(plotlyOutput("endoscopyUse_EndoscopyUseCustom"),
                                                                
                                                                ############]]]]]  CustomTable Plot- TimeSeriesAnalysis ############
                                                                plotlyOutput("plotCustomTSA"))
                                       )),
                              tabPanel("Theograph", sidebarPanel(width = 2,
                                                                 # Select variable for the hospital number
                                                                 
                                                                 ############]]]]]  CustomTable Chooser - Theograph HospNum  ############
                                                                 uiOutput("HospNumCustomTheo"),
                                                                 
                                                                 ############]]]]]  CustomTable Chooser - Theograph Date  ############
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
             
             #########]]]]] Table Create- BarrettsTable#############  
             DT::dataTableOutput("BarrettsTable")
          ),
          
          
          mainPanel(width = 100,
                    navbarPage("Barretts analytics",
                        tabPanel("Quality Metrics", sidebarPanel(width = 2,
                          
                          ############]]]]]  BarrettsTable Event- EndoEvent standardiser############
                          actionButton("Barr_DDRbtn",label = "Barr_DDR",icon = icon("fas fa-sort-numeric-up")),
                          bsPopover ("Barr_DDRbtn", "Select in order: Endoscopic Findings, ProcedurePerformed, Macroscopicdescription and Histology text", placement = "bottom", trigger = "hover",
                                     options = NULL),
                          
                          ############]]]]]  BarrettsTable Event- EndoEvent DDR column select for modal ############
                          bsModal("Barr_DDRModal", "Change name", "Barr_DDRbtn", size = "small",
                                  uiOutput("Barr_DDRColSelect_colEndoscopist"),
                                  uiOutput("Barr_DDRColSelect_colHistol"),
                                  uiOutput("Barr_DDRColSelect_colEndoFindings"),
                                  footer = tagList(
                                    modalButton("Cancel"),
                                    uiOutput("Barr_DDRModalbtn"),
                                    actionButton("Barr_DDRModalbtn", "OK")
                                  )
                          )),
                          
                          mainPanel(verticalLayout(
                            
                            ############]]]]]  BarrettsTable Plot Quality Endoscopist vs Worst grade Plot  ############
                            plotlyOutput("plotBarrQM"),
                            
                            ############]]]]]  BarrettsTable Plot-Quality Documentation quality Plot ############
                            plotlyOutput("plotBarrEQ"),
                            
                            DT::dataTableOutput("BarrDDR_Table"),
                            #########]]]]]  Drilldown Table- BarrettsTable############# 
                            
                            DT::dataTableOutput("drilldownBarr")))),
                        
                        ############]]]]]  BarrTable Vislualisation Esquiss ############
                        tabPanel("Visualise",
                                 tags$div(
                                   style = "height: 700px;", # needs to be in fixed height container
                                   esquisserUI(
                                     id = "esquisseBarr",
                                     header = FALSE, # dont display gadget title
                                     choose_data = FALSE # dont display button to change data
                                   )
                                 )),
                        
                        
                        
                        ############]]]]]  BarrettsTable CrossTablulate  ############
                        tabPanel("Cross Tabulate", style="overflow: visible",
                                 fluidRow(rpivotTableOutput("BarrPivot"))),
                        

                        tabPanel("Endoscopy Utilisation", style="overflow: visible",
                                 sidebarPanel(
                                   
                                   ############]]]]]  BarrettsTable Chooser -Date EndoUtilisation############
                                   uiOutput("Date_endoscopyutilisationBarr"),
                                   
                                   ############]]]]]  BarrettsTable Chooser- TimeSeriesAnalysis Event  ############
                                   uiOutput("endoscopicEventBarr"),width = 2),
                                 
                                 ############]]]]]  BarrettsTable Chooser- TimeSeriesAnalysis Event  ############
                                 mainPanel(verticalLayout(plotlyOutput("endoscopyUse_EndoscopyUseBarr"),
                                                          
                                          ############]]]]]  BarrettsTable Plot-Time Series Analysis Plot ############
                                          plotlyOutput("plotBarrTSA")))),

                        tabPanel("Theograph", sidebarPanel(width = 2,
                          # Select variable for the hospital number
                          
                          ############]]]]]  BarrettsTable Chooser-Theograph Endoscopist  ############
                          uiOutput("HospNumBarrTheo"),
                          # Select variable for the dates
                          
                          ############]]]]]  BarrettsTable Chooser-Theograph Date  ############
                          uiOutput("DatesBarrTheo")
                          
                          ############]]]]]  BarrettsTable Plot-Theograph Plot ############
                        ),mainPanel(plotlyOutput("plotBarrPT")))
                    )
                        
            )
            
          ),

# Polyps  ----------------------------------------------------
    tabPanel("Polyps", tableOutput("table3"),
        bsCollapsePanel("Polyp Data", "", style = "info",
                        
             #########]]]]]  Table- polypTable#############  
             DT::dataTableOutput("polypTable")
        ),
        mainPanel(width = 100,
          
          navbarPage("Polyp analytics",
                      tabPanel("Quality metrics(ADR)", 
                               ############]]]]]  Button - EndoEvent standardiser############
                               actionButton("GRS_ADRbtn",label = "GRS_ADR",icon = icon("fas fa-sort-numeric-up")),
                               bsPopover ("GRS_ADRbtn", "Select in order: Endoscopic Findings, ProcedurePerformed, Macroscopicdescription and Histology text", placement = "bottom", trigger = "hover",
                                          options = NULL),
                               
                               ############]]]]]  Button - EndoEvent ADR column select for modal ############
                               bsModal("GRS_ADRModal", "Change name", "GRS_ADRbtn", size = "small",
                                       uiOutput("GRS_ADRColSelect_colProcPerf"),
                                       uiOutput("GRS_ADRColSelect_colEndoEndoscopist"),
                                       uiOutput("GRS_ADRColSelect_colMacroDescript"),
                                       uiOutput("GRS_ADRColSelect_colHistol"),
                                       footer = tagList(
                                         modalButton("Cancel"),
                                         uiOutput("ADRModalbtn"),
                                         actionButton("GRS_ADRModalbtn", "OK")
                                       )
                               ),
                               ############]]]]]  GRSTable Create ############
                               
                               DT::dataTableOutput("GRS_Table"),
                               
                               #########]]]]]  Drilldown Table- polypTable############# 
                               
                               DT::dataTableOutput("drilldown")),
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
                              
                              ############]]]]]  PolypTable CrossTabulate ############
                              fluidRow(rpivotTableOutput("OverallPivotPolyp"))),
                     tabPanel("Endoscopy Utilisation", style="overflow: visible",
                              sidebarPanel(
                                
                                ############]]]]]  PolypTable Chooser- EndoUtilisation Date Chooser ############
                                uiOutput("Date_endoscopyutilisationPolyp"),
                                
                                ############]]]]]  PolypTable Chooser- TimeSeries Analysis Event  ############
                                uiOutput("endoscopicEventPolyp"),width = 2),
                              mainPanel(
                              fluidRow(
                                       ############]]]]]  PolypTable Plot- EndoUtilisation Plot ############
                                       plotlyOutput("endoscopyUse_EndoscopyUsePolyp"),
                                       ############]]]]]  PolypTable Plot- TimeSeries Analysis Plot ############
                                       plotlyOutput("plotPolypTSA")))),
                      tabPanel("Theograph", plotOutput("plotPolypPF")),
                               tabPanel("Test", plotlyOutput("plot2"))
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

)}