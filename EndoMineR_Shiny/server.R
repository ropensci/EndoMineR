
library(shiny)
library(EndoMineR)
library(stringr)
library(stringi)
library(readxl)
library(DT)
library(shinyFiles)
library(lubridate)
library(data.table)
library(tidyr)
library(pander)
library(esquisse)
library(jsmodule)
library(shiny);library(DT);library(data.table);library(jstable)
library(plotly)
library(ggTimeSeries)
library(shinyWidgets)
library(profvis)


# Define server logic required to draw a histogram
options(shiny.maxRequestSize=30*1024^2) 
options(shiny.sanitize.errors = TRUE)
enableBookmarking(store = "url")

RV <- reactiveValues(data = data.frame())
RV2 <- reactiveValues(data = data.frame())
RV3 <- reactiveValues(data = data.frame())
RV4 <- reactiveValues(data = data.frame())
Trim <- reactiveValues(data = data.frame())
pivotData<-reactiveValues(data = data.frame())
RV5 <- reactiveValues(data = data.frame())
polypData <- reactiveValues(data = data.frame())
polypTrim <- reactiveValues(data = data.frame())
BarrDDR_TableData <- reactiveValues(data = data.frame())
BarrTrim <- reactiveValues(data = data.frame())
BarrDDR_Table <- reactiveValues(data = data.frame())
CustomData <- reactiveValues(data = data.frame())
CustomTrim <- reactiveValues(data = data.frame())
GRS_TableData <- reactiveValues(data = data.frame())



server <- function(input, output,session) {


  
  setBookmarkExclude(c(  
  "endotable_rows_current", "endotable_cell_clicked","endotable_search", "endotable_rows_selected", "endotable_rows_all", "endotable_state","endotable_columns_selected","endotable_search_columns",
  "pathTable_rows_current", "pathTable_cell_clicked","pathTable_search", "pathTable_rows_selected", "pathTable_rows_all", "pathTable_state","pathTable_columns_selected","pathTable_search_columns",
  "mergedTable_rows_current", "mergedTable_cell_clicked","mergedTable_search", "mergedTable_rows_selected", "mergedTable_rows_all", "mergedTable_state","mergedTable_columns_selected","mergedTable_search_columns",
  "BarrettsTable_rows_current", "BarrettsTable_cell_clicked","BarrettsTable_search", "BarrettsTable_rows_selected", "BarrettsTable_rows_all", "BarrettsTable_state","BarrettsTable_columns_selected","BarrettsTable_search_columns",
  "BarrDDR_Table_rows_current", "BarrDDR_cell_clicked","BBarrDDR_Table_search", "BarrDDR_Table_rows_selected", "BarrDDR_Table_rows_all", "BarrDDR_Table_state","BarrDDR_Table_columns_selected","BarrDDR_search_columns",
  "CustomTable_rows_current", "CustomTable_cell_clicked","CustomTable_search", "CustomTablee_rows_selected", "CustomTable_rows_all", "CustomTable_state","CustomTable_columns_selected","CustomTable_search_columns",
  "polypTable_rows_current", "polypTable_cell_clicked","polypTable_search", "polypTable_rows_selected", "polypTable_rows_all", "polypTable_state","polypTable_columns_selected","polypTable_search_columns",
  "polypTrim_rows_current", "polypTrim_cell_clicked","polypTrim_search", "polypTrim_rows_selected", "polypTrim_rows_all", "polypTrim_state","polypTrim_columns_selected","polypTrim_search_columns",
  "GRS_Table_rows_current", "GRS_Table_cell_clicked","GRS_Table_search", "GRS_Table_rows_selected", "GRS_Table_rows_all", "GRS_Table_state","GRS_Table_columns_selected","GRS_Table_search_columns",
  "drilldown_rows_current", "drilldown_cell_clicked","drilldown_search", "drilldown_rows_selected", "drilldown_rows_all", "drilldown_state","drilldown_columns_selected","drilldown_search_columns",
  "drilldownBarr_rows_current", "drilldownBarr_cell_clicked","drilldownBarr_search", "drilldownBarr_rows_selected", "drilldownBarr_rows_all", "drilldownBarr_state","drilldownBarr_columns_selected","drilldownBarr_search_columns",
  "mergedTable_rows_current", "mergedTable_cell_clicked","mergedTable_search", "mergedTable_rows_selected", "mergedTable_rows_all", "mergedTable_state","mergedTable_columns_selected","mergedTable_search_columns",
  "esquisseBarr-controls-adjust","esquisseBarr-controls-bins","esquisseBarr-controls-caption","esquisseBarr-controls-code-holderCode","esquisseBarr-controls-code-insert_code",
  "esquisseBarr-controls-export_png","esquisseBarr-controls-export_ppt","esquisseBarr-controls-fill_color","esquisseBarr-controls-flip","esquisseBarr-controls-legend_position",
  "esquisseBarr-controls-palette","esquisseBarr-controls-position","esquisseBarr-controls-scale","esquisseBarr-controls-size","esquisseBarr-controls-smooth_add","esquisseBarr-controls-smooth_span",
  "esquisseBarr-controls-subtitle","esquisseBarr-controls-theme","esquisseBarr-controls-title","esquisseBarr-controls-x","esquisseBarr-controls-y","esquisseBarr-dragvars","esquisseBarr-geom-auto",
  "esquisseBarr-geom-bar","esquisseBarr-geom-boxplot","esquisseBarr-geom-btn-action","esquisseBarr-geom-density","esquisseBarr-geom-histogram","esquisseBarr-geom-line","esquisseBarr-geom-point",
  "esquisseBarr-geom-sf","esquisseBarr-geom-tile","esquisseBarr-geom-violin","esquisseBarr-play_plot","esquisseCustom-controls-adjust","esquisseCustom-controls-bins","esquisseCustom-controls-caption",
  "esquisseCustom-controls-code-holderCode","esquisseCustom-controls-code-insert_code","esquisseCustom-controls-export_png","esquisseCustom-controls-export_ppt","esquisseCustom-controls-fill_color",
  "esquisseCustom-controls-flip","esquisseCustom-controls-legend_position","esquisseCustom-controls-palette","esquisseCustom-controls-position","esquisseCustom-controls-scale",
  "esquisseCustom-controls-size","esquisseCustom-controls-smooth_add","esquisseCustom-controls-smooth_span","esquisseCustom-controls-subtitle","esquisseCustom-controls-theme",
  "esquisseCustom-controls-title","esquisseCustom-controls-x","esquisseCustom-controls-y","esquisseCustom-dragvars","esquisseCustom-geom-auto","esquisseCustom-geom-bar",
  "esquisseCustom-geom-boxplot","esquisseCustom-geom-btn-action","esquisseCustom-geom-density","esquisseCustom-geom-histogram","esquisseCustom-geom-line",
  "esquisseCustom-geom-point","esquisseCustom-geom-sf","esquisseCustom-geom-tile","esquisseCustom-geom-violin","esquisseCustom-play_plot","esquisseIBD-controls-adjust",
  "esquisseIBD-controls-bins","esquisseIBD-controls-caption","esquisseIBD-controls-code-holderCode","esquisseIBD-controls-code-insert_code","esquisseIBD-controls-export_png",
  "esquisseIBD-controls-export_ppt","esquisseIBD-controls-fill_color","esquisseIBD-controls-flip","esquisseIBD-controls-legend_position","esquisseIBD-controls-palette",
  "esquisseIBD-controls-position","esquisseIBD-controls-scale","esquisseIBD-controls-size","esquisseIBD-controls-smooth_add","esquisseIBD-controls-smooth_span",
  "esquisseIBD-controls-subtitle","esquisseIBD-controls-theme","esquisseIBD-controls-title","esquisseIBD-controls-x","esquisseIBD-controls-y","esquisseIBD-dragvars",
  "esquisseIBD-geom-auto","esquisseIBD-geom-bar","esquisseIBD-geom-boxplot","esquisseIBD-geom-btn-action","esquisseIBD-geom-density","esquisseIBD-geom-histogram",
  "esquisseIBD-geom-line","esquisseIBD-geom-point","esquisseIBD-geom-sf","esquisseIBD-geom-tile","esquisseIBD-geom-violin","esquisseIBD-play_plot","esquissePolyp-controls-adjust",
  "esquissePolyp-controls-bins","esquissePolyp-controls-caption","esquissePolyp-controls-code-holderCode","esquissePolyp-controls-code-insert_code","esquissePolyp-controls-export_png",
  "esquissePolyp-controls-export_ppt","esquissePolyp-controls-fill_color","esquissePolyp-controls-filter-data-dovydhyzhd_Date_x","esquissePolyp-controls-filter-data-dovydhyzhd_Date_x_na_remove",
  "esquissePolyp-controls-flip","esquissePolyp-controls-legend_position","esquissePolyp-controls-palette","esquissePolyp-controls-position","esquissePolyp-controls-scale",
  "esquissePolyp-controls-size","esquissePolyp-controls-smooth_add","esquissePolyp-controls-smooth_span","esquissePolyp-controls-subtitle","esquissePolyp-controls-theme",
  "esquissePolyp-controls-title","esquissePolyp-controls-x","esquissePolyp-controls-y","esquissePolyp-dragvars","esquissePolyp-geom-auto","esquissePolyp-geom-bar","esquissePolyp-geom-boxplot",
  "esquissePolyp-geom-btn-action","esquissePolyp-geom-density","esquissePolyp-geom-histogram","esquissePolyp-geom-line","esquissePolyp-geom-point","esquissePolyp-geom-sf",
  "esquissePolyp-geom-tile","esquissePolyp-geom-violin","esquissePolyp-play_plot"))  
  
  
  ############# Home Page ###################
  #Split up the dataframe with textPrep
  
  
  
  
  
  
  #########:::::File upload- endotable#############
  observe({
    inFile_endoscopy <- input$FileIn_endoscopy
    if (!is.null(inFile_endoscopy)) {   
      dataFile <- read_excel(inFile_endoscopy$datapath, sheet=1)
      dat <- data.frame(EndoPaste(dataFile)[1], stringsAsFactors=FALSE)
      RV$data<-dat
      
    }
  })
  
  
  
  
  
  #########:::::Table Create- endotable#############
  output$endotable = DT::renderDT({

    RV$data
   
  },selection = list(target = 'column'),extensions = 'Buttons', 
   options = list(
    fixedHeader=TRUE,
    scrollX = TRUE,
    scrollY = TRUE,
    pageLength = 5,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')))

  
  
  
  #########:::::File upload- pathtable#############
  
  observe({
    inFile_path <- input$pathology
    if (!is.null(inFile_path)) {   
      dataFile <- read_excel(inFile_path$datapath, sheet=1)
      dat <- data.frame(EndoPaste(dataFile)[1], stringsAsFactors=FALSE)
      RV2$data<-dat
      
    }
  })
  
  
  
  #########:::::Table Create- pathTable#############
  output$pathTable = DT::renderDT({
    RV2$data
  },selection = list(target = 'column'),extensions = 'Scroller',options = list(scrollX = TRUE,pageLength = 5,
                                                       dom = 'Bfrtip',
                                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')))
  
 
  

  
  #########:::::File upload- mergedtable#############
  
  observe({
    inFile_merged <- input$inFile_merged
    if (!is.null(inFile_merged)) {   
      dataFile <- read_excel(inFile_merged$datapath, sheet=1)
      dat <- data.frame(EndoPaste(dataFile)[1], stringsAsFactors=FALSE)
      RV3$data<-dat
    }
  })
  
  
  #########:::::Table Create- mergedtable#############
  output$mergedTable = DT::renderDT({
    shiny::validate(
      need(nrow(RV3$data) > 0, "Make sure you have selected date and hospital number in both datasets then merge")
    ) 
    if (!is.null(RV3$data)) {  
      
      
      
      RV3$data[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(RV3$data),'"><br>')
      
      RV3$data[["Actions"]]<-
        paste0('
               <div class="btn-group" role="group" aria-label="Basic example">
               <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(RV3$data),'>Delete</button>
               </div>
               ')
    }
    
    
    datatable(RV3$data,escape=F, extensions = c("Select","Buttons"), selection = "none",callback = JS( "var ncols = table.columns().count();",
                                                                                                       "var tbl = table.table().node();",
                                                                                                       "var tblID = $(tbl).closest('.datatables').attr('id');",
                                                                                                       "table.on('click', 'tbody td', function(){",
                                                                                                       "  // if the column is selected, deselect it:",
                                                                                                       "  if(table.column(this, {selected: true}).length){",
                                                                                                       "    table.column(this).deselect();",
                                                                                                       "  // otherwise, select the column unless it's among the last two columns:",
                                                                                                       "  } else if([ncols-1, ncols-2].indexOf(table.column(this).index()) === -1){",
                                                                                                       "    table.column(this).select();",
                                                                                                       "  }",
                                                                                                       "  // send selected columns to Shiny",
                                                                                                       "  var indexes = table.columns({selected:true}).indexes();",
                                                                                                       "  var indices = Array(indexes.length);",
                                                                                                       "  for(var i = 0; i < indices.length; ++i){",
                                                                                                       "    indices[i] = indexes[i];",
                                                                                                       "  }",
                                                                                                       "  Shiny.setInputValue(tblID + '_columns_selected', indices);",
                                                                                                       " var checkboxes = document.getElementsByName('row_selected');",
                                                                                                       "  var checkboxesChecked = [];",
                                                                                                       " for (var i=0; i<checkboxes.length; i++) {",
                                                                                                       "    if (checkboxes[i].checked) {",
                                                                                                       "   checkboxesChecked.push(checkboxes[i].value);",
                                                                                                       "    }",
                                                                                                       "   }",
                                                                                                       " Shiny.onInputChange('checked_rows',checkboxesChecked);",
                                                                                                       "});"),
              
              options = list(
                scrollX = TRUE,
                scrollY = TRUE,
                pageLength = 200,
                select = "api",
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'))
    )
    
    
    })
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  #For the working set custom table- reactive to new columns
  observeEvent(input$polypTable_rows_all,{
    if (length(input$polypTable_columns_selected)>1){
    polypTrim$data<- polypData$data[input$polypTable_rows_all, input$polypTable_columns_selected]
    } else(polypTrim$data<-NULL)
  },ignoreInit = TRUE)
  

  
  
  ########### Endotable events ############  

  
  
  
  ###########:::::Button-Date standardiser ###########
  
  #Standardise the date Dataset 1
  observeEvent(input$DateStandardiserEndo,{
    RV$data[,as.numeric(input$endotable_columns_selected)]<-parse_date_time(str_extract(RV$data[,as.numeric(input$endotable_columns_selected)],
                                                                                        "(\\d{4}[[:punct:]]\\d{2}[^:alnum:]\\d{2})|(\\d{2}[^:alnum:]\\d{2}[^:alnum:]\\d{4})"),
                                                                            orders = c("dmy", "ymd"))
  },ignoreInit = TRUE) 
  
  
  ###########:::::Button-HospNum standardiser ###########
  
  #Standardise the Hospital Number Dataset 1
  observeEvent(input$HospitalNumberExtractorEndo,{
    RV$data[,as.numeric(input$endotable_columns_selected)]<-str_extract(RV$data[,as.numeric(input$endotable_columns_selected)],
                                                                        "([a-z0-9]\\d{4,}[a-z0-9])")
    
  },ignoreInit = TRUE)
  
  
  
  ###########:::::Button-textPrep ###########
   
  observeEvent(input$textPrep,{
   # browser()
    mywordsOGD<-input$caption
    mywordsOGD<-unlist(strsplit(mywordsOGD,","))
    
   
    RV$data<-textPrep(RV$data[,1],mywordsOGD)
    
    #Try type conversion here:
    RV$data<-type.convert(RV$data)
  
    
  },ignoreInit = TRUE)
 

 
  
  
  
  
  observeEvent(input$Del_row_head,{
    row_to_del=as.numeric(gsub("Row","",input$checked_rows))
    RV3$data=RV3$data[-row_to_del,]}
  )
  
  
  observeEvent(input$MergeImages,{
    input$captionDelim
    file_selected<-parseFilePaths(volumes, input$Btn_GetFile)
    folder_selected<-parseDirPath(volumes, input$folder)
    
    Imgdf<-MyImgLibrary(file_selected$datapath,
                input$captionDelim,folder_selected)
    #Now merge the Imgdf with RV3$data and make this RV$data so it can be displayed
    Imgdf$PatientID<-tolower(Imgdf$PatientID)
    
    colnames(Imgdf)[which(names(Imgdf) == "PatientID")] <- "HospitalNum"
    colnames(Imgdf)[which(names(Imgdf) == "Endo_ResultEntered")] <- "Date"
    Imgdf$Date <- gsub("\n", "", Imgdf$Date)
    Imgdf$Date <- as.Date(Imgdf$Date)
   
    names(RV3$data)[as.numeric(input$mergedTable_columns_selected[1])]<- "HospitalNum"
    names(RV3$data)[as.numeric(input$mergedTable_columns_selected[2])]<- "Date"
    RV3$data$Date<-as.Date(RV3$data$Date)
    #No need for fuzzy join here as images are from the endoscopy- may need to change this with other images though
    RV3$data<-left_join(RV3$data,Imgdf,by = c("Date","HospitalNum"), copy = FALSE)
    
    return(RV3$data)
    }
  )
  
  

  
#Delete rows with the delete picker
  observeEvent(input$lastClick,
               {
                 if (input$lastClickId%like%"delete")
                 {
                   row_to_del=as.numeric(gsub("delete_","",input$lastClickId))
                   RV3$data=RV3$data[-row_to_del,]
                 }
                 else if (input$lastClickId%like%"modify")
                 {
                   showModal(modal_modify)
                 }
               }
  )
  
  volumes = getVolumes()
  
  #Show the file
  observe({  
    shinyFileChoose(input, "Btn_GetFile", roots = volumes)
    if(!is.null(input$Btn_GetFile)){
      file_selected<-parseFilePaths(volumes, input$Btn_GetFile)
      output$txt_file <- renderText(as.character(file_selected$datapath))
    }
  })
  
  

  
  observe({  
    shinyDirChoose(input, 'folder', roots=volumes)
    if(!is.null(input$folder)){
      
      dir <- reactive(input$folder)
      output$folder_file <- renderText({parseDirPath(volumes, input$folder)})
          }
  })

  

  
  
  ########### pathTable events ############  
  
  
  
  ###########::::: Button-Date standardiser ###########
  
  #Standardise the date pathTable
  observeEvent(input$DateStandardiserEPath,{
    RV2$data[,as.numeric(input$pathTable_columns_selected)]<-parse_date_time(str_extract(RV2$data[,as.numeric(input$pathTable_columns_selected)],
                                                                                         "(\\d{4}[[:punct:]]\\d{2}[^:alnum:]\\d{2})|(\\d{2}[^:alnum:]\\d{2}[^:alnum:]\\d{4})"),
                                                                             orders = c("dmy", "ymd"))
  },ignoreInit = TRUE)
  
  
  
  
  
  
  ###########::::: Button-HospitalNumber standardiser ###########
  
  #Standardise the Hospital Number pathTable
  observeEvent(input$HospitalNumberExtractorPath,{
    RV2$data[,as.numeric(input$pathTable_columns_selected)]<-str_extract(RV2$data[,as.numeric(input$pathTable_columns_selected)],
                                                                         "([a-z0-9]\\d{4,}[a-z0-9])")
  },ignoreInit = TRUE) 
  

  
  
  
  ###########::::: Button-Categorical standardiser ###########
  
  #Standardise the Categorical as categorical in Pathology
  observeEvent(input$CategoricalDataPath,{
    RV$data[,as.numeric(input$pathTable_columns_selected)]<-as.factor(RV$data[,as.numeric(input$pathTable_columns_selected)])
    
  },ignoreInit = TRUE)
  
  
  ###########:::::Button-textPrep ###########
  
  observeEvent(input$textPrepPath,{
    mywordsOGD<-input$captionPath
    mywordsOGD<-unlist(strsplit(mywordsOGD,","))
    RV2$data<-textPrep(RV2$data[,1],mywordsOGD)
    
    #Try type conversion here:
    RV2$data<-type.convert(RV2$data)
    
  },ignoreInit = TRUE)
  
  
  

  

  ########### Mergetable events ############  
  
  ###########::::: Button-textPrep ###########
  
  observeEvent(input$textPrepMerge,{
    mywordsOGD<-input$captionMerge
    mywordsOGD<-unlist(strsplit(mywordsOGD,","))
    RV3$data<-textPrep(RV3$data[,1],mywordsOGD)
    
    #Try type conversion here:
    RV3$data<-type.convert(RV3$data)
    
  },ignoreInit = TRUE)
  
  ############::::: Button- EndoMerge############
  
  observeEvent(input$Endomerge2,{
    #Merge the patientID column and date from each table. Make sure that the patient ID is chosen first;
    #Need to fix this to understand when it is selecting the number. I think the user needs to 
    #convert to date and then select columns (date first) at one sitting with the datatable.
    
    #browser()
    
    RV3$data<-Endomerge2(RV$data,colnames(RV$data[as.numeric(input$endotable_columns_selected[1])]),
                         colnames(RV$data[as.numeric(input$endotable_columns_selected[2])]),
                         RV2$data,
                         colnames(RV2$data[as.numeric(input$pathTable_columns_selected[1])]),
                         colnames(RV2$data[as.numeric(input$pathTable_columns_selected[2])]))
    
    RV4$data<- RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
    
    RV3$data<-RV3$data[,1:ncol(RV3$data)-1]
    

  },ignoreInit = TRUE)
  
  ############::::: Button- Date standardiser############
  
  #Standardise the date
  observeEvent(input$DateStandardiserMerge,{
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-parse_date_time(str_extract(RV3$data[,as.numeric(input$mergedTable_columns_selected)],
                                                                                           "(\\d{4}[[:punct:]]\\d{2}[^:alnum:]\\d{2})|(\\d{2}[^:alnum:]\\d{2}[^:alnum:]\\d{4})"),
                                                                               orders = c("dmy", "ymd"))
  },ignoreInit = TRUE)
  
  ############::::: Button- Hospital standardiser############
  
  #Standardise the Hospital Number Merge
  observeEvent(input$HospitalNumberExtractorMerge,{
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-str_extract(RV3$data[,as.numeric(input$mergedTable_columns_selected)],
                                                                         "([a-z0-9]\\d{4,}[a-z0-9])")
  },ignoreInit = TRUE) 
  
  ############::::: Button- Categorical standardiser############
  
  #Standardise the Categorical data
  observeEvent(input$CategoricalDataMerge,{
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-as.factor(RV3$data[,as.numeric(input$mergedTable_columns_selected)])
  },ignoreInit = TRUE)
  
  ############::::: Button- Numeric standardiser############
  
  #Standardise the Numbers as numeric in Endoscopy
  observeEvent(input$NumericDataMerge,{
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-as.numeric(str_extract(RV3$data[,as.numeric(input$mergedTable_columns_selected)],
                                                                                      "[0-9]+"))

  },ignoreInit = TRUE)
  
  
  ############::::: Button- Negex############
  
  #Negex Remove
  observeEvent(input$NegExMerge,{
    standardisedTextOutput<-str_split(RV3$data[,as.numeric(input$mergedTable_columns_selected)], "\\.")
    standardisedTextOutput<-lapply(standardisedTextOutput, function(x) NegativeRemove(x))

    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-unlist(lapply(standardisedTextOutput, function(x) paste0(unlist(x),collapse=" ")))

    
  },ignoreInit = TRUE)
  
  

  ############::::: Button- Regex############
  
  observeEvent(input$regexSearch_ok,{
    #Need to bring up modal from here first to get the input$new_name as a renderui component
    #browser()
    myInterim<-str_extract_all(RV3$data[,as.numeric(input$mergedTable_columns_selected)],input$regexSearch)
    NewCol<-as.character(lapply(myInterim,function(x) unlist(x)))
    
    RV3$data<-cbind(NewCol,RV3$data)
    
    
  },ignoreInit = TRUE)
  
  
  
  
  
  
  
  
  
  ############::::: Button- Endoscopist standardiser############
  
  
  #Extract the endoscopist
  observeEvent(input$EndoscEndoscopistMerge,{
    #browser()
    #RV$data[,as.numeric(input$endotable_columns_selected)]
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-EndoscEndoscopist(RV3$data[,as.numeric(input$mergedTable_columns_selected)])

  },ignoreInit = TRUE)
  
  
  ############::::: Button- Medication standardiser############
  
  #Extract the medication  
  observeEvent(input$EndoscMedsMerge,{
    RV3$data<-cbind(EndoscMeds(RV3$data[,as.numeric(input$mergedTable_columns_selected)]),RV3$data)

  },ignoreInit = TRUE)
  
  
  ############::::: Button- Instrument standardiser############
  
  #Extract the instrument
  observeEvent(input$EndoscInstrumentMerge,{
    RV3$data$mergedTable_columns_selected<-EndoscInstrument(RV3$data[,as.numeric(input$mergedTable_columns_selected)])

  },ignoreInit = TRUE)
  
  
  
  ############::::: Button- EndoEvent standardiser############
  
  #Extract the endoscopic events
  observeEvent(input$EndoEventModalbtn,{
    RV3$data$EndoscopyEvent<-EndoscopyEvent(RV3$data, input$EndoEventcol_selFindings, 
    input$EndoEventColSelect_colProcPerf,
    input$EndoEventColSelect_colMacroDescript, 
    input$EndoEventColSelect_colHistol)

      },ignoreInit = TRUE)
  
  ############::::: Chooser- EndoEvent column select for modal ############
  
  output$EndoEventColSelect_colEndoFindings <- renderUI(
    selectInput("EndoEventcol_selFindings","Select the Endoscopy Findings column", choices= 
                  colnames(RV3$data))
  )
  
  output$EndoEventColSelect_colProcPerf <- renderUI(
    selectInput("EndoEventcol_selProcPerf","Select the Procedure performed column", choices= 
                  colnames(RV3$data))
  )
  
  output$EndoEventColSelect_colMacroDescript <- renderUI(
    selectInput("EndoEventcol_selMacroDescript","Select the Macroscopic findings column", choices= 
                  colnames(RV3$data))
  )
  
  output$EndoEventColSelect_colHistol <- renderUI(
    selectInput("EndoEventcol_selHistol","Select the Histology column", choices= 
                  colnames(RV3$data))
  )

  ############::::: Button- Remove duplicates############
  
  #Extract the endoscopic events
  observeEvent(input$RemovDupsModal_Okbtn,{
    #browser()
    

    
    
    #Get the column with the procedure in it:
    #input$ProcPerf_RemDepsChooserIn
    
    #Get the column with the list of columns to look in and merge the cols together
    dd<-unlist(input$LexiconChecker_RemDupsChooserIn)
    
    #Merge the chosen columns together
    dd2<-tidyr::unite(RV3$data,dd)
    
    #Add the merged columns back to the dataframe
    RV3$data$mergedColsToCheck<- as.character(dd2$dd)
   
    #Extract all the terms that were found from the upper list
    RV3$data$LocationMatch<-as.character(lapply(str_extract_all(RV3$data$mergedColsToCheck,tolower(paste0(unlist(LocationListUpper(),use.names=F),collapse="|"))),function(x) unlist(paste(x,collapse=""))))
    
    #Now exclude any rows that have upper strings in them and are listed as being colons or flexis                               
    RV3$data<-RV3$data[!((RV3$data$LocationMatch!="") &(grepl("colon|flexi",as.character(RV3$data[,input$ProcPerf_RemDepsChooserIn])))),]
    RV3$data$LocationMatch<-NULL
    
    
    #Extract all the terms that were found from the upper list
    RV3$data$LocationMatch<-as.character(lapply(str_extract_all(RV3$data$mergedColsToCheck,tolower(paste0(unlist(LocationListLower(),use.names=F),collapse="|"))),function(x) unlist(paste(x,collapse=""))))
    
    #Now exclude any rows that have upper strings in them and are listed as being colons or flexis   - NEED TO CHANGE THIS AS SOME PATIENTS DONT HAVE PATHOLOGY SAMPLES BUT SHOULDNT BE EXCLUDED....                            
    RV3$data<-RV3$data[!((RV3$data$LocationMatch!="") &(grepl("ogd|gastrosc",as.character(RV3$data[,input$ProcPerf_RemDepsChooserIn])))),]
    RV3$data$LocationMatch<-NULL
    
    
  
    RV3$data<-RV3$data[!duplicated(RV3$data[,1:(ncol(RV3$data)-5)]), ]
    
    
    
    #Get the procedure performed from a modal and get the other columns (as many as you want) to be merged together.
    #Delete row where there is evidence of a colonoscopy or a flexible sigmoidoscopy containing any words in the lexicon for LocationListUpper
    #Delete row where there is evidence of a gastroscopy/ERCP/ containing any words in the lexicon for LocationListLower
    
    
    
  })
 
  
  
  output$ProcPerf_RemDepsChooser <- renderUI(
    selectInput("ProcPerf_RemDepsChooserIn","Select the Procedure Performed column", choices= 
                  colnames(RV3$data))
  )
  
  output$LexiconChecker_RemDupsChooser <- renderUI(
    multiInput(inputId = "LexiconChecker_RemDupsChooserIn", label = "Select the columns to check are appropriate for the Procedure performed",
               choices = colnames(RV3$data), width = "350px"
    )
  )
  
  
  
  
  
    
    
    
  ############::::: Button- Biopsy Number standardiser############
  
  
  #Extract the number of biopsies  
  observeEvent(input$NumBxModal_ok,{
    #Need to bring up modal from here first to get the input$new_name as a renderui component
    RV3$data$NumBx<-HistolNumbOfBx(RV3$data[,as.numeric(input$mergedTable_columns_selected)],input$new_name)
  },ignoreInit = TRUE)
  
  
  ############::::: Button- Biopsy size standardiser############
  
  #Extract the biopsy size
  observeEvent(input$BxSizeMerge,{
    RV3$data$BxSize<-HistolBxSize(RV3$data[,as.numeric(input$mergedTable_columns_selected)])
  },ignoreInit = TRUE) 
  
  
 
  ############## Custom Table ###############
  
  #########::::: Table Create- CustomTable############# 
  #Trimmed from the mergedTable data sets:
  output$CustomTable = DT::renderDT({
    #browser()
    
    CustomData$data<-RV3$data
    
  },filter = 'top',selection = list(target = 'column'),extensions = 'Buttons', 
  options = list(
    fixedHeader=TRUE,
    scrollX = TRUE,
    scrollY = TRUE,
    pageLength = 5,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')))
  
  
  
  
  
  
  #########::::: Working Table- CustomTable#############  
  #For the working set custom table- reactive to new columns
  observeEvent(input$CustomTable_columns_selected,{
    #browser()
    if (length(input$CustomTable_columns_selected)>1){
      CustomTrim$data<- CustomData$data[input$CustomTable_rows_all, input$CustomTable_columns_selected]
    } else(CustomTrim$data<-NULL)
    
  },ignoreInit = TRUE)
  
  #For the working set custom table- reactive to filtered rows: 
  
  #For the working set custom table- reactive to new columns
  observeEvent(input$CustomTable_rows_all,{
    if (length(input$CustomTable_columns_selected)>1){
      CustomTrim$data<- CustomData$data[input$CustomTable_rows_all, input$CustomTable_columns_selected]
    } else(CustomTrim$data<-NULL)
  },ignoreInit = TRUE)
  
  
  
  
  
  
  
  ############:::::  Visualisation Esquiss ############
  
  callModule(module = esquisserServer, id = "esquisseCustom", data = CustomTrim)
  
  
  ############:::::  CrossTabulate############
  
  output$OverallPivot <- renderRpivotTable({

    rpivotTable(CustomTrim$data)
        

  })
  
  ############:::::  Chooser - EndoUtilisation Date  ############
  
  output$Date_endoscopyutilisationCustom<-renderUI({
    selectInput("Date_endoscopyutilisationChooserCustom", label = h4("Choose the column showing the date"),
                choices = colnames(CustomTrim$data) ,selected = 1
    )
  })
  

  
  
  
  ############:::::  Plot EndoUtilisation   ############
  
  output$endoscopyUse_EndoscopyUseCustom <- renderPlotly({
    #Create the grouped table here of the number of endoscopies done by day
    if(nrow(CustomTrim$data>0)){
      if(ncol(CustomTrim$data>0)){
    #Then perform as per below
    dtData<-CustomTrim$data%>% group_by(!!rlang::sym(input$Date_endoscopyutilisationChooserCustom)) %>% dplyr::summarise(n = n())
    # base plot
    
    #Get rid of NA's as they mess things up.
    dtData<-na.omit(as.data.table(dtData))
    
    p1 = ggplot_calendar_heatmap(
      dtData,
      input$Date_endoscopyutilisationChooserCustom,
      'n'
    )
    
    # adding some formatting
    p1 + 
      xlab('') + 
      ylab('') + 
      scale_fill_continuous(low = 'green', high = 'red') + 
      facet_wrap(~Year, ncol = 1)
      }}
  })
  
  
  ############:::::  Chooser - EndoUtilisation Event  ############
  
  output$endoscopicEventCustom<-renderUI({
    selectInput("endoscopicEventColChooserCustom", label = h4("Choose the column containing the events of interest"),
                choices = colnames(CustomTrim$data) ,selected = 1
    )
  })
  


  
  
  
  ############:::::  Plot- TimeSeriesAnalysis ############
  
  
  output$plotCustomTSA <- renderPlotly({

    
    if(nrow(CustomTrim$data>0)){
      if(ncol(CustomTrim$data>0)){
    Endo_ResultPerformeda <- sym(input$Date_endoscopyutilisationChooserCustom)
    TestNumbers <-
      CustomTrim$data %>% group_by(!! rlang::sym(input$endoscopicEventColChooserCustom)) %>% 
      arrange(as.Date(!!Endo_ResultPerformeda)) %>% group_by(
        week = week(as.Date(!!Endo_ResultPerformeda)),
        month = month(as.Date(!!Endo_ResultPerformeda)),
        year = year(as.Date(!!Endo_ResultPerformeda))
      ) %>%
      summarise(Number = n())
    names(TestNumbers) <- c("week", "month", "year", "freq")
    TestNumbers$DayMonth <-
      paste("01_", TestNumbers$month, "_", TestNumbers$year, sep = "")
    TestNumbers$DayMonth <- dmy(TestNumbers$DayMonth)
    
    
    ggplot(data = TestNumbers, aes(x = week, y = freq)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "loess") 
    
      }}
    
    
  })
  
  
  
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############:::::  Chooser - Theograph HospNum  ############
  

  
  output$HospNumCustomTheo<-renderUI({
    selectInput("HospNumCustomTheoChooser", label = h4("Choose the column containing the hospital numbers"),
                choices = colnames(CustomTrim$data) ,selected = 1
    )
  })
  
  ############:::::  Chooser - Theograph Date  ############
  
  output$DatesCustomTheo<-renderUI({
    selectInput("DateColChooserCustomTheoChooser", label = h4("Choose the column containing the (formatted) dates of the endoscopies"),
                choices = colnames(CustomTrim$data) ,selected = 1
    )
  })
  
  
  ########### Barrett's  ############
  
  
  
  
  
  #########::::: Table Create- BarrettsTable#############  
  output$BarrettsTable = DT::renderDT({
    #Create a copy that can be independently edited for the Barrett's table
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
    
    
  },filter = 'top',selection = list(target = 'column'),extensions = 'Buttons', options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    pageLength = 5,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')))
  
  
  #########::::: Working Table- BarrettsTable#############  
  
  observeEvent(input$BarrettsTable_columns_selected,{
    #browser()
    if (length(input$BarrettsTable_columns_selected)>1){
      BarrTrim$data<- RV4$data[input$BarrettsTable_rows_all, input$BarrettsTable_columns_selected]
    } else(BarrTrim$data<-NULL)
    
  },ignoreInit = TRUE)
  
  
  #For the working set custom table- reactive to new columns
  observeEvent(input$BarrettsTable_rows_all,{
    if (length(input$BarrettsTable_columns_selected)>1){
      BarrTrim$data<- RV4$data[input$BarrettsTable_rows_all, input$BarrettsTable_columns_selected]
    } else(BarrTrim$data<-NULL)
  },ignoreInit = TRUE)
  

  
  output$Barr_DDRColSelect_colEndoscopist <- renderUI(
    selectInput("DDRColSel_colEndoscopist","Select the Endoscopist column", choices= 
                  colnames(BarrTrim$data))
  )
  

  output$Barr_DDRColSelect_colHistol <- renderUI(
    selectInput("DDRColSel_colHistol","Select the Histology column (called IMorNoIM- to get this run rhe PathStage button on the working set first", choices= 
                  colnames(BarrTrim$data))
  )
  
  output$Barr_DDRColSelect_colEndoFindings <- renderUI(
    selectInput("DDRColSel_colEndoFindings","Select the Endoscopy Findings column", choices= 
                  colnames(BarrTrim$data))
  )
  
  #Extract the endoscopic events
  observeEvent(input$Barr_DDRModalbtn,{
    
    #browser()
    
    #BarrDDR_TableData$data$IMorNoIM<<-Barretts_PathStage(RV4$data[input$BarrettsTable_rows_all,],input$DDRColSel_colHistol)

    DDRTable<-RV4$data%>%group_by(!!rlang::sym(input$DDRColSel_colEndoscopist),!!rlang::sym(input$DDRColSel_colHistol))%>%dplyr::summarise(n=n())    
    BarrDDR_TableData$data<-DDRTable%>%spread(input$DDRColSel_colHistol, n)
  },ignoreInit = TRUE)
  
  ############::::: BarrDDR_Table Create ############
  #From EndoMineR
  #GRS_Type_Assess_By_Unit(dataframe, ProcPerformed, Endo_Endoscopist, Dx,Histol) 
  
  output$BarrDDR_Table = DT::renderDT({
    #cols <- as.numeric(input$polypTable_columns_selected)
    #selectedCol<-colnames(polypData$data)[cols]
    #browser()
    BarrDDR_TableData$data
    
    #BarrDDR_TableData$data$IMorNoIM<-Barretts_PathStage(RV4$data, input$DDRColSel_colHistol)
  },filter = 'top',selection = list(target = 'row'),extensions = 'Buttons', options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    pageLength = 50,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'))) 
  
  
  #########::::: Drilldown Table- BarrDDR_Table############# 
  
  # subset the records to the row that was clicked
  drilldataBarrd <- reactive({
    shiny::validate(
      need(length(input$BarrDDR_Table_rows_selected) > 0, "Select rows to drill down!")
    )    
    #browser()
    selected_species <- BarrDDR_TableData$data[as.integer(input$BarrDDR_Table_rows_selected), ]
    variables <-    c(t(selected_species[,1]))
    mycolname <- colnames(selected_species)[1]
    RV4$data[RV4$data[, mycolname] %in%  variables ,]
  })
  
  # display the subsetted data
  output$drilldownBarr <- DT::renderDT({
    drilldataBarrd() 
  },selection = list(target = 'column'),extensions = 'Buttons', 
  options = list(
    fixedHeader=TRUE,
    scrollX = TRUE,
    scrollY = TRUE,
    pageLength = 5,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')))
  
  
  
  
  ############:::::  Vislualisation Esquiss ############
  
  callModule(module = esquisserServer, id = "esquisseBarr", data = BarrTrim)  
  
  ############::::: Button Prague score ############
  
   observeEvent(input$PragueScore,{
     #browser()
     cols <- as.numeric(input$BarrettsTable_columns_selected)
     selectedCol<-colnames(RV4$data)[cols]
     #Need to get rid of the mytext column as it is a list and it messes up the esquiss graphics
     RV4$data<-Barretts_PragueScore(RV4$data, selectedCol[1], selectedCol[2])
     RV4$data$mytext<-NULL
     RV4$data$MStage<-as.numeric(RV4$data$MStage)
     RV4$data$CStage<-as.numeric(RV4$data$CStage)
  },ignoreInit = TRUE)
  
  
  ############::::: Button- Path stage  ############
  
  observeEvent(input$PathStage,{
    #browser()
    cols <- as.numeric(input$BarrettsTable_columns_selected)
    selectedCol<-colnames(RV4$data)[cols]
    RV4$data$IMorNoIM<-Barretts_PathStage(RV4$data, selectedCol[1])
    
    
    
    #BarrDDR_Table
  },ignoreInit = TRUE)
  
  
  
  ############::::: Button- Follow up type  ############
  
  observeEvent(input$FollowUpType,{
    RV4$data$FU_Type<-Barretts_FUType(RV4$data, "CStage", "MStage", "IMorNoIM")
  },ignoreInit = TRUE)
  
  
  ############::::: Button- Surveillance Time  ############
  
  
  observeEvent(input$SurveillanceTime,{
    cols <- as.numeric(input$BarrettsTable_columns_selected)
    selectedCol<-colnames(RV4$data)[cols]
    RV4$data<-SurveilTimeByRow(RV4$data, selectedCol[1],selectedCol[2])
  },ignoreInit = TRUE)
  

  
  ############::::: Chooser - Quality Endoscopist Chooser stage  ############
  
  output$endoscopistCol<-renderUI({
    selectInput("endoscopistColChooser", label = h4("Choose the column showing the endoscopist:"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  

  ############::::: Chooser  - Quality Worst grade    ############
  
  
  
  output$worstGradeCol<-renderUI({
    selectInput("WorstGradeChooser", label = h4("Choose the column showing the worst grade"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  
  

  ############::::: Chooser  - Quality Document Quality  ############
  
  output$endoDoc_documentqual<-renderUI({
    selectInput("endoDoc_documentqualChoose", label = h4("Choose the endoscopic documentation column"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  
  ############:::::  Plot-Quality Documentation quality Plot ############
  
  
  myNotableWords <- c("[Ii]sland", "[Hh]iat|astric fold|[Pp]inch","esion|odule|lcer")
  
  
  
  output$plotBarrEQ <- renderPlotly({
    #browser()
    #Perform the lookup from EndoMiner for "[Ii]sland", Prague Score, "[Hh]iat|astric fold|[Pp]inch", "esion|odule|lcer"
    if(!is.null(BarrTrim$data)){
      if(ncol(BarrTrim$data>0)){
        if(nrow(BarrTrim$data>0)){
    shiny::validate(
      need(length(input$DDRColSel_colEndoscopist) > 0, "Press Barr_DDR to get the plots")
    ) 
    Hiatus<-BarrTrim$data %>% group_by(!! rlang::sym(input$DDRColSel_colEndoscopist)) %>% summarise(Hiatus = (sum(grepl("[Hh]iatus|[Ii]sland", !!rlang::sym(input$DDRColSel_colEndoFindings))) / dplyr::n()) * 100)
    Island<-BarrTrim$data %>% group_by(!! rlang::sym(input$DDRColSel_colEndoscopist)) %>% summarise(Island = (sum(grepl("[Ii]sland", !!rlang::sym(input$DDRColSel_colEndoFindings))) / dplyr::n()) * 100)
    Pinch<-BarrTrim$data %>% group_by(!! rlang::sym(input$DDRColSel_colEndoscopist)) %>% summarise(Pinch = (sum(grepl("[Pp]inch", !!rlang::sym(input$DDRColSel_colEndoFindings))) / dplyr::n()) * 100)
    Lesion<-BarrTrim$data %>% group_by(!! rlang::sym(input$DDRColSel_colEndoscopist)) %>% summarise(Lesion = (sum(grepl("esion|odule|lcer", !!rlang::sym(input$DDRColSel_colEndoFindings))) / dplyr::n()) * 100)
    FinalTable <-
      full_join(Hiatus, Island, by = input$DDRColSel_colEndoscopist)
    FinalTable <-
      full_join(FinalTable, Pinch, by = input$DDRColSel_colEndoscopist)
    FinalTable <-
      full_join(FinalTable, Lesion, by = input$DDRColSel_colEndoscopist)
    
    
    # Need to add the total colonoscopy count in here
    FinalTable <- data.frame(FinalTable)
    
    #Need to gather the table to make tidy for ggplot
    FinalTable<-tidyr::gather(FinalTable,key="DocumentedElement",value="PercentDocs",--!!rlang::sym(input$DDRColSel_colEndoscopist))
    
    ggplot(FinalTable,  aes(x = DocumentedElement,fill=input$DDRColSel_colEndoscopist)) + 
      geom_histogram(stat = "count")
      
        }
      }}
      
    
  })
  
  
  ############:::::  Plot Quality Endoscopist vs Worst grade Plot  ############
  
  output$plotBarrQM <- renderPlotly({
    if(!is.null(BarrTrim$data)){
      if(ncol(BarrTrim$data>0)){
        if(nrow(BarrTrim$data>0)){
          shiny::validate(
            need(length(input$DDRColSel_colEndoscopist) > 0, "Press Barr_DDR to get the plots")
          ) 
    key <- input$DDRColSel_colEndoscopist
   
   p<-ggplot(BarrTrim$data,  aes_string(x = input$DDRColSel_colHistol,fill=key)) + 
      geom_histogram(stat = "count")
   ggplotly(p,source = "subset",key=key) %>% layout(dragmode = "select")
   
      
        }
      }}
  })
  

  
  ############:::::  CrossTablulate  ############
  
  
  output$BarrPivot <- renderRpivotTable({
    if(!is.null(BarrTrim$data)){
    rpivotTable(BarrTrim$data)
      }
  })
  
  
  
  ############:::::  Chooser-Theograph Endoscopist  ############
  
  
  output$HospNumBarrTheo<-renderUI({
    selectInput("HospNumBarrTheoChooser", label = h4("Choose the column containing the hospital numbers"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  
  ############:::::  Chooser-Theograph Date  ############
  
  output$DatesBarrTheo<-renderUI({
    selectInput("DateColChooserBarrTheoChooser", label = h4("Choose the column containing the (formatted) dates of the endoscopies"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  
  ############:::::  Plot-Theograph Plot ############
  
  
  output$plotBarrPT <- renderPlotly({
    if(!is.null(BarrTrim$data)){
    
    #browser()
    #Create a column with factors for the worst grade
    BarrTrim$data$RecodedColumn<-as.integer(factor(RV4$data$IMorNoIM, c("No_IM","IM","LGD","HGD","T1a","IGD","SM1","SM2"), ordered = TRUE))
    
    #Only select patients where there is more than one endoscopy:
    bb<-BarrTrim$data %>% group_by(!! rlang::sym(input$HospNumBarrTheoChooser)) %>% filter(n() > 2)
    
    #Now use the user defined date and patient ID columns to make the theographs
    
    #Now develop the patient specific journey with faceted plot in ggplot2
    ggplot(bb) +
      geom_line(aes(input$DateColChooserBarrTheoChooser,RecodedColumn),shape=11,size=1) +
      geom_point(aes(input$DateColChooserBarrTheoChooser,RecodedColumn),shape=11,colour="red",size=1) +
      xlab("Date") + 
      ylab("Histopathological State") +
      theme(axis.text.x=element_text(angle=-90)) + 
      facet_grid(input$HospNumBarrTheoChooser)
      }
  })
  
  
  ############:::::  Chooser -Date EndoUtilisation############
  output$Date_endoscopyutilisationBarr<-renderUI({
    selectInput("Date_endoscopyutilisationChooserBarr", label = h4("Choose the column showing the date"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  
  ############:::::  Plot-EndoUtilisation Plot ############
  
  output$endoscopyUse_EndoscopyUseBarr <- renderPlotly({
    
    if(!is.null(BarrTrim$data)){
    #Create the grouped table here of the number of endoscopies done by day
    #Then perform as per below
    dtData<-BarrTrim$data %>% group_by(!!rlang::sym(input$Date_endoscopyutilisationChooserBarr)) %>% dplyr::summarise(n = n())
    # base plot
    
    #Get rid of NA's as they mess things up.
    dtData<-na.omit(as.data.table(dtData))
    
    p1 = ggplot_calendar_heatmap(
      dtData,
      input$Date_endoscopyutilisationChooserBarr,
      'n'
    )
    
    # adding some formatting
    p1 + 
      xlab('') + 
      ylab('') + 
      scale_fill_continuous(low = 'green', high = 'red') + 
      facet_wrap(~Year, ncol = 1)
    
      }
  })
  
  
  
  
  ############:::::  Chooser- TimeSeriesAnalysis Event  ############
  
  output$endoscopicEventBarr<-renderUI({
    selectInput("endoscopicEventColChooserBarr", label = h4("Choose the column containing the events of interest"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  
  
  
  ############:::::  Chooser - TimeSeriesAnalysis Dates  ############
  
  output$DatesBarr<-renderUI({
    selectInput("DateColChooserBarr", label = h4("Choose the column containing the (formatted) dates of the endoscopies"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  
  
  ############:::::  Plot-Time Series Analysis Plot ############
  
  output$plotBarrTSA <- renderPlotly({
    ####Need to deal with this one:
    if(!is.null(BarrTrim$data)){
    
    #browser()
    Endo_ResultPerformeda <- sym(input$Date_endoscopyutilisationChooserBarr)
    TestNumbers <-
      BarrTrim$data %>% group_by(!! rlang::sym(input$endoscopicEventColChooserBarr)) %>% 
      arrange(as.Date(!!Endo_ResultPerformeda)) %>% group_by(
        week = week(as.Date(!!Endo_ResultPerformeda)),
        month = month(as.Date(!!Endo_ResultPerformeda)),
        year = year(as.Date(!!Endo_ResultPerformeda))
      ) %>%
      summarise(Number = n())
    names(TestNumbers) <- c("week", "month", "year", "freq")
    TestNumbers$DayMonth <-
      paste("01_", TestNumbers$month, "_", TestNumbers$year, sep = "")
    TestNumbers$DayMonth <- dmy(TestNumbers$DayMonth)
    
    
    ggplot(data = TestNumbers, aes(x = week, y = freq)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "loess") 
    
      }
    
    
  })
  

  
  
  ########### Polyps ############    
  
  
  
  
  
  
  #########::::: Table- polypTable#############  
  
  
  output$polypTable = DT::renderDT({
    #polypData$data
    
    #Create a copy that can be independently edited for the polyp table
    mypolypdata1<- RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "polyp")),]
    polypData$data <- mypolypdata1[Reduce(`|`, lapply(mypolypdata1, grepl, pattern = "colonoscopy")),]
    
  },filter = 'top',selection = list(target = 'column'),options = list(scrollX = TRUE,pageLength = 5,
                                                                      dom = 'Bfrtip',
                                                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')))
  
  #########::::: Working Table- polypTable############# 
  observeEvent(input$polypTable_columns_selected,{
    #browser()
    if (length(input$polypTable_columns_selected)>1){
      polypTrim$data<- polypData$data[input$polypTable_rows_all, input$polypTable_columns_selected]
    } else(BarrTrim$data<-NULL)
    
  },ignoreInit = TRUE)
  
  
  
  ############::::: Chooser- GRS_ADR column select for modal ############
  
  output$GRS_ADRColSelect_colEndoEndoscopist <- renderUI(
    selectInput("GRS_ADRColSelect_colEndoEndoscopist","Select the Procedure performed column", choices= 
                  colnames(RV3$data))
  )
  
  output$GRS_ADRColSelect_colProcPerf <- renderUI(
    selectInput("ADRColSel_colProcPerf","Select the Endoscopist column", choices= 
                  colnames(RV3$data))
  )
  
  output$GRS_ADRColSelect_colMacroDescript <- renderUI(
    selectInput("ADRColSel_colMacroDescript","Select the Endoscopy Findings column", choices= 
                  colnames(RV3$data))
  )
  
  output$GRS_ADRColSelect_colHistol <- renderUI(
    selectInput("ADRColSel_colHistol","Select the Histology column", choices= 
                  colnames(RV3$data))
  )
  
  #########::::: Button GRS #############  
  
  observeEvent(input$GRS_ADRModalbtn,{
    
    #browser()
    GRS_TableData$data<<-GRS_Type_Assess_By_Unit(polypData$data[input$polypTable_rows_all,], input$ADRColSel_colEndoEndoscopist,
                                                 input$ADRColSel_colProcPerf,
                                                 input$ADRColSel_colMacroDescript,
                                                 input$ADRColSel_colHistol)
    
  },ignoreInit = TRUE)
  
  ############::::: GRSTable Create ############
  #From EndoMineR 
  #GRS_Type_Assess_By_Unit(dataframe, ProcPerformed, Endo_Endoscopist, Dx,Histol) 
  
  output$GRS_Table = DT::renderDT({
    #cols <- as.numeric(input$polypTable_columns_selected)
    #selectedCol<-colnames(polypData$data)[cols]
    #browser()
    #GRS_TableData<<-GRS_Type_Assess_By_Unit(polypData$data[input$polypTable_rows_all,], selectedCol[1],selectedCol[2], selectedCol[3], selectedCol[4])
    GRS_TableData$data
  },filter = 'top',selection = list(target = 'row'),extensions = 'Buttons', options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    pageLength = 50,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'))) 
  
  
  #########::::: Drilldown Table- polypTable############# 
  
  # subset the records to the row that was clicked
  drilldataPolyp <- reactive({
    shiny::validate(
      need(length(input$GRS_Table_rows_selected) > 0, "Select rows to drill down!")
    )    
    selected_species <- GRS_TableData$data[as.integer(input$GRS_Table_rows_selected), ]
    variables <-    c(t(as.character(selected_species[,1])))
    mycolname <- colnames(selected_species)[1]
    polypData$data[polypData$data[, mycolname] %in%  variables ,]
  })
  
  # display the subsetted data
  output$drilldown <- DT::renderDT({
    drilldataPolyp() 
},selection = list(target = 'column'),extensions = 'Buttons', 
options = list(
  fixedHeader=TRUE,
  scrollX = TRUE,
  scrollY = TRUE,
  pageLength = 5,
  dom = 'Bfrtip',
  buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')))
  
  
  
  

  ############:::::  Visualisation Esquiss ############
  
  callModule(module = esquisserServer, id = "esquissePolyp",data = polypTrim)
  
  ############::::: PolypTable Chooser- EndoUtilisation Date Chooser ############
  
  output$Date_endoscopyutilisationPolyp<-renderUI({
    selectInput("Date_endoscopyutilisationChooserPolyp", label = h4("Choose the column showing the date"),
                choices = colnames(polypTrim$data) ,selected = 1
    )
  })
  
  
  ############:::::  Plot- EndoUtilisation Plot ############
  
  
  output$endoscopyUse_EndoscopyUsePolyp <- renderPlotly({
    
    if(nrow(BarrTrim$data>0)){
      if(ncol(BarrTrim$data>0)){
    #Then perform as per below
    dtData<-polypData$data%>% group_by(!!rlang::sym(input$Date_endoscopyutilisationChooserPolyp)) %>% dplyr::summarise(n = n())
    # base plot
    
    #Get rid of NA's as they mess things up.
    dtData<-na.omit(as.data.table(dtData))
    
    p1 = ggplot_calendar_heatmap(
      dtData,
      input$Date_endoscopyutilisationChooserPolyp,
      'n'
    )
    
    # adding some formatting
    p1 + 
      xlab('') + 
      ylab('') + 
      scale_fill_continuous(low = 'green', high = 'red') + 
      facet_wrap(~Year, ncol = 1)
    
      }}
  })
  


  
  ############:::::  Chooser- TimeSeries Analysis Event  ############
  output$endoscopicEventPolyp<-renderUI({
    selectInput("endoscopicEventColChooserPolyp", label = h4("Choose the column containing the events of interest"),
                choices = colnames(polypTrim$data) ,selected = 1
    )
  })
  
  ############:::::  Chooser- TimeSeries Analysis Date  ############
  
  output$DatesPolyp<-renderUI({
    selectInput("DateColChooserPolyp", label = h4("Choose the column containing the (formatted) dates of the endoscopies"),
                choices = colnames(polypTrim$data) ,selected = 1
    )
  })
  
  
  
   
  ############:::::  Plot- TimeSeries Analysis Plot ############
  
  output$plotPolypTSA <- renderPlotly({
    
    if(nrow(BarrTrim$data>0)){
      if(ncol(BarrTrim$data>0)){
    ####Need to deal with this one:
    
    #browser()
    Endo_ResultPerformeda <- sym(input$Date_endoscopyutilisationChooserPolyp)
    TestNumbers <-
      polypTrim$data %>% group_by(!! rlang::sym(input$endoscopicEventColChooserPolyp)) %>% 
      arrange(as.Date(!!Endo_ResultPerformeda)) %>% group_by(
        week = week(as.Date(!!Endo_ResultPerformeda)),
        month = month(as.Date(!!Endo_ResultPerformeda)),
        year = year(as.Date(!!Endo_ResultPerformeda))
      ) %>%
      summarise(Number = n())
    names(TestNumbers) <- c("week", "month", "year", "freq")
    TestNumbers$DayMonth <-
      paste("01_", TestNumbers$month, "_", TestNumbers$year, sep = "")
    TestNumbers$DayMonth <- dmy(TestNumbers$DayMonth)
    
    
    ggplot(data = TestNumbers, aes(x = week, y = freq)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "loess") 
    
      }}
    
    
  })
  
  
  
  ############:::::  CrossTabulate ############
  
  
  output$OverallPivotPolyp <- renderRpivotTable({
    rpivotTable(polypTrim$data)
  })
 
  
  #selectedData <- reactive({
    #output$endotable
 # })
  
  
  

  
   

}

