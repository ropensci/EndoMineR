
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


# Define server logic required to draw a histogram
options(shiny.maxRequestSize=30*1024^2) 

RV <- reactiveValues(data = data.frame())
RV2 <- reactiveValues(data = data.frame())
RV3 <- reactiveValues(data = data.frame())
RV4 <- reactiveValues(data = data.frame())
Trim <- reactiveValues(data = data.frame())
pivotData<-reactiveValues(data = data.frame())
RV5 <- reactiveValues(data = data.frame())
polypData <- reactiveValues(data = data.frame())
polypTrim <- reactiveValues(data = data.frame())
BarrettsData <- reactiveValues(data = data.frame())
BarrTrim <- reactiveValues(data = data.frame())
CustomData <- reactiveValues(data = data.frame())
CustomTrim <- reactiveValues(data = data.frame())

RV9 <- reactiveValues(data = data.frame())
RV10 <- reactiveValues(data = data.frame())
Numbxvals <- reactiveValues(data = NULL)




server <- function(input, output,session) {

  
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
  },selection = list(target = 'column'),options = list(scrollX = TRUE,pageLength = 5,
                                                       dom = 'Bfrtip',
                                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')))
  
 
  

  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  #For the working set custom table- reactive to new columns
  observeEvent(input$polypTable_rows_all,{
    if (length(input$polypTable_columns_selected)>1){
    polypTrim$data<- polypData$data[input$polypTable_rows_all, input$polypTable_columns_selected]
    } else(BarrTrim$data<-NULL)
  },ignoreInit = TRUE)
  

  
  
  ########### Endotable events ############  
  
  ###########:::::Endotable events-textPrep ############
  #Prepare the text for endoscopy
  observeEvent(input$textPrep,{
    mywordsOGD<-input$caption
    mywordsOGD<-unlist(strsplit(mywordsOGD,","))
    RV$data<-textPrep(RV$data[,1],mywordsOGD)

    #Try type conversion here:
    RV$data<-type.convert(RV$data)
   
  },ignoreInit = TRUE)
  
  
  
  
  ###########:::::Endotable events-Date standardiser ###########
  #Standardise the date Dataset 1
  observeEvent(input$DateStandardiserEndo,{
    RV$data[,as.numeric(input$endotable_columns_selected)]<-parse_date_time(str_extract(RV$data[,as.numeric(input$endotable_columns_selected)],
                                                                                        "(\\d{4}[[:punct:]]\\d{2}[^:alnum:]\\d{2})|(\\d{2}[^:alnum:]\\d{2}[^:alnum:]\\d{4})"),
                                                                            orders = c("dmy", "ymd"))
  },ignoreInit = TRUE) 
  
  
  ###########:::::Endotable events-HospNum standardiser ###########
  
  #Standardise the Hospital Number Dataset 1
  observeEvent(input$HospitalNumberExtractorEndo,{
    RV$data[,as.numeric(input$endotable_columns_selected)]<-str_extract(RV$data[,as.numeric(input$endotable_columns_selected)],
                                                                        "([a-z0-9]\\d{4,}[a-z0-9])")
    
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
      
      #output$folder_file <- renderText(as.character(folder_selected$datapath))
    }
  })

  
  ###########:::::pathTable events- textPrep ############  
  observeEvent(input$textPrepPath,{
    mywordsPath<-input$captionPath
    mywordsPath<-unlist(strsplit(mywordsPath,","))
    RV2$data<-textPrep(RV2$data[,1],mywordsPath)
    RV2$data<-type.convert(RV2$data)
  },ignoreInit = TRUE)
  
  
  
  ###########:::::pathTable events-Date standardiser ###########
  
  #Standardise the date pathTable
  observeEvent(input$DateStandardiserEPath,{
    RV2$data[,as.numeric(input$pathTable_columns_selected)]<-parse_date_time(str_extract(RV2$data[,as.numeric(input$pathTable_columns_selected)],
                                                                                         "(\\d{4}[[:punct:]]\\d{2}[^:alnum:]\\d{2})|(\\d{2}[^:alnum:]\\d{2}[^:alnum:]\\d{4})"),
                                                                             orders = c("dmy", "ymd"))
  },ignoreInit = TRUE)
  
  
  
  
  
  
  ###########:::::pathTable events-HospitalNumber standardiser ###########
  
  #Standardise the Hospital Number pathTable
  observeEvent(input$HospitalNumberExtractorPath,{
    RV2$data[,as.numeric(input$pathTable_columns_selected)]<-str_extract(RV2$data[,as.numeric(input$pathTable_columns_selected)],
                                                                         "([a-z0-9]\\d{4,}[a-z0-9])")
  },ignoreInit = TRUE) 
  

  
  
  
  ###########:::::pathTable events-Categorical standardiser ###########
  
  #Standardise the Categorical as categorical in Pathology
  observeEvent(input$CategoricalDataPath,{
    RV$data[,as.numeric(input$pathTable_columns_selected)]<-as.factor(RV$data[,as.numeric(input$pathTable_columns_selected)])
    
  },ignoreInit = TRUE)
  
  
 
  

  

  ########### EndoMerge events ############  
  
  ###########::::: MergeTable events-textPrep ###########
  
  observeEvent(input$textPrepMerge,{
    mywordsOGD<-input$captionMerge
    mywordsOGD<-unlist(strsplit(mywordsOGD,","))
    RV3$data<-textPrep(RV3$data[,1],mywordsOGD)
    
    #Try type conversion here:
    RV3$data<-type.convert(RV3$data)
    
  },ignoreInit = TRUE)
  
  ############::::: MergeTable Event- EndoMerge############
  
  observeEvent(input$Endomerge2,{
    #Merge the patientID column and date from each table. Make sure that the patient ID is chosen first;
    #Need to fix this to understand when it is selecting the number. I think the user needs to 
    #convert to date and then select columns (date first) at one sitting with the datatable.
    
    RV3$data<-Endomerge2(RV$data,colnames(RV$data[as.numeric(input$endotable_columns_selected[1])]),
                         colnames(RV$data[as.numeric(input$endotable_columns_selected[2])]),
                         RV2$data,
                         colnames(RV2$data[as.numeric(input$pathTable_columns_selected[1])]),
                         colnames(RV2$data[as.numeric(input$pathTable_columns_selected[2])]))
    
    RV4$data<- RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
    
    RV3$data<-RV3$data[,1:ncol(RV3$data)-1]
    

  },ignoreInit = TRUE)
  
  ############::::: MergeTable Event- Date standardiser############
  
  #Standardise the date
  observeEvent(input$DateStandardiserMerge,{
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-parse_date_time(str_extract(RV3$data[,as.numeric(input$mergedTable_columns_selected)],
                                                                                           "(\\d{4}[[:punct:]]\\d{2}[^:alnum:]\\d{2})|(\\d{2}[^:alnum:]\\d{2}[^:alnum:]\\d{4})"),
                                                                               orders = c("dmy", "ymd"))
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  ############::::: MergeTable Event- Hospital standardiser############
  
  #Standardise the Hospital Number Merge
  observeEvent(input$HospitalNumberExtractorMerge,{
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-str_extract(RV3$data[,as.numeric(input$mergedTable_columns_selected)],
                                                                         "([a-z0-9]\\d{4,}[a-z0-9])")
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE) 
  
  ############::::: MergeTable Event- Categorical standardiser############
  
  #Standardise the Categorical data
  observeEvent(input$CategoricalDataMerge,{
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-as.factor(RV3$data[,as.numeric(input$mergedTable_columns_selected)])
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  ############::::: MergeTable Event- Numeric standardiser############
  
  #Standardise the Numbers as numeric in Endoscopy
  observeEvent(input$NumericDataMerge,{
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-as.numeric(str_extract(RV3$data[,as.numeric(input$mergedTable_columns_selected)],
                                                                                      "[0-9]+"))
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  
  ############::::: MergeTable Event- Negex############
  
  #Negex Remove
  observeEvent(input$NegExMerge,{
    standardisedTextOutput<-str_split(RV3$data[,as.numeric(input$mergedTable_columns_selected)], "\\.")
    standardisedTextOutput<-lapply(standardisedTextOutput, function(x) NegativeRemove(x))

    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-unlist(lapply(standardisedTextOutput, function(x) paste0(unlist(x),collapse=" ")))
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
    
  },ignoreInit = TRUE)
  
  

  ############::::: MergeTable Event- Regex############
  
  observeEvent(input$regexSearch_ok,{
    #Need to bring up modal from here first to get the input$new_name as a renderui component
    browser()
    myInterim<-str_extract_all(RV3$data[,as.numeric(input$mergedTable_columns_selected)],input$regexSearch)
    NewCol<-as.character(lapply(myInterim,function(x) unlist(x)))
    
    RV3$data<-cbind(NewCol,RV3$data)
    
    
  },ignoreInit = TRUE)
  
  
  
  
  
  
  
  
  
  ############::::: MergeTable Event- Endoscopist standardiser############
  
  
  #Extract the endoscopist
  observeEvent(input$EndoscEndoscopistMerge,{
    #browser()
    #RV$data[,as.numeric(input$endotable_columns_selected)]
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-EndoscEndoscopist(RV3$data[,as.numeric(input$mergedTable_columns_selected)])
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  
  ############::::: MergeTable Event- Medication standardiser############
  
  #Extract the medication  
  observeEvent(input$EndoscMedsMerge,{
    RV3$data<-cbind(EndoscMeds(RV3$data[,as.numeric(input$mergedTable_columns_selected)]),RV3$data)
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  
  ############::::: MergeTable Event- Instrument standardiser############
  
  #Extract the instrument
  observeEvent(input$EndoscInstrumentMerge,{
    RV3$data$mergedTable_columns_selected<-EndoscInstrument(RV3$data[,as.numeric(input$mergedTable_columns_selected)])
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  
  
  ############::::: MergeTable Event- EndoEvent standardiser############
  
  #Extract the endoscopic events
  observeEvent(input$EndoEvent,{

    cols <- as.numeric(input$mergedTable_columns_selected)
    selectedCol<-colnames(RV3$data)[cols]
    RV3$data$EndoscopyEvent<-EndoscopyEvent(RV3$data, selectedCol[1], selectedCol[2],selectedCol[3], selectedCol[4])
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
      },ignoreInit = TRUE)
  



  ############::::: MergeTable Event- Biopsy Number standardiser############
  
  
  #Extract the number of biopsies  
  observeEvent(input$NumBxModal_ok,{
    #Need to bring up modal from here first to get the input$new_name as a renderui component
    RV3$data$NumBx<-HistolNumbOfBx(RV3$data[,as.numeric(input$mergedTable_columns_selected)],input$new_name)
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  
  ############::::: MergeTable Event- Biopsy size standardiser############
  
  #Extract the biopsy size
  observeEvent(input$BxSizeMerge,{
    RV3$data$BxSize<-HistolBxSize(RV3$data[,as.numeric(input$mergedTable_columns_selected)])
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
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
  
  
  
  
  
  
  
  ############::::: CustomTable Visualisation Esquiss ############
  
  callModule(module = esquisserServer, id = "esquisseCustom", data = CustomTrim)
  
  ############::::: CustomTable CrossTabulate############
  
  output$OverallPivot <- renderRpivotTable({
    rpivotTable(CustomTrim$data)
  })
  
  ############::::: CustomTable - EndoUtilisation Date Chooser ############
  
  output$Date_endoscopyutilisationCustom<-renderUI({
    selectInput("Date_endoscopyutilisationChooserCustom", label = h4("Choose the column showing the date"),
                choices = colnames(CustomTrim$data) ,selected = 1
    )
  })
  

  
  
  
  ############::::: CustomTable EndoUtilisation  Plot ############
  
  output$endoscopyUse_EndoscopyUseCustom <- renderPlotly({
    #Create the grouped table here of the number of endoscopies done by day
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
  })
  
  
  ############::::: CustomTable - EndoUtilisation Event Chooser ############
  
  
  
  output$endoscopicEventCustom<-renderUI({
    selectInput("endoscopicEventColChooserCustom", label = h4("Choose the column containing the events of interest"),
                choices = colnames(CustomTrim$data) ,selected = 1
    )
  })
  
  ############::::: CustomTable - TimeSeriesAnalysis Date Chooser ############
  
  output$DatesCustom<-renderUI({
    selectInput("DateColChooserCustom", label = h4("Choose the column containing the (formatted) dates of the endoscopies"),
                choices = colnames(CustomTrim$data) ,selected = 1
    )
  }) 

  
  
  
  ############::::: CustomTable Plot- TimeSeriesAnalysis ############
  
  
  output$plotCustomTSA <- renderPlotly({

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
    
    
  })
  
  
  
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############::::: CustomTable - Theograph HospNum Chooser ############
  

  
  output$HospNumCustomTheo<-renderUI({
    selectInput("HospNumCustomTheoChooser", label = h4("Choose the column containing the hospital numbers"),
                choices = colnames(CustomTrim$data) ,selected = 1
    )
  })
  
  ############::::: CustomTable - Theograph Date Chooser ############
  
  output$DatesCustomTheo<-renderUI({
    selectInput("DateColChooserCustomTheoChooser", label = h4("Choose the column containing the (formatted) dates of the endoscopies"),
                choices = colnames(CustomTrim$data) ,selected = 1
    )
  })
  
  
  ########### Barrett's  ############
  
  
  
  
  
  #########:::::Table- BarrettsTable#############  
  output$BarrettsTable = DT::renderDT({
    #Create a copy that can be independently edited for the Barrett's table
    RV4$data
    
    
  },filter = 'top',selection = list(target = 'column'),extensions = 'Buttons', options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    pageLength = 5,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')))
  
  
  #########:::::Working Table- BarrettsTable#############  
  
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
  
  
  
  
  
  
  
  ############::::: BarrTable Vislualisation Esquiss ############
  
  callModule(module = esquisserServer, id = "esquisseBarr", data = BarrTrim)  
  
  ############::::: BarrettsTable Event- Prague score ############
  
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
  
  
  ############::::: BarrettsTable Event- Path stage  ############
  
  observeEvent(input$PathStage,{
    browser()
    cols <- as.numeric(input$BarrettsTable_columns_selected)
    selectedCol<-colnames(RV4$data)[cols]
    RV4$data$IMorNoIM<-Barretts_PathStage(RV4$data, selectedCol[1])
  },ignoreInit = TRUE)
  
  
  
  ############::::: BarrettsTable Event- Follow up type  ############
  
  observeEvent(input$FollowUpType,{
    RV4$data$FU_Type<-Barretts_FUType(RV4$data, "CStage", "MStage", "IMorNoIM")
  },ignoreInit = TRUE)
  
  
  ############::::: BarrettsTable Event- Surveillance Time  ############
  
  
  observeEvent(input$SurveillanceTime,{
    cols <- as.numeric(input$BarrettsTable_columns_selected)
    selectedCol<-colnames(RV4$data)[cols]
    RV4$data<-SurveilTimeByRow(RV4$data, selectedCol[1],selectedCol[2])
  },ignoreInit = TRUE)
  
  ############::::: BarrTable Esquiss ############
  
  callModule(module = esquisserServer, id = "esquisseCustom", data = CustomTrim)
  ############::::: BarrettsTable - Quality Endoscopist Chooser stage  ############
  
  output$endoscopistCol<-renderUI({
    selectInput("endoscopistColChooser", label = h4("Choose the column showing the endoscopist:"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  

  ############::::: BarrettsTable - Quality Worst grade Chooser stage  ############
  
  
  
  output$worstGradeCol<-renderUI({
    selectInput("WorstGradeChooser", label = h4("Choose the column showing the worst grade"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  
  

  ############::::: BarrettsTable - Quality Document Quality Chooser stage  ############
  
  output$endoDoc_documentqual<-renderUI({
    selectInput("endoDoc_documentqualChoose", label = h4("Choose the endoscopic documentation column"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  
  
  
  ############::::: BarrettsTable - TimeSeriesAnalysis Event Chooser stage  ############
  
  output$endoscopicEventBarr<-renderUI({
    selectInput("endoscopicEventColChooserBarr", label = h4("Choose the column containing the events of interest"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  
  
  
  ############::::: BarrettsTable - TimeSeriesAnalysis Dates Chooser stage  ############

    output$DatesBarr<-renderUI({
    selectInput("DateColChooserBarr", label = h4("Choose the column containing the (formatted) dates of the endoscopies"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })

  
  
  ############::::: BarrettsTable - Quality Endoscopist vs Worst grade Plot  ############
  
  output$plotBarrQM <- renderPlotly({
   ggplot(BarrTrim$data,  aes_string(x = input$WorstGradeChooser,fill=input$endoscopistColChooser)) + 
      geom_histogram(stat = "count")
  })
  
  ############::::: BarrettsTable CrossTablulate  ############
  
  
  output$BarrPivot <- renderRpivotTable({
    rpivotTable(BarrTrim$data)
  })
  
  
  
  ############::::: BarrettsTable -Theograph Endoscopist Chooser ############
  
  
  output$HospNumBarrTheo<-renderUI({
    selectInput("HospNumBarrTheoChooser", label = h4("Choose the column containing the hospital numbers"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  
  ############::::: BarrettsTable -Theograph Date Chooser ############
  
  output$DatesBarrTheo<-renderUI({
    selectInput("DateColChooserBarrTheoChooser", label = h4("Choose the column containing the (formatted) dates of the endoscopies"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  
  ############::::: BarrettsTable Plot-Theograph Plot ############
  
  
  output$plotBarrPT <- renderPlotly({
    
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
  })
  
  
  ############::::: BarrettsTable -EndoUtilisation############
  output$Date_endoscopyutilisationBarr<-renderUI({
    selectInput("Date_endoscopyutilisationChooserBarr", label = h4("Choose the column showing the date"),
                choices = colnames(BarrTrim$data) ,selected = 1
    )
  })
  
  ############::::: BarrettsTable Plot-EndoUtilisation Plot ############
  
  output$endoscopyUse_EndoscopyUseBarr <- renderPlotly({
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
  })
  
  
  
  
  
  
  ############::::: BarrettsTable Plot-Quality Documentation quality Plot ############
  
  
  myNotableWords <- c("[Ii]sland", "[Hh]iat|astric fold|[Pp]inch","esion|odule|lcer")
  
  
  
  output$plotBarrEQ <- renderPlotly({
    #browser()
    #Perform the lookup from EndoMiner for "[Ii]sland", Prague Score, "[Hh]iat|astric fold|[Pp]inch", "esion|odule|lcer"
    Hiatus<-BarrTrim$data %>% group_by(!! rlang::sym(input$endoscopistColChooser)) %>% summarise(Hiatus = (sum(grepl("[Hh]iatus|[Ii]sland", !!rlang::sym(input$endoDoc_documentqualChoose))) / dplyr::n()) * 100)
    Island<-BarrTrim$data %>% group_by(!! rlang::sym(input$endoscopistColChooser)) %>% summarise(Island = (sum(grepl("[Ii]sland", !!rlang::sym(input$endoDoc_documentqualChoose))) / dplyr::n()) * 100)
    Pinch<-BarrTrim$data %>% group_by(!! rlang::sym(input$endoscopistColChooser)) %>% summarise(Pinch = (sum(grepl("[Pp]inch", !!rlang::sym(input$endoDoc_documentqualChoose))) / dplyr::n()) * 100)
    Lesion<-BarrTrim$data %>% group_by(!! rlang::sym(input$endoscopistColChooser)) %>% summarise(Lesion = (sum(grepl("esion|odule|lcer", !!rlang::sym(input$endoDoc_documentqualChoose))) / dplyr::n()) * 100)
    FinalTable <-
      full_join(Hiatus, Island, by = input$endoscopistColChooser)
    FinalTable <-
      full_join(FinalTable, Pinch, by = input$endoscopistColChooser)
    FinalTable <-
      full_join(FinalTable, Lesion, by = input$endoscopistColChooser)

    
    # Need to add the total colonoscopy count in here
    FinalTable <- data.frame(FinalTable)
    
    #Need to gather the table to make tidy for ggplot
    FinalTable<-tidyr::gather(FinalTable,key="DocumentedElement",value="PercentDocs",--!!rlang::sym(input$endoscopistColChooser))
    
    ggplot(FinalTable,  aes(x = DocumentedElement,fill=input$endoscopistColChooser)) + 
      geom_histogram(stat = "count")

  })
  
  
  ############::::: BarrettsTable Plot-Time Series Analysis Plot ############
  
  output$plotBarrTSA <- renderPlotly({
    ####Need to deal with this one:
    
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
      polypTrim$data<- RV4$data[input$polypTable_rows_all, input$polypTable_columns_selected]
    } else(BarrTrim$data<-NULL)
    
  },ignoreInit = TRUE)
  
  
  
  
  
  
  

  ############::::: PolypTable Visualisation Esquiss ############
  
  callModule(module = esquisserServer, id = "esquissePolyp",data = polypTrim)
  
  ############::::: PolypTable Chooser- EndoUtilisation Date Chooser ############
  
  output$Date_endoscopyutilisationPolyp<-renderUI({
    selectInput("Date_endoscopyutilisationChooserPolyp", label = h4("Choose the column showing the date"),
                choices = colnames(polypTrim$data) ,selected = 1
    )
  })
  
  
  ############::::: PolypTable Plot- EndoUtilisation Plot ############
  
  
  output$endoscopyUse_EndoscopyUsePolyp <- renderPlotly({
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
  })
  


  
  ############::::: PolypTable - TimeSeries Analysis Event Chooser ############
  output$endoscopicEventPolyp<-renderUI({
    selectInput("endoscopicEventColChooserPolyp", label = h4("Choose the column containing the events of interest"),
                choices = colnames(polypTrim$data) ,selected = 1
    )
  })
  
  ############::::: PolypTable - TimeSeries Analysis Date Chooser ############
  
  output$DatesPolyp<-renderUI({
    selectInput("DateColChooserPolyp", label = h4("Choose the column containing the (formatted) dates of the endoscopies"),
                choices = colnames(polypTrim$data) ,selected = 1
    )
  })
  
  
  
   
  ############::::: PolypTable Plot- TimeSeries Analysis Plot ############
  
  output$plotPolypTSA <- renderPlotly({
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
    
    
  })
  
  
  
  
  
  ############::::: PolypTable CrossTabulate ############
  
  
  output$OverallPivotPolyp <- renderRpivotTable({
    rpivotTable(polypTrim$data)
  })
  
 
  #From EndoMineR 
  #GRS_Type_Assess_By_Unit(dataframe, ProcPerformed, Endo_Endoscopist, Dx,Histol) 
  
  output$GRS_Table = DT::renderDT({
    cols <- as.numeric(input$polypTable_columns_selected)
    selectedCol<-colnames(polypData$data)[cols]
    #browser()
    GRS_Type_Assess_By_Unit(polypData$data[input$polypTable_rows_all,], selectedCol[1],selectedCol[2], selectedCol[3], selectedCol[4])
  
    },filter = 'top',selection = list(target = 'column'),extensions = 'Buttons', options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    pageLength = 50,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'))) 

}

