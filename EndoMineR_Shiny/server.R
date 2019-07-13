
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

# Define server logic required to draw a histogram
options(shiny.maxRequestSize=30*1024^2) 

RV <- reactiveValues(data = data.frame())
data_r <-reactiveValues(data = data.frame())
RV2 <- reactiveValues(data = data.frame())
RV3 <- reactiveValues(data = data.frame())
RV4 <- reactiveValues(data = data.frame())
Trim <- reactiveValues(data = data.frame())
pivotData<-reactiveValues(data = data.frame())
RV5 <- reactiveValues(data = data.frame())
polypData <- reactiveValues(data = data.frame())
BarrTrim <- reactiveValues(data = data.frame())
BarrettsData <- reactiveValues(data = data.frame())
RV9 <- reactiveValues(data = data.frame())
RV10 <- reactiveValues(data = data.frame())



server <- function(input, output,session) {
  
#Radiant things
  
 
  observe({
  #browser()
    inFile_endoscopy <- input$FileIn_endoscopy
    if (!is.null(inFile_endoscopy)) {   
      dataFile <- read_excel(inFile_endoscopy$datapath, sheet=1)
      dat <- data.frame(EndoPaste(dataFile)[1], stringsAsFactors=FALSE)
      RV$data<-dat

    }
  })
  
  observe({
    inFile_path <- input$pathology
    if (!is.null(inFile_path)) {   
      dataFile <- read_excel(inFile_path$datapath, sheet=1)
      dat <- data.frame(EndoPaste(dataFile)[1], stringsAsFactors=FALSE)
      RV2$data<-dat
      
    }
  })
  
  observe({
    inFile_merged <- input$inFile_merged
    if (!is.null(inFile_merged)) {   
      dataFile <- read_excel(inFile_merged$datapath, sheet=1)
      dat <- data.frame(EndoPaste(dataFile)[1], stringsAsFactors=FALSE)
      RV3$data<-dat
    }
  })
  
  
  
  
  ############# The tables ###################
  #Split up the dataframe with textPrep
  
  
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

  
 
  
  
  output$BarrettsTable = DT::renderDT({
    RV4$data
      

  },filter = 'top',selection = list(target = 'column'),extensions = 'Buttons', options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    pageLength = 5,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')))
  
   
  
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
  
  output$pathTable = DT::renderDT({
    RV2$data
  },selection = list(target = 'column'),options = list(scrollX = TRUE,pageLength = 5,
                                                       dom = 'Bfrtip',
                                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')))
  
  
  #Trimmed from the mergedTable data sets:
  output$trimTable = DT::renderDT({
   
    Trim$data<-RV3$data[input$mergedTable_rows_all, input$mergedTable_columns_selected]
  },selection = list(target = 'column'),extensions = 'Buttons', 
  options = list(
    fixedHeader=TRUE,
    scrollX = TRUE,
    scrollY = TRUE,
    pageLength = 5,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')))


  output$polypTable = DT::renderDT({
    polypData$data
    
  },filter = 'top',selection = list(target = 'column'),options = list(scrollX = TRUE,pageLength = 500,
                                                       dom = 'Bfrtip',
                                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')))

  
  
  ########### Dataset 1 events ############  
  #Prepare the text for endoscopy
  observeEvent(input$textPrep,{
    mywordsOGD<-input$caption
    mywordsOGD<-unlist(strsplit(mywordsOGD,","))
    RV$data<-textPrep(RV$data[,1],mywordsOGD)
    
    #Try type conversion here:
    RV$data<-type.convert(RV$data)
   
  },ignoreInit = TRUE)
  
  
  
  
  
  #Standardise the date Dataset 1
  observeEvent(input$DateStandardiserEndo,{
    RV$data[,as.numeric(input$endotable_columns_selected)]<-parse_date_time(str_extract(RV$data[,as.numeric(input$endotable_columns_selected)],
                                                                                        "(\\d{4}[[:punct:]]\\d{2}[^:alnum:]\\d{2})|(\\d{2}[^:alnum:]\\d{2}[^:alnum:]\\d{4})"),
                                                                            orders = c("dmy", "ymd"))
  },ignoreInit = TRUE) 
  
  
  #Standardise the Hospital Number Dataset 1
  observeEvent(input$HospitalNumberExtractorEndo,{
    RV$data[,as.numeric(input$endotable_columns_selected)]<-str_extract(RV$data[,as.numeric(input$endotable_columns_selected)],
                                                                        "([a-z0-9]\\d{4,}[a-z0-9])")
    
  },ignoreInit = TRUE)
  
  
  
  
   

 

  
  #Standardise the Categorical as categorical in Pathology
  observeEvent(input$CategoricalDataPath,{
    RV$data[,as.numeric(input$pathTable_columns_selected)]<-as.factor(RV$data[,as.numeric(input$pathTable_columns_selected)])
    
  },ignoreInit = TRUE)
  
  
  
  observeEvent(input$NegExPath,{
    RV$data[,as.numeric(input$pathTable_columns_selected)]<-NegativeRemove(RV$data[,as.numeric(input$pathTable_columns_selected)])
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

  
  ########### Dataset 2 events ############  
  observeEvent(input$textPrepPath,{
    mywordsPath<-input$captionPath
    mywordsPath<-unlist(strsplit(mywordsPath,","))
    RV2$data<-textPrep(RV2$data[,1],mywordsPath)
    RV2$data<-type.convert(RV2$data)
  },ignoreInit = TRUE)
  
  
  
  
  #Standardise the date Dataset 2
  observeEvent(input$DateStandardiserEPath,{
    RV2$data[,as.numeric(input$pathTable_columns_selected)]<-parse_date_time(str_extract(RV2$data[,as.numeric(input$pathTable_columns_selected)],
                                                                                         "(\\d{4}[[:punct:]]\\d{2}[^:alnum:]\\d{2})|(\\d{2}[^:alnum:]\\d{2}[^:alnum:]\\d{4})"),
                                                                             orders = c("dmy", "ymd"))
  },ignoreInit = TRUE)
  
  
  #Standardise the Hospital Number Dataset 2
  observeEvent(input$HospitalNumberExtractorPath,{
    RV2$data[,as.numeric(input$pathTable_columns_selected)]<-str_extract(RV2$data[,as.numeric(input$pathTable_columns_selected)],
                                                                         "([a-z0-9]\\d{4,}[a-z0-9])")
  },ignoreInit = TRUE) 
  
  
  observeEvent(input$textPrepMerge,{
    mywordsOGD<-input$captionMerge
    mywordsOGD<-unlist(strsplit(mywordsOGD,","))
    RV3$data<-textPrep(RV3$data[,1],mywordsOGD)
    
    #Try type conversion here:
    RV3$data<-type.convert(RV3$data)
    
  },ignoreInit = TRUE)
  
  
  
  
  ############ Basic Stats Events ############
  
  datasetInputBasicStats <- reactive({
    switch(input$datasetBasicStats,
           "Endoscopy" = RV$data,
           "Pathology" = RV2$data,
           "Merged" = RV3$data,
           "Trimmed" = Trim$data,
           "Barretts" = RV4$data)
  })
  

  

  
  sumdiplay = reactive({
    plouf <- summary(datasetInputBasicStats())
    info <- lapply(colnames(plouf),function(coln){
      valueBox(
        coln,
        paste0(plouf[,coln],collapse = "\n,"),
        icon = icon("credit-card"),
        width = 3
      )
    })
    return(info)
  })
  
  
  output$ibox <- renderUI({
    sumdiplay()
  })
  

  output$OverallPivot <- renderRpivotTable({
     rpivotTable(Trim$data)
    })
  
  
 
  
  
  
  
   
  ########### EndoMerge events ############  
  
  observeEvent(input$Endomerge2,{
    #Merge the patientID column and date from each table. Make sure that the patient ID is chosen first;
    #Need to fix this to understand when it is selecting the number. I think the user needs to 
    #convert to date and then select columns (date first) at one sitting with the datatable.
    
    RV3$data<-Endomerge2(RV$data,colnames(RV$data[as.numeric(input$endotable_columns_selected[1])]),
                         colnames(RV$data[as.numeric(input$endotable_columns_selected[2])]),
                         RV2$data,
                         colnames(RV2$data[as.numeric(input$pathTable_columns_selected[1])]),
                         colnames(RV2$data[as.numeric(input$pathTable_columns_selected[2])]))
    
    RV3$data<-RV3$data[,1:ncol(RV3$data)-1]
    
    #Create a copy that can be independently edited for the Barrett's table
    RV4$data <- RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
    
    #Create a copy that can be independently edited for the polyp table
    mypolypdata1<- RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "polyp")),]
    polypData$data <- mypolypdata1[Reduce(`|`, lapply(mypolypdata1, grepl, pattern = "colonoscopy")),]
    
    
    #Create a copy that can be filtered for the Trim table
    Trim$data<-RV3$data
    
  },ignoreInit = TRUE)
  
  
  #Standardise the date
  observeEvent(input$DateStandardiserMerge,{
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-parse_date_time(str_extract(RV3$data[,as.numeric(input$mergedTable_columns_selected)],
                                                                                           "(\\d{4}[[:punct:]]\\d{2}[^:alnum:]\\d{2})|(\\d{2}[^:alnum:]\\d{2}[^:alnum:]\\d{4})"),
                                                                               orders = c("dmy", "ymd"))
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  #Standardise the Hospital Number Merge
  observeEvent(input$HospitalNumberExtractorMerge,{
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-str_extract(RV3$data[,as.numeric(input$mergedTable_columns_selected)],
                                                                         "([a-z0-9]\\d{4,}[a-z0-9])")
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE) 
  
  
  #Standardise the Categorical data
  observeEvent(input$CategoricalDataMerge,{
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-as.factor(RV3$data[,as.numeric(input$mergedTable_columns_selected)])
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  #Standardise the Numbers as numeric in Endoscopy
  observeEvent(input$NumericDataMerge,{
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-as.numeric(str_extract(RV3$data[,as.numeric(input$mergedTable_columns_selected)],
                                                                                      "[0-9]+"))
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  #Negex Remove
  observeEvent(input$NegExMerge,{
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-NegativeRemove(RV3$data[,as.numeric(input$mergedTable_columns_selected)])
    
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  
  
  
  
  
  
  
  
  
  
  #Extract the endoscopist
  observeEvent(input$EndoscEndoscopistMerge,{
    #browser()
    RV$data[,as.numeric(input$endotable_columns_selected)]
    RV3$data[,as.numeric(input$mergedTable_columns_selected)]<-EndoscEndoscopist(RV3$data[,as.numeric(input$mergedTable_columns_selected)])
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  
  #Extract the mediaction  
  observeEvent(input$EndoscMedsMerge,{
    RV3$data<-cbind(EndoscMeds(RV3$data[,as.numeric(input$mergedTable_columns_selected)]),RV3$data)
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  #Extract the instrument
  observeEvent(input$EndoscInstrumentMerge,{
    RV3$data$mergedTable_columns_selected<-EndoscInstrument(RV3$data[,as.numeric(input$mergedTable_columns_selected)])
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  
  
  #Extract the endoscopic events
  observeEvent(input$EndoEvent,{

    cols <- as.numeric(input$mergedTable_columns_selected)
    selectedCol<-colnames(RV3$data)[cols]
    RV3$data$EndoscopyEvent<-EndoscopyEvent(RV3$data, selectedCol[1], selectedCol[2],selectedCol[3], selectedCol[4])
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
      },ignoreInit = TRUE)
  




  
  #Extract the number of biopsies  
  observeEvent(input$NumBxMerge,{
    #browser()
    RV3$data$NumBx<-HistolNumbOfBx(RV3$data[,as.numeric(input$mergedTable_columns_selected)],'specimen')
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE)
  
  #Extract the biopsy size
  observeEvent(input$BxSizeMerge,{
    RV3$data$BxSize<-HistolBxSize(RV3$data[,as.numeric(input$mergedTable_columns_selected)])
    RV4$data<-RV3$data[Reduce(`|`, lapply(RV3$data, grepl, pattern = "columnar.*?lined.*?\\.|barrett")),]
  },ignoreInit = TRUE) 
  
  
  

  
  
  ########### Barrett's events ############  
   observeEvent(input$PragueScore,{
     cols <- as.numeric(input$BarrettsTable_columns_selected)
     selectedCol<-colnames(RV4$data)[cols]
     #Need to get rid of the mytext column as it is a list and it messes up the esquiss graphics
     RV4$data<-Barretts_PragueScore(RV4$data, selectedCol[1], selectedCol[2])
     RV4$data$mytext<-NULL
     RV4$data$MStage<-as.numeric(RV4$data$MStage)
     RV4$data$CStage<-as.numeric(RV4$data$CStage)
  },ignoreInit = TRUE)
  
  
  
  observeEvent(input$PathStage,{
    cols <- as.numeric(input$BarrettsTable_columns_selected)
    selectedCol<-colnames(RV4$data)[cols]
    RV4$data$IMorNoIM<-Barretts_PathStage(RV4$data, selectedCol[1])
  },ignoreInit = TRUE)
  
  
  
  observeEvent(input$FollowUpType,{
    RV4$data$FU_Type<-Barretts_FUType(RV4$data, "CStage", "MStage", "IMorNoIM")
  },ignoreInit = TRUE)
  
  
  
  observeEvent(input$SurveillanceTime,{
    cols <- as.numeric(input$BarrettsTable_columns_selected)
    selectedCol<-colnames(RV4$data)[cols]
    RV4$data<-SurveilTimeByRow(RV4$data, selectedCol[1],selectedCol[2])
  },ignoreInit = TRUE)
  
  
  
  output$endoscopistCol<-renderUI({
    selectInput("endoscopistColChooser", label = h4("Choose the column showing the endoscopist:"),
                choices = colnames(RV4$data) ,selected = 1
    )
  })
  

  
  output$worstGradeCol<-renderUI({
    selectInput("WorstGradeChooser", label = h4("Choose the column showing the worst grade"),
                choices = colnames(RV4$data) ,selected = 1
    )
  })
  
  output$endoscopistCol_documentqual<-renderUI({
    selectInput("endoscopistColChooser_documentqual", label = h4("Choose the column showing the endoscopist:"),
                choices = colnames(RV4$data) ,selected = 1
    )
  })
  

  output$endoDoc_documentqual<-renderUI({
    selectInput("endoDoc_documentqualChoose", label = h4("Choose the endoscopic documentation column"),
                choices = colnames(RV4$data) ,selected = 1
    )
  })
  
  output$endoscopicEvent<-renderUI({
    selectInput("endoscopicEventColChooser", label = h4("Choose the column containing the events of interest"),
                choices = colnames(RV4$data) ,selected = 1
    )
  })
  output$Dates<-renderUI({
    selectInput("DateColChooser", label = h4("Choose the column containing the (formatted) dates of the endoscopies"),
                choices = colnames(RV4$data) ,selected = 1
    )
  })
  
  
  
  
  
  output$plotBarrQM <- renderPlotly({
   ggplot(RV4$data,  aes_string(x = input$WorstGradeChooser,fill=input$endoscopistColChooser)) + 
      geom_histogram(stat = "count")
  })
  
  
  
  myNotableWords <- c("[Ii]sland", "[Hh]iat|astric fold|[Pp]inch","esion|odule|lcer")
  
  
  
  output$plotBarrEQ <- renderPlotly({
    #browser()
    
    #Perform the lookup from EndoMiner for "[Ii]sland", Prague Score, "[Hh]iat|astric fold|[Pp]inch", "esion|odule|lcer"
    jj <- ListLookup(RV4$data, input$endoDoc_documentqualChoose,myNotableWords)
    ggplot(jj,  aes(x = X2,y=Prop)) + 
      geom_bar(stat="identity")

  })
  
  
  output$plotBarrTSA <- renderPlotly({
    ####Need to deal with this one:
    
    #browser()
    Endo_ResultPerformeda <- sym(input$DateColChooser)
    TestNumbers <-
      RV4$data %>% group_by(!! rlang::sym(input$endoscopicEventColChooser)) %>% 
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
  
  #output$mytabletryout = DT::renderDT({RV4$data})
  
  
  
  #Trimmed from the mergedTable data sets:
  output$BarrtrimTable = DT::renderDT({
    #browser()
    BarrTrim$data<-RV4$data[input$BarrettsTable_rows_all, input$BarrettsTable_columns_selected]
    #BarrTrim$data<-unique(BarrTrim$data)
  },selection = list(target = 'column'),extensions = 'Buttons', 
  options = list(
    fixedHeader=TRUE,
    scrollX = TRUE,
    scrollY = TRUE,
    pageLength = nrow(RV4$data),
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')))
  
  ########### Polyp events ############     
  observeEvent(input$GRS,{
    cols <- as.numeric(input$polypTable_columns_selected)
    selectedCol<-colnames(polypData$data)[cols]
    GRS_Type_Assess_By_Unit(polypData$data[input$polypTable_rows_all,], selectedCol[1],selectedCol[2], selectedCol[3], selectedCol[4])
  },ignoreInit = TRUE)
  
 
  #From EndoMineR 
  #GRS_Type_Assess_By_Unit(dataframe, ProcPerformed, Endo_Endoscopist, Dx,Histol) 
  
  output$GRS_Table = DT::renderDT({
    cols <- as.numeric(input$polypTable_columns_selected)
    selectedCol<-colnames(polypData$data)[cols]
    browser()
    GRS_Type_Assess_By_Unit(polypData$data[input$polypTable_rows_all,], selectedCol[1],selectedCol[2], selectedCol[3], selectedCol[4])
  
    },filter = 'top',selection = list(target = 'column'),extensions = 'Buttons', options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    pageLength = 5,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'))) 
  
  
  ########### Graphic events ############   
  observeEvent(input$data,{
      # Launch with:
    if (input$data == "Endoscopy") {
      data_r$data <- RV$data
      data_r$name <- "Endoscopy"
    } else if (input$data == "Pathology") {
      data_r$data <- RV2$data
      data_r$name <- "Pathology"
    } else if (input$data == "Merged data") {
      data_r$data <- RV3$data
      data_r$name <- "mergedData"
    } else if (input$data == "Barretts") {
      data_r$data <- RV4$data
      data_r$name <- "Barretts"
    } else if (input$data == "Trimmed") {
      data_r$data <- Trim$data
      data_r$name <- "Trimmed"
    }
  },ignoreInit = TRUE)
  
  callModule(module = esquisserServer, id = "esquisse", data = data_r)

}

