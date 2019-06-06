
library(shiny)
library(EndoMineR)
library(stringr)
library(stringi)
library(readxl)
library(DT)

# Define server logic required to draw a histogram

RV <- reactiveValues(data = data.frame())

RV2 <- reactiveValues(data = data.frame())
RV3 <- reactiveValues(data = data.frame())
RV4 <- reactiveValues(data = data.frame())
RV5 <- reactiveValues(data = data.frame())
RV6 <- reactiveValues(data = data.frame())
RV7 <- reactiveValues(data = data.frame())
RV8 <- reactiveValues(data = data.frame())
RV9 <- reactiveValues(data = data.frame())
RV10 <- reactiveValues(data = data.frame())



server <- function(input, output) {
 
  observe({
    inFile_endoscopy <- input$endoscopy
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
    inFile_merged <- input$merged
    if (!is.null(inFile_merged)) {   
      dataFile <- read_excel(inFile_merged$datapath, sheet=1)
      dat <- data.frame(EndoPaste(dataFile)[1], stringsAsFactors=FALSE)
      RV3$data<-dat
    }
  })
  
  
  
  
  ############# The tables ###################
  #Split up the dataframe with textPrep
  output$endotable = DT::renderDT({
    if (!is.null(RV$data)) {  
      
      RV$data[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(RV$data),'"><br>')
    
      RV$data[["Actions"]]<-
      paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
             <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(RV$data),'>Delete</button>
             </div>
             ')
    }
    
    
 datatable(RV$data,escape=F, extensions = "Select", selection = "none",callback = JS("table.on('click', 'tbody td', function(){",
                                                                                     "  // if the column is already selected, deselect it:",
                                                                                     "  if(table.column(this, {selected: true}).length){",
                                                                                     "    table.column(this).deselect();",
                                                                                     "  // otherwise, select the column unless there's the class 'notselectable':",
                                                                                     "  } else if(!$(this).hasClass('notselectable')){",
                                                                                     "    table.column(this).select();",
                                                                                     "  }",
                                                                                     "});"),
           options = list(scrollX = TRUE,
                          pageLength = 5,
                          columnDefs = list(
                            list(className = "notselectable", targets = c(-1,-2))
                          ),
                          select = "api"))
  
    
  })
  
  output$pathTable = DT::renderDT({
    RV2$data
  },selection = list(target = 'column'),options = list(scrollX = TRUE,pageLength = 5))
  
  output$mergedTable = DT::renderDT({
    RV3$data
  },selection = list(target = 'column'),options = list(scrollX = TRUE,pageLength = 5))

  output$polypTable = DT::renderDT({
    RV4$data
  },options = list(scrollX = TRUE))
  
  #To do- the selection of columns only gives the index back not the name of the column
  output$metricTable = DT::renderDT({
    RV5$data
  },options = list(scrollX = TRUE))
  
  output$plot <- renderPlot({
    cols <- as.numeric(input$mergedTable_columns_selected)
    selectedCol<-colnames(RV3$data)[cols]
    #RV5$data<-MetricByEndoscopist(RV3$data,'endoscopist',selectedCol)
    EndoBasicGraph(RV3$data, "endoscopist", selectedCol)
  })

  
  
  
  ############# The function events ###################
  
  
  ########### Endoscopy events ############  
  #Prepare the text for endoscopy
  observeEvent(input$textPrep,{
    mywordsOGD<-input$caption
    mywordsOGD<-unlist(strsplit(mywordsOGD,","))
    RV$data<-textPrep(RV$data[,1],mywordsOGD,NegEx="TRUE")
   
  },ignoreInit = TRUE)
  
  #Extract the endoscopist
  observeEvent(input$EndoscEndoscopist,{
    RV$data$endoscopist<-EndoscEndoscopist(RV$data$endoscopist)
  },ignoreInit = TRUE)
  
  #Extract the medications
  observeEvent(input$EndoscMeds,{
    RV$data<-cbind(EndoscMeds(RV$data$medications),RV$data)
  },ignoreInit = TRUE)
  
  #Extract the instrument
  observeEvent(input$EndoscInstrument,{
    RV$data$instrument<-EndoscInstrument(RV$data$instrument)
  },ignoreInit = TRUE)
  
  ########### Generic events ############ 
  #Standardise the date
  observeEvent(input$DateStandardiserEndo,{
   
    RV$data[,as.numeric(input$endotable_columns_selected)]<-str_extract(RV$data[,as.numeric(input$endotable_columns_selected)],
                                                                        "(\\d{4}[[:punct:]]\\d{2}[^:alnum:]\\d{2})|(\\d{2}[^:alnum:]\\d{2}[^:alnum:]\\d{4})")
  },ignoreInit = TRUE)
  
  
  #Standardise the date
  observeEvent(input$DateStandardiserEPath,{
    RV2$data[,as.numeric(input$pathTable_columns_selected)]<-str_extract(RV2$data[,as.numeric(input$pathTable_columns_selected)],
                                                                        "(\\d{4}[[:punct:]]\\d{2}[^:alnum:]\\d{2})|(\\d{2}[^:alnum:]\\d{2}[^:alnum:]\\d{4})")
  },ignoreInit = TRUE)
  
  observeEvent(input$Del_row_head,{
    row_to_del=as.numeric(gsub("Row","",input$checked_rows))
    
    RV$data=RV$data[-row_to_del]}
  )
  
  
  observeEvent(input$lastClick,
               {
                 if (input$lastClickId%like%"delete")
                 {
                   row_to_del=as.numeric(gsub("delete_","",input$lastClickId))
                   RV$data=RV$data[-row_to_del]
                 }
                 else if (input$lastClickId%like%"modify")
                 {
                   showModal(modal_modify)
                 }
               }
  )
  

  
  
  ########### Pathology events ############  
  observeEvent(input$textPrepPath,{
    mywordsPath<-input$captionPath
    mywordsPath<-unlist(strsplit(mywordsPath,","))
    RV2$data<-textPrep(RV2$data[,1],mywordsPath,NegEx="TRUE")
  },ignoreInit = TRUE)
  
  #Extract the endoscopist
  observeEvent(input$NumBx,{
    RV2$data$NumBx<-HistolNumbOfBx(RV2$data$macroscopicdescription,'specimen')
  },ignoreInit = TRUE)
  
  #Extract the medications
  observeEvent(input$BxSize,{
    RV2$data$BxSize<-HistolBxSize(RV2$data$macroscopicdescription)
  },ignoreInit = TRUE)
  

  
  ########### EndoMerge events ############  
  
  observeEvent(input$Endomerge2,{
    #Merge the patientID column and date from each table. Make sure that the patient ID is chosen first;
    browser()
    RV3$data<-Endomerge2(RV$data,
                         colnames(RV$data[as.numeric(input$endotable_columns_selected[1])]),
                         colnames(RV$data[as.numeric(input$endotable_columns_selected[2])]),
                         RV2$data,
                         colnames(RV2$data[as.numeric(input$pathTable_columns_selected[1])]),
                         colnames(RV2$data[as.numeric(input$pathTable_columns_selected[2])]))
  },ignoreInit = TRUE)

  
  
  ########### Barrett's events ############  
   observeEvent(input$PragueScore,{
     RV3$data<-Barretts_PragueScore(RV3$data, "findings", "findings")
  },ignoreInit = TRUE)
  
  observeEvent(input$PathStage,{
    RV3$data$IMorNoIM<-Barretts_PathStage(RV3$data, "diagnosis")
  },ignoreInit = TRUE)
  
  observeEvent(input$FollowUpType,{
    RV3$data$FU_Type<-Barretts_FUType(RV3$data, "CStage", "MStage", "IMorNoIM")
  },ignoreInit = TRUE)
  
  observeEvent(input$DataViz4,{
    RV3$data$IMorNoIM<-BarrettsAll(RV3$data, "findings","findings",RV3$data,"diagnosis","diagnosis")
  },ignoreInit = TRUE)

  
  ########### Polyp events ############     

  observeEvent(input$GRS,{
    RV4$data<-GRS_Type_Assess_By_Unit(RV3$data, "procedureperformed","endoscopist", "diagnosis", "diagnosis")
  },ignoreInit = TRUE)
  
  ########### Graphic events ############   
  
  
  observeEvent(input$MetricByEndoscopist,{
     cols <- as.numeric(input$mergedTable_columns_selected)
     selectedCol<-colnames(RV3$data)[cols]
     RV5$data<-MetricByEndoscopist(RV3$data,'endoscopist',selectedCol)
  },ignoreInit = TRUE)
  
}

