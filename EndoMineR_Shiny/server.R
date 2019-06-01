
library(shiny)
library(EndoMineR)
library(stringr)
library(stringi)
# Define server logic required to draw a histogram

RV <- reactiveValues(data = TheOGDReportFinal)
RV2 <- reactiveValues(data = PathDataFrameFinal)
RV3 <- reactiveValues(data = data.frame())
RV4 <- reactiveValues(data = data.frame())
RV5 <- reactiveValues(data = data.frame())
RV6 <- reactiveValues(data = data.frame())
RV7 <- reactiveValues(data = data.frame())
RV8 <- reactiveValues(data = data.frame())
RV9 <- reactiveValues(data = data.frame())
RV10 <- reactiveValues(data = data.frame())



server <- function(input, output) {
 
#Do the extraction  
  # mywordsOGD<-c("hospital number:","patient name:","general practitioner","date of procedure:","endoscopist:","2nd endoscopist:","medications:","instrument:","extent of exam:","endoscopist:","2nd endoscopist:","medications:","instrument:","extent of exam:","indications:", "procedure performed:","findings:","diagnosis:")


  mywordsPath<-c("hospital number:","patient name:","dob:","general practitioner:",
                   "date received:","nature of specimen:","macroscopic description:" ,"diagnosis:")
  
  
  ############# The tables ###################
  #Split up the dataframe with textPrep
  output$endotable = DT::renderDT({
    RV$data
  },options = list(scrollX = TRUE))
  output$pathTable = DT::renderDT({
    RV2$data
  },options = list(scrollX = TRUE))
  
  output$mergedTable = DT::renderDT({
    RV3$data
  },selection = list(target = 'column'),options = list(scrollX = TRUE))

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
    RV$data<-textPrep(RV$data$OGDReportWhole,mywordsOGD,NegEx="TRUE")
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

  
  
  
  
  
  ########### Pathology events ############  
  observeEvent(input$textPrepPath,{
    mywordsPath<-input$captionpath
    mywordsPath<-unlist(strsplit(mywordsPath,","))
    RV2$data<-textPrep(RV2$data$PathReportWhole,mywordsPath,NegEx="TRUE")
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
    RV3$data<-Endomerge2(RV$data,"dateofprocedure","hospitalnumber",RV2$data,"datereceived","hospitalnumber")
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
  
  #
  observeEvent(input$MetricByEndoscopist,{
     cols <- as.numeric(input$mergedTable_columns_selected)
     selectedCol<-colnames(RV3$data)[cols]
     RV5$data<-MetricByEndoscopist(RV3$data,'endoscopist',selectedCol)
  },ignoreInit = TRUE)
  
}

