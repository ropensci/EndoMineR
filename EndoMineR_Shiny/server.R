
library(shiny)
library(EndoMineR)
# Define server logic required to draw a histogram

RV <- reactiveValues(data = TheOGDReportFinal)
RV2 <- reactiveValues(data = PathDataFrameFinal)

server <- function(input, output) {
 
#Do the extraction  
  mywordsOGD<-c("hospital number:","patient name:","general practitioner","date of procedure:",
             "endoscopist:","2nd endoscopist:","medications:","instrument:","extent of exam:",
             "indications:", "procedure performed:","findings:","diagnosis:")
  
  mywordsPath<-c("hospital number:","patient name:","dob:","general practitioner:",
                   "date received:","nature of specimen:","macroscopic description:" ,"diagnosis:")
  
  #Split up the dataframe with textPrep
  output$endotable = DT::renderDT({
    RV$data
  })
  output$pathTable = DT::renderDT({
    RV2$data
  })

  
  
  
  
  
  ###########Endoscopy buttons############  
  #Prepare the text for endoscopy
  observeEvent(input$textPrep,{
    RV$data<-textPrep(RV$data$OGDReportWhole,mywordsOGD,NegEx="TRUE",Extractor="1")
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

  
  
  
  
  
  ########### Pathology buttons############  
  observeEvent(input$textPrepPath,{
    RV2$data<-textPrep(RV2$data$PathReportWhole,mywordsPath,NegEx="TRUE",Extractor="1")
  },ignoreInit = TRUE)
  
  #Extract the endoscopist
  observeEvent(input$NumBx,{
    RV2$data$NumBx<-HistolNumbOfBx(RV2$data$macroscopicdescription,'specimen')
  },ignoreInit = TRUE)
  
  #Extract the medications
  observeEvent(input$BxSize,{
    RV2$data$BxSize<-HistolBxSize(RV2$data$macroscopicdescription)
  },ignoreInit = TRUE)
  
  #Extract the instrument
  observeEvent(input$EndoscInstrument,{
    RV$data$instrument<-EndoscInstrument(RV$data$instrument)
  },ignoreInit = TRUE)
  
  observeEvent(input$Endomerge2,{
    RV$data<-Endomerge2(RV$data,"dateofprocedure","hospitalnumber",RV$data,"datereceived","hospitalnumber")
  },ignoreInit = TRUE)
  
}
