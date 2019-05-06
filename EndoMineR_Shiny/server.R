
library(shiny)
library(EndoMineR)
library(stringr)
library(stringi)
# Define server logic required to draw a histogram

RV <- reactiveValues(data = TheOGDReportFinal)
RV2 <- reactiveValues(data = PathDataFrameFinal)
RV3 <- reactiveValues(data = data.frame())


server <- function(input, output) {
 
#Do the extraction  
  # mywordsOGD<-c("hospital number:","patient name:","general practitioner","date of procedure:","endoscopist:","2nd endoscopist:","medications:","instrument:","extent of exam:","endoscopist:","2nd endoscopist:","medications:","instrument:","extent of exam:","indications:", "procedure performed:","findings:","diagnosis:")


  mywordsPath<-c("hospital number:","patient name:","dob:","general practitioner:",
                   "date received:","nature of specimen:","macroscopic description:" ,"diagnosis:")
  
  #Split up the dataframe with textPrep
  output$endotable = DT::renderDT({
    RV$data
  })
  output$pathTable = DT::renderDT({
    RV2$data
  })
  output$mergedTable = DT::renderDT({
    RV3$data
  })

  
  
  
  
  
  ###########Endoscopy buttons############  
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

  
  
  
  
  
  ########### Pathology buttons############  
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
  
  #Extract the instrument
  observeEvent(input$EndoscInstrument,{
    RV$data$instrument<-EndoscInstrument(RV$data$instrument)
  },ignoreInit = TRUE)
  
  observeEvent(input$Endomerge2,{
    RV3$data<-Endomerge2(RV$data,"dateofprocedure","hospitalnumber",RV2$data,"datereceived","hospitalnumber")
  },ignoreInit = TRUE)
  
  
  ########### Barrett's buttons############  
#   observeEvent(input$PragueScore,{
#     RV3$data$PragueScore<-Barretts_PragueScore(RV3$data, "findings", "Original.y")
#  },ignoreInit = TRUE)
   
  
}
