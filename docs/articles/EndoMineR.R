## ----setup, include=FALSE------------------------------------------------
library(pander)
library(EndoMineR)
knitr::opts_chunk$set(echo = TRUE)

## ----exampleNewLines, eval = FALSE---------------------------------------
#  v<-NewLines(Myendo,'OGDReportWhole')

## ----exampleExtractor,echo=FALSE-----------------------------------------
PathDataFrameFinalColon2<-PathDataFrameFinalColon
#pander(head(Mypath,3))

names(PathDataFrameFinalColon2)<-"PathReportWhole"
pander(head(PathDataFrameFinalColon2,1))

## ----exampleExtractor2---------------------------------------------------
mywords<-c("Hospital Number","Patient Name:","DOB:","General Practitioner:",
"Date received:","Clinical Details:","Macroscopic description:",
"Histology:","Diagnosis:")
PathDataFrameFinalColon2<-Extractor(PathDataFrameFinalColon2,"PathReportWhole",mywords)


## ----exampleExtractor3,echo=FALSE----------------------------------------
PathDataFrameFinalColon2<-head(PathDataFrameFinalColon2[2:10],1)
pander::panderOptions('table.split.table', Inf)
pander::panderOptions('table.split.cells', Inf)
pander(head(PathDataFrameFinalColon2,1))

## ----exampleEndoscEndoscopist, eval = FALSE------------------------------
#  EndoscEndoscopist(Myendo,'Endoscopist')

## ----exampleEndoCleaningFunc, eval = FALSE-------------------------------
#  v<-EndoscMeds(Myendo,'Medications')
#  v<-EndoscInstrument(Myendo,'Instrument')
#  v<-EndoscIndications(Myendo,'Indications')
#  v<-EndoscProcPerformed(Myendo,'ProcedurePerformed')

## ----exampleEHistolHistol, eval = FALSE----------------------------------
#  t<-HistolHistol(Mypath,'Histology')

## ----exampleHistolNumbOfBx, eval = FALSE---------------------------------
#  v<-HistolNumbOfBx(Mypath,'Macroscopicdescription','specimen')

## ----exampleOtherFunctionsHistology, eval = FALSE------------------------
#  v<-HistolMacDescripCleanup(Mypath,"Macroscopicdescription")
#  v<-HistolExtrapolDx(Mypath,"Diagnosis")
#  v<-HistolAccessionNumber(Mypath,"Histology","SP-\\d{2}-\\d{7}")

