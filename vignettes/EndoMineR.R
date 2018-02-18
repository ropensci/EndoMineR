## ----setup, include=FALSE------------------------------------------------
library(pander)
library(EndoMineR)
knitr::opts_chunk$set(echo = TRUE)

## ----exampleNewLines, eval = FALSE---------------------------------------
#  v<-NewLines(TheOGDReportFinal,'OGDReportWhole')

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

## ----exampleEndoscEndoscopist, echo = FALSE------------------------------
pander(head(Myendo[2:6],10))

## ----exampleEndoscEndoscopist2,echo = TRUE-------------------------------
Myendo2<-EndoscEndoscopist(Myendo,'Endoscopist')

## ----exampleEndoscEndoscopist3, echo = FALSE-----------------------------
pander(head(Myendo2[2:6],10))

## ----exampleRawEndoExtract, echo = TRUE----------------------------------
mywords<-c("Hospital:","Hospital Number:","Patient Name:","General Practitioner:","Date of procedure:","Endoscopist:","Second Endoscopist",
           "Medications:","Instrument:","Extent of Exam:","Indications:","Procedure Performed:",
"Findings:","Diagnosis:")
TheOGDReportFinal2<-Extractor(TheOGDReportFinal,"OGDReportWhole",mywords)


## ----exampleEndoCleaningFuncMed, echo = TRUE-----------------------------
TheOGDReportFinal2df<-data.frame(TheOGDReportFinal2["HospitalNumber"],TheOGDReportFinal2["Instrument"],TheOGDReportFinal2["Indications"],TheOGDReportFinal2["Medications"],TheOGDReportFinal2["ProcedurePerformed"])
pander(head(TheOGDReportFinal2df,10))
v<-EndoscMeds(TheOGDReportFinal2df,'Medications')
pander(head(v,10))

## ----exampleEHistolHistol, echo = TRUE-----------------------------------
pander(head(Mypath$Histology,2))

## ----exampleEHistolHistol2, echo = TRUE----------------------------------
t<-HistolHistol(Mypath,'Histology')
pander(head(t$Histology,2))

## ----exampleNeg, echo = TRUE---------------------------------------------
Mypath[7,10]

## ----exampleNeg2, echo = TRUE--------------------------------------------
MypathNegRem<-NegativeRemove(Mypath,"Diagnosis")
MypathNegRem[7,10]

## ----exampleHistolNumbOfBx, eval = FALSE---------------------------------
#  v<-HistolNumbOfBx(Mypath,'Macroscopicdescription','specimen')

## ----exampleHistolExtrapolDx, echo = TRUE--------------------------------
Mypath3<-data.frame(Mypath["HospitalNumber"],Mypath["Diagnosis"])
pander(head(Mypath3,10))

## ----exampleHistolExtrapolDx2, echo = TRUE-------------------------------
Mypath3<-HistolExtrapolDx(Mypath3,"Diagnosis")
pander(head(Mypath3,10))

## ----exampleOtherFunctionsHistology, eval = FALSE------------------------
#  
#  v<-HistolAccessionNumber(Mypath,"Histology","SP-\\d{2}-\\d{7}")
#  v<-HistolMacDescripCleanup(Mypath,"Macroscopicdescription")

