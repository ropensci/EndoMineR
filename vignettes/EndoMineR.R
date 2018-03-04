## ----setup, include=FALSE------------------------------------------------
library(pander)
library(EndoMineR)
knitr::opts_chunk$set(echo = TRUE)

## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)

## ----fig.width=8, fig.height=8,fig.align='center',out.width = "50%",echo=FALSE----
knitr::include_graphics("img/EndoMineROverView.png")

## ----fig.width=12, fig.height=8,fig.align='center',echo=FALSE------------
knitr::include_graphics("img/EndoMineR_Extractor.svg")

## ----exampleExtractor,echo=FALSE-----------------------------------------
PathDataFrameFinalColon2<-PathDataFrameFinalColon
names(PathDataFrameFinalColon2)<-"PathReportWhole"
pander(head(PathDataFrameFinalColon2,1))

## ----exampleExtractor2,echo=TRUE-----------------------------------------
mywords<-c("Hospital Number","Patient Name:","DOB:","General Practitioner:",
"Date received:","Clinical Details:","Macroscopic description:",
"Histology:","Diagnosis:")
PathDataFrameFinalColon2<-Extractor(PathDataFrameFinalColon2,"PathReportWhole",mywords)


## ----exampleExtractor3,echo=FALSE----------------------------------------
PathDataFrameFinalColon2<-head(PathDataFrameFinalColon2[2:10],1)
pander::panderOptions('table.split.table', Inf)
pander(head(PathDataFrameFinalColon2,1))

## ----fig.width=12, fig.height=8,fig.align='center',echo=FALSE------------
knitr::include_graphics("img/EndoMineR_EndoscopyClean.png")

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


## ----exampleEndoCleaningFuncMed2, echo = FALSE---------------------------
pander(head(v,10))

## ----fig.width=12, fig.height=8,fig.align='center',echo=FALSE------------
knitr::include_graphics("img/EndoMineR_HistopExtrapol.png")

## ----exampleEHistolHistol, echo = FALSE----------------------------------
head(Mypath$Histology,2)

## ----exampleEHistolHistol2, echo = TRUE----------------------------------
t<-HistolHistol(Mypath,'Histology')

## ----exampleEHistolHistol3, echo = FALSE---------------------------------
head(t$Histol_Simplified,2)

## ----exampleHistolDx1, echo = FALSE--------------------------------------
head(Mypath$Diagnosis,2)


## ----exampleHistolDx2, echo = TRUE---------------------------------------
t<-HistolDx(Mypath,'Diagnosis')

## ----exampleHistolDx3, echo = FALSE--------------------------------------
head(t$Dx_Simplified,2)

## ----exampleHistolNumbOfBx1, echo = FALSE--------------------------------
sg<-data.frame(Mypath$HospitalNumber,Mypath$PatientName,Mypath$Macroscopicdescription)
pander(head(sg,5))

## ----exampleHistolNumbOfBx2, echo = TRUE---------------------------------
v<-HistolNumbOfBx(Mypath,'Macroscopicdescription','specimen')

## ----exampleHistolNumbOfBx3, echo = FALSE--------------------------------
sh<-data.frame(v$HospitalNumber,v$PatientName,v$NumbOfBx)
pander(head(sh,5))

## ----exampleHistolExtrapolDx, echo = TRUE--------------------------------
Mypath3<-data.frame(Mypath["HospitalNumber"],Mypath["Diagnosis"])


## ----exampleHistolExtrapolDx2, echo = FALSE------------------------------
pander(head(Mypath3,10))

## ----exampleHistolExtrapolDx3, echo = TRUE-------------------------------
Mypath3<-HistolExtrapolDx(Mypath3,"Diagnosis")

## ----exampleHistolExtrapolDx4, echo = FALSE------------------------------
pander(head(Mypath3,10))

## ----exampleOtherFunctionsHistology, eval = FALSE------------------------
#  
#  v<-HistolAccessionNumber(Mypath,"Histology","SP-\\d{2}-\\d{7}")

## ----exampleNeg, echo = FALSE--------------------------------------------
Mypath[7,10]

## ----exampleNeg2, echo = TRUE--------------------------------------------
MypathNegRem<-NegativeRemove(Mypath,"Diagnosis")

## ----exampleNeg3, echo = FALSE-------------------------------------------
MypathNegRem[7,10]

