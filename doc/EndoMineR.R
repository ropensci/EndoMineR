## ----setup, include=FALSE-----------------------------------------------------
library(pander)
library(EndoMineR)
library(here)
knitr::opts_chunk$set(echo = TRUE)

## ----global_options, include=FALSE--------------------------------------------
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)

## ----fig.width=8, fig.height=8,fig.align='center',out.width = "100%",echo=FALSE----
knitr::include_graphics("img/EndoMineRBasic.jpg")

## ----exampleEndoPaste,echo=TRUE-----------------------------------------------

#An example dataset
testList<-structure(list(PatientName = c("Tom Hardy", "Elma Fudd", "Bingo Man"), 
                         HospitalNumber = c("H55435", "Y3425345", "Z343424"), Text = c("All bad. Not good", 
"Serious issues", "from a land far away")), class = "data.frame", row.names = c(NA, -3L))
myReadyDataset<-EndoPaste(testList)



## ----exampleExtractor,echo=TRUE-----------------------------------------------
PathDataFrameFinalColon2<-PathDataFrameFinalColon
names(PathDataFrameFinalColon2)<-"PathReportWhole"
pander(head(PathDataFrameFinalColon2,1))

## ----exampleExtractortbl,echo=FALSE-------------------------------------------
pander(head(PathDataFrameFinalColon2,1))

## ----exampleExtractor2,echo=TRUE----------------------------------------------
library(EndoMineR)
mywords<-c("Hospital Number","Patient Name:","DOB:","General Practitioner:",
"Date received:","Clinical Details:","Macroscopic description:",
"Histology:","Diagnosis:")

PathDataFrameFinalColon3<-Extractor(PathDataFrameFinalColon2$PathReportWhole,mywords)


## ----exampleExtractor3,echo=FALSE---------------------------------------------

panderOptions('table.split.table', Inf)
pander(head(PathDataFrameFinalColon3[,1:9],1))

## ----exampletextPrep,echo=FALSE-----------------------------------------------
#Submit delimiters
mywords<-c("Hospital Number","Patient Name:","DOB:","General Practitioner:",
"Date received:","Clinical Details:","Macroscopic description:",
"Histology:","Diagnosis:")
CleanResults<-textPrep(PathDataFrameFinal$PathReportWhole,mywords)

## ----exampleEndoscEndoscopist2,echo = TRUE------------------------------------
EndoscEndoscopist(Myendo$Endoscopist[2:6])

## ----exampleEndomerge2,echo=TRUE----------------------------------------------
v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',Mypath,'Dateofprocedure','HospitalNumber')

## ----exampleEndoCleaningFuncMed, echo = TRUE----------------------------------
MyendoMeds<-cbind(EndoscMeds(Myendo$Medications), Myendo)

## ----exampleEndoCleaningFuncMedtbl, echo = FALSE------------------------------
pander(head(MyendoMeds[1:4],5))


## ----exampleHistolNumbOfBx1, echo = TRUE--------------------------------------
sg<-data.frame(Mypath$HospitalNumber,Mypath$PatientName,Mypath$Macroscopicdescription)

## ----exampleHistolNumbOfBx1tbl, echo = FALSE----------------------------------
pander(head(sg,5))

## ----exampleHistolNumbOfBx2, echo = TRUE--------------------------------------
Mypath$NumbOfBx<-HistolNumbOfBx(Mypath$Macroscopicdescription,'specimen')
sh<-data.frame(Mypath$HospitalNumber,Mypath$PatientName,Mypath$NumbOfBx)

## ----exampleHistolNumbOfBx2tbl, echo = FALSE----------------------------------
pander(head(sh,5))

## ----exampleHistolBxSize1, echo = TRUE----------------------------------------
Mypath$BxSize<-HistolBxSize(Mypath$Macroscopicdescription)
sh<-data.frame(Mypath$HospitalNumber,Mypath$PatientName,Mypath$BxSize)

## ----exampleHistolBxSize1tbl, echo = FALSE------------------------------------

pander(head(sh,5))

