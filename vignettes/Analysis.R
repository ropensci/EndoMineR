## ----setup, include=FALSE------------------------------------------------
library(pander)
library(EndoMineR)
knitr::opts_chunk$set(echo = TRUE)


## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set( warning=FALSE, message=FALSE)

## ----fig.width=12, fig.height=8,fig.align='center',echo=FALSE------------
knitr::include_graphics("img/EndoMineR_Surveillance.svg")

## ----exampleSurveillanceTimeByRow, eval = TRUE---------------------------
em1<-SurveilTimeByRow(Myendo,'HospitalNumber','Dateofprocedure')
pander(head(data.frame(em1[2],em1[ncol(em1)]),5))
em2<-SurveilLastToNow(Myendo,'HospitalNumber','Dateofprocedure')
pander(head(data.frame(em2[2],em2[ncol(em2)]),5))
em3<-SurveilLastTest(Myendo,'HospitalNumber','Dateofprocedure')
pander(head(data.frame(em3[2],em3[5]),5))
em4<-SurveilFirstTest(Myendo,'HospitalNumber','Dateofprocedure')
pander(head(data.frame(em4[2],em4[5]),5))

## ----exampleSurveillanceCapacity-----------------------------------------
em5<-SurveilCapacity(Myendo,"Dateofprocedure")


## ----exampleSurveillanceCapacity2,echo=FALSE-----------------------------
pander::panderOptions('table.split.table', Inf)
pander(head(em5))

## ----exampleHowManyTests-------------------------------------------------
how<-HowManyTests(Myendo,'Indications','Dateofprocedure','Surv')
how[1]

## ----exampleSurveySankey, eval = FALSE-----------------------------------
#  how<-SurveySankey(Myendo,"ProcPerformed")

## ----fig.width=12, fig.height=8,fig.align='center',echo=FALSE,out.width = "100%"----
knitr::include_graphics("img/EndoMineR_Sankey.svg")

## ----examplePatientFlow_CircosPlots, eval = FALSE------------------------
#  flow<-PatientFlow_CircosPlots(v,"Dateofprocedure","HospitalNumber","ProcedurePerformed")

## ----fig.width=12, fig.height=8,fig.align='center',echo=FALSE,out.width = "60%"----
knitr::include_graphics("img/EndoMineR_Circos.svg")

## ----exampleListLookup, eval = TRUE,echo=FALSE---------------------------
pander::panderOptions('table.split.table', Inf)
pander(head(data.frame(Myendo[2:3],Myendo[13])))

## ----exampleListLookup2, eval = TRUE-------------------------------------
myNotableWords <- c("arrett", "oeliac")
tt<-ListLookup(Myendo,'Findings',myNotableWords)

## ----exampleListLookup3, echo=FALSE--------------------------------------
pander::panderOptions('table.split.table', Inf)
pander(head(tt))

## ----exampleEndoscChopperMeds--------------------------------------------
Myendo<-EndoscMeds(Myendo,'Medications')
pander(head(data.frame(Myendo$HospitalNumber,Myendo$Fent,Myendo$Midaz),10))
MetricByEndoscopist(Myendo,'Endoscopist','Fent')

## ----exampleTermStandardLocation-----------------------------------------
f<-TermStandardLocation(Mypath,'Histology')

#Just some column selection
fgg<-data.frame(f[(ncol(f)-1)],f[(ncol(f))])
pander(head(fgg,10))

## ----examplePolypTidyUpLocator-------------------------------------------
fgg<-PolypLocator(fgg,'SampleLocation') 

#Just some column selection
fg<-data.frame(fgg[(ncol(fgg)-2)],fgg[(ncol(fgg)-1)],fgg[(ncol(fgg))])
pander(head(fg,3))

## ----exampleGRS_Type_Assess_By_Unit, echo = TRUE,message=FALSE-----------
#Import the endoscopy and pathology datasets for colonoscopy
 MypathColon<-PathDataFrameFinalColon
 MyendoColon <- ColonFinal
 #Rename the columns so that numbers do not lead the column titles
 MyendoColon$OGDReportWhole <-gsub("2nd Endoscopist:","Second endoscopist:",
 MyendoColon$OGDReportWhole)
 #Extract the columns needed from endoscopy
 EndoscTree <-c("Hospital Number:","Patient Name:","General Practitioner:",
        "Date of procedure:","Endoscopist:","Second endoscopist:","Medications",
        "Instrument","Extent of Exam:","Indications:","Procedure Performed:",
        "Findings:","Endoscopic Diagnosis:")
 MyendoColon<-Extractor(MyendoColon,"OGDReportWhole",EndoscTree)
 #Extract the columns needed from Histology
 Histoltree <-c(
     "Hospital Number:","Patient Name:","DOB:","General Practitioner:",
     "Date received:","Clinical Details","Nature of specimen","Macroscopic description:","Histology",
     "Diagnosis")
#Tidy up to allow merge
 MypathColon <-Extractor(MypathColon,"PathReportWhole",Histoltree)
 names(MypathColon)[names(MypathColon) == 'Datereceived'] <- 'Dateofprocedure'
 MypathColon$Dateofprocedure <- as.Date(MypathColon$Dateofprocedure)
 #Do the merge
 vColon <-Endomerge2(MypathColon, "Dateofprocedure","HospitalNumber",
                     MyendoColon, "Dateofprocedure","HospitalNumber")

 
 #Use the function
GRSTable<-GRS_Type_Assess_By_Unit(vColon,'ProcedurePerformed','Endoscopist','Diagnosis','Histology')


## ----exampleGRS_Type_Assess_By_Unit1, echo = FALSE,message=FALSE---------
pander::panderOptions('table.split.table', Inf)
pander(GRSTable)

