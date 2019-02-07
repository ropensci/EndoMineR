## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,message=FALSE, warning=FALSE)
library(knitr)
library(EndoMineR)
library(pander)
library(prettydoc)

## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)

## ----fig.width=12, fig.height=8,fig.align='center',echo=FALSE------------
knitr::include_graphics("img/BarrettsGuide.png")

## ----exampleBarretts_PragueScore,echo = TRUE,message=FALSE, warning=FALSE----
v<-Barretts_PragueScore(Myendo,'Findings')


## ----exampleBarretts_PragueScore2,echo = FALSE,message=FALSE, warning=FALSE----

panderOptions('table.split.table', Inf)
pander(v[23:27,(ncol(v)-4):ncol(v)])

## ----exampleBarretts_PathStage, echo = TRUE,message=FALSE, warning=FALSE----

# The histology is then merged with the Endoscopy dataset. The merge occurs
# according to date and Hospital number
b<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',Mypath,'Dateofprocedure',
'HospitalNumber')

#Picking out only the Barrett's related endoscopies
b<-b[grepl("Surv",b$Indications),]

b<-Barretts_PathStage(b,'Histology')


## ----exampleBarretts_PathStage2, echo = TRUE,message=FALSE, warning=FALSE,echo=FALSE----

panderOptions('table.split.table', Inf)
pander(b[20:23,(ncol(b)-4):ncol(b)])

## ----exampleBarretts_EventType, echo = TRUE,message=FALSE, warning=FALSE----
v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",Mypath,"Dateofprocedure","HospitalNumber")



## ----exampleBarretts_FUType, echo = TRUE,message=FALSE, warning=FALSE----
v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",Mypath,"Dateofprocedure","HospitalNumber")
b<-Barretts_PathStage(v,'Histology')
b1<-Barretts_PragueScore(b)
b3<-Barretts_FUType(b1)


## ----exampleBarretts_FUType2, echo = FALSE,message=FALSE, warning=FALSE,echo=FALSE----

panderOptions('table.split.table', Inf)
pander(b3[23:27,(ncol(b3)-4):ncol(b3)])



## ----exampleBarretts_LesionRecognitionEMR2, echo = TRUE,fig.width=7,fig.height=5,message=FALSE, warning=FALSE----
# As long as the code int he chunk above has been run then all that needs to be done is:
BarrettsEMRGrades(b3)

## ----exampleBarrettsBasicNumbers, echo = TRUE,message=FALSE, warning=FALSE----
BarrettsBasicNumbers(b3,"Date.x")

## ----exampleBarrettsTherapeuticsRFA_ByCatheter,echo = TRUE,fig.width=7,fig.height=5,message=FALSE, warning=FALSE----
BarrettssRFACath(b3,"ProcedurePerformed","Findings")

## ----exampleBarrettsCRIM, echo = TRUE,message=FALSE, warning=FALSE-------
ds<-Barretts_CRIM(b3,'pHospitalNum',"EVENT")
ds2<-data.frame(ds$pHospitalNum,ds$ind)

## ----exampleBarrettsCRIM1, echo = FALSE,message=FALSE, warning=FALSE-----
panderOptions('table.split.table', Inf)
pander(head(ds2,10))

## ----exampleBarrettsQuality_AnalysisDocumentation, echo = TRUE,message=FALSE, warning=FALSE----
BarrettsDocumentQual(b3,"Findings")

## ----exampleBarrettsQuality_AnalysisBiopsyNumber,echo = TRUE,fig.width=7,fig.height=5,message=FALSE, warning=FALSE----
 # The number of average number of biopsies is then calculated and
 # compared to the average Prague C score so that those who are taking
 # too few biopsies can be determined
b4<-HistolNumbOfBx(b3,'Macroscopicdescription','specimen')
b4<-HistolBxSize(b4,'Macroscopicdescription')
BarrettsBxQual(b4,'Date.x','HospitalNumber',
                                      'Endoscopist')

## ----exampleBarrettsSurveillance_PathDetection,echo = TRUE,fig.width=7,fig.height=5,message=FALSE, warning=FALSE----
BarrettsPathDetectQual(b3,'Myplot')

## ----exampleBarrettsSurveillanceDDR,echo = TRUE,fig.width=7,fig.height=5,message=FALSE, warning=FALSE----
bDDR<-BarrettsDDRQual(b3,'Endoscopist','IMorNoIM')

## ----exampleBarrettsSurveillanceDDR2,echo = TRUE,echo=FALSE,message=FALSE, warning=FALSE----
panderOptions('table.split.table', Inf)
pander(head(bDDR,10))


## ----exampleBarrettsPatientTracking_Enrollment_Surveillance, echo = TRUE----
Enroll<-BarrettsSurveil(Myendo,'HospitalNumber','Dateofprocedure','Indications')
MyEnroll<-data.frame(Enroll["HospitalNumber"],Enroll["Years"])

## ----exampleBarrettsPatientTracking_Enrollment_Surveillance1, echo = FALSE----
pander(MyEnroll)


## ----exampleBarrettsPatientTracking_UniqueHospNum, echo = TRUE-----------
v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",Mypath,"Dateofprocedure","HospitalNumber")
b4<-BarrettsAll(v)
colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
Rule<-BarrettsSurveil_HospNum(b4,'Rule1','HospitalNumber')

## ----exampleBarrettsPatientTracking_UniqueHospNum1, echo = FALSE---------
pander(head(Rule,10))



