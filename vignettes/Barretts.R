## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,message=FALSE, warning=FALSE)
library(knitr)
library(EndoMineR)
library(pander)
library(prettydoc)

## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)

## ----exampleBarretts_PragueScore,echo = TRUE,message=FALSE, warning=FALSE----
v<-Barretts_PragueScore(Myendo,'Findings')
panderOptions('table.split.table', Inf)
pander(v[23:27,(ncol(v)-4):ncol(v)])

## ----exampleBarretts_PathStage, echo = TRUE,message=FALSE, warning=FALSE----
b<-HistolAll(Mypath)
# The histology is then merged with the Endoscopy dataset. The merge occurs
# according to date and Hospital number
b<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',b,'Dateofprocedure',
'HospitalNumber')
b<-Barretts_PathStage(b,'Histology')
panderOptions('table.split.table', Inf)
pander(b[20:23,(ncol(b)-4):ncol(b)])

## ----exampleBarretts_EventType, echo = TRUE,message=FALSE, warning=FALSE----
v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",Mypath,"Dateofprocedure","HospitalNumber")
b<-Barretts_EventType(v,'Histology', 'ProcedurePerformed','OGDReportWhole','Findings')
pander(b[23:27,(ncol(b)-4):ncol(b)])

## ----exampleBarretts_FUType, echo = TRUE,message=FALSE, warning=FALSE----
v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",Mypath,"Dateofprocedure","HospitalNumber")
b<-Barretts_PathStage(v,'Histology')
b2<-Barretts_EventType(b,'Histology','ProcedurePerformed','OGDReportWhole','Findings')
b3<-Barretts_FUType(b2,'Findings')
panderOptions('table.split.table', Inf)
pander(b3[23:27,(ncol(b3)-4):ncol(b3)])

## ----exampleBarrettsAll, echo = TRUE,message=FALSE, warning=FALSE--------
 # Firstly relevant columns are extrapolated from the
 # Mypath demo dataset. These functions are all part of Histology data
 # cleaning as part of the package.
 mywords<-c("Hospital Number","Patient Name:","DOB:","General Practitioner:",
 "Date received:","Clinical Details:","Macroscopic description:",
 "Histology:","Diagnosis:")
 Mypath<-Extractor(Mypath,"PathReportWhole",mywords)
 names(Mypath)<-c("Original","HospitalNumber","PatientName","DOB",
 "GeneralPractitioner","Datereceived","ClinicalDetails",
 "Macroscopicdescription","Histology","Diagnosis","HospitalNumber",
 "PatientName","DOB","GeneralPractitioner","Dateofprocedure",
 "ClinicalDetails","Macroscopicdescription","Histology","Diagnosis")
 v<-HistolAll(Mypath)
 rm(Mypath)
 
  # Rename the columns in whatever endoscopy dataframe you have
 names(Myendo)<-c("OGDReportWhole","HospitalNumber","PatientName",
 "GeneralPractitioner","Dateofprocedure","Endoscopist","Secondendoscopist",
 "Medications","Instrument","ExtentofExam","Indications","ProcedurePerformed",
 "Findings" )
 #Now use the function
 Myendo<-EndoscAll(Myendo)
 
 # The histology is then merged with the Endoscopy dataset. The merge occurs
 # according to date and Hospital number
 v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
 'HospitalNumber')
 
 
 #The function relies on the other Barrett's functions being run as well:
 b3<-BarrettsAll(v)
 panderOptions('table.split.table', Inf)
 pander(b3[23:27,(ncol(b3)-4):ncol(b3)])
 

## ----exampleBarretts_LesionRecognitionEMR, eval = FALSE,message=FALSE, warning=FALSE----
#  #As long as the code int he chunk above has been run then all that needs to be done is:
#  BarrettsEMRGrades(b3)

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

BarrettsDDRQual(b3,'Endoscopist','IMorNoIM')

## ----exampleBarrettsPatientTracking_Enrollment_Surveillance, echo = TRUE----
Enroll<-BarrettsSurveil(Myendo,'HospitalNumber','Dateofprocedure','Indications')
MyEnroll<-data.frame(Enroll["HospitalNumber"],Enroll["Years"])
pander(MyEnroll)

## ----exampleBarrettsPatientTracking_UniqueHospNum, echo = TRUE-----------
v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",Mypath,"Dateofprocedure","HospitalNumber")
b4<-BarrettsAll(v)
colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
Rule<-BarrettsSurveil_HospNum(b4,'Rule1','HospitalNumber')
pander(head(Rule,10))

