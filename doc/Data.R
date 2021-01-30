## ----setup, include=FALSE-----------------------------------------------------
library(pander)
library(EndoMineR)
knitr::opts_chunk$set(echo = TRUE)

## ----global_options, include=FALSE--------------------------------------------
knitr::opts_chunk$set( warning=FALSE, message=FALSE)

## ----MyendoExtract, eval = FALSE----------------------------------------------
#  mywords <- c("OGDReportWhole","HospitalNumber","PatientName",
#               "GeneralPractitioner","Dateofprocedure","Endoscopist",
#               "Secondendoscopist","Medications","Instrument","ExtentofExam",
#               "Indications","ProcedurePerformed","Findings")
#  Extractor(TheOGDReportFinal,"OGDReportWhole",mywords)

## ----MypathExtract, eval = FALSE----------------------------------------------
#  mywords<-c("HospitalNumber","PatientName","DOB","GeneralPractitioner",
#             "Dateofprocedure","ClinicalDetails","Macroscopicdescription",
#             "Histology","Diagnosis")
#  Extractor(PathDataFrameFinal,"PathReportWhole",mywords)

