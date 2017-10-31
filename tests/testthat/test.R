#load(file="/home/rstudio/EndoMineR/data/PathDataFrameFinal.rda")
#load(file="/home/rstudio/EndoMineR/data/TheOGDReportFinal.rda")
##### CleanUp test functions ####
Mypath<-PathDataFrameFinal
Myendo<-TheOGDReportFinal
Myendo$OGDReportWhole<-gsub("2nd Endoscopist:","Second endoscopist:",Myendo$OGDReportWhole)
EndoscTree<-list("Hospital Number:","Patient Name:","General Practitioner:","Date of procedure:","Endoscopist:","Second endoscopist:","Medications","Instrument","Extent of Exam:","Indications:","Procedure Performed:","Findings:","Endoscopic Diagnosis:")
for(i in 1:(length(EndoscTree)-1)) {
  Myendo<-Extractor(Myendo,"OGDReportWhole",as.character(EndoscTree[i]),as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
}


Histoltree<-list("Hospital Number:","Patient Name:","General Practitioner:","Date of procedure:","Clinical Details","Nature of specimen","Histology","Diagnosis","")
for(i in 1:(length(Histoltree)-1)) {
  Mypath<-Extractor(Mypath,"PathReportWhole",as.character(Histoltree[i]),as.character(Histoltree[i+1]),gsub(" ","",as.character(Histoltree[i])))
}
Mypath$Dateofprocedure<-as.Date(Mypath$Dateofprocedure)
v<-Endomerge2(Mypath,"Dateofprocedure","HospitalNumber",Myendo,"Dateofprocedure","HospitalNumber")





#Make sure that the Endoscopist cleanup function has data in the Endoscopist column
test_that("Extractor", {
  expect_that(names(v),equals(c("OGDReportWhole","pHospitalNum","PatientName.x","GeneralPractitioner.x","Date.x",
                                "Endoscopist","Secondendoscopist","Medications","Instrument","ExtentofExam","Indications",
                                "ProcedurePerformed","Findings","PathReportWhole","eHospitalNum","PatientName.y",
                                "GeneralPractitioner.y","Date.y","ClinicalDetails","Natureofspecimen",
                                "Histology","Diagnosis","Days")))
  expect_that(all(!is.na(v$OGDReportWhole)),equals(TRUE))
  expect_that(all(!is.na(v$pHospitalNum)),equals(TRUE))
  expect_that(all(!is.na(v$PatientName.x)),equals(TRUE))
  expect_that(all(!is.na(v$GeneralPractitioner.x)),equals(TRUE))
  expect_that(all(!is.na(v$Date.x)),equals(TRUE))
  expect_that(all(!is.na(v$Endoscopist)),equals(TRUE))
  expect_that(all(!is.na(v$Secondendoscopist)),equals(TRUE))
  expect_that(all(!is.na(v$Medications)),equals(TRUE))
  expect_that(all(!is.na(v$Instrument)),equals(TRUE))
  expect_that(all(!is.na(v$ExtentofExam)),equals(TRUE))
  expect_that(all(!is.na(v$Indications)),equals(TRUE))
  expect_that(all(!is.na(v$GeneralPractitioner.y)),equals(TRUE))
  expect_that(all(!is.na(v$Date.y)),equals(TRUE))
  expect_that(all(!is.na(v$ClinicalDetails)),equals(TRUE))
  expect_that(all(!is.na(v$Natureofspecimen)),equals(TRUE))
  expect_that(all(!is.na(v$Histology)),equals(TRUE))
  expect_that(all(!is.na(v$Diagnosis)),equals(TRUE))
  expect_that(all(!is.na(v$Days)),equals(TRUE))
})

test_that("EndoscChopperEndoscopist", {
  Myendo<-EndoscChopperEndoscopist(Myendo,"Endoscopist")
  Myendo$Endoscopist<-gsub("2nd [Ee]ndoscopist","Second endoscopist",Myendo$Endoscopist)
  expect_that(all(!is.na(Myendo$Endoscopist)),equals(TRUE))
  
})

test_that("EndoscChopperMeds", {
  Myendo<-EndoscChopperMeds(Myendo,"Medications")
  expect_that(all(!is.na(Myendo$Medications)),equals(TRUE))
})

test_that("EndoscChopperInstrument", {
  Myendo<-EndoscChopperInstrument(Myendo,"Instrument")
  expect_that(all(!is.na(Myendo$Instrument)),equals(TRUE))
})

test_that("EndoscChopperIndications", {
  Myendo<-EndoscChopperIndications(Myendo,"Indications")
  expect_that(all(!is.na(Myendo$Indications)),equals(TRUE))
})

test_that("EndoscChopperProcPerformed", {
  Myendo<-EndoscChopperProcPerformed(Myendo,"ProcedurePerformed")
  expect_that(all(!is.na(Myendo$Findings)),equals(TRUE))
})

test_that("EndoscChopperFindings", {
  Myendo<-EndoscChopperFindings(Myendo,"Findings")
  expect_that(all(!is.na(Myendo$Findings)),equals(TRUE))
})

test_that("NegativeRemove", {
})

test_that("ColumnCleanUp", {
})

test_that("HistolChopperMacDescripCleanup", {
})

test_that("HistolChopperHistol", {
})



test_that("HistolChopperDx", {
  Mypath<-HistolChopperDx(Mypath,"Diagnosis")
  expect_that(all(!is.na(Mypath$Dx)),equals(TRUE))
})

test_that("HistolChopperExtrapolDx", {
  Mypath<-HistolChopperExtrapolDx(Mypath,"Diagnosis")
  expect_that(all(!is.na(Mypath$Dysplasia)),equals(FALSE))
})


test_that("HistolChopperNumbOfBx", {
  Mypath<-HistolChopperNumbOfBx(Mypath,"Natureofspecimen","specimen")
  expect_that(all(!is.na(Mypath$NumbOfBx)),equals(TRUE))
})

test_that("HistolChopperNumbOfBx", {
  #Mypath<-HistolChopperMacDescrip(Mypath, "Natureofspecimen")
  Mypath<-HistolChopperBxSize(Mypath,"Natureofspecimen")
  expect_that(all(!is.na(Mypath$BxSize)),equals(TRUE))
})

##### Barrett's test functions ####
##EndoMineR functions

test_that("SurveillanceTimeByRow", {
  em<-SurveillanceTimeByRow(Myendo,"HospitalNumber","Dateofprocedure")
  expect_that(nrow(em)>0,equals(TRUE))
})

test_that("SurveillanceLastToNow", {
  em<-SurveillanceLastToNow(Myendo,"HospitalNumber","Dateofprocedure")
  expect_that(nrow(em)>0,equals(TRUE))
})

test_that("SurveillanceLastTest", {
  em<-SurveillanceLastTest(Myendo,"HospitalNumber","Dateofprocedure")
  expect_that(nrow(em)>0,equals(TRUE))
})

test_that("SurveillanceFirstTest", {
  em<-SurveillanceFirstTest(Mypath,"HospitalNumber","Dateofprocedure")
  expect_that(nrow(em)>0,equals(TRUE))
})

test_that("SurveillanceTimeByRow", {
  em<-SurveillanceTimeByRow(Myendo,"HospitalNumber","Dateofprocedure")
  expect_that(nrow(em)>0,equals(TRUE))
})

test_that("SurveillanceCapacity", {
  em<-SurveillanceCapacity(Myendo,"Dateofprocedure")
})


test_that("HowManyTests", {
  Tests<-HowManyTests(Myendo,"Indications","Dateofprocedure","S.*")
  expect_that(class(Tests[1])=="list",equals(TRUE))
  expect_that(class(Tests[2])=="list",equals(TRUE))
})

test_that("MetricByEndoscopist", {
  myNotableWords<-c("arrett","oeliac")
  tt<-ListLookup(Myendo,"Findings",myNotableWords)
  expect_that(class(tt)=="data.frame",equals(TRUE))
})



test_that("MetricByEndoscopist", {
  Myendo<-EndoscChopperMeds(Myendo,"Medications")
  Fent<-MetricByEndoscopist(Myendo,"Endoscopist","Fent")
  expect_that(class(Fent[1])=="list",equals(TRUE))
  expect_that(class(Fent[2])=="list",equals(TRUE))
})




