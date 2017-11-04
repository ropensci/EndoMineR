#load(file="/home/rstudio/EndoMineR/data/PathDataFrameFinal.rda")
#load(file="/home/rstudio/EndoMineR/data/TheOGDReportFinal.rda")

#### The data for the tests ####
Mypath <- PathDataFrameFinal
Myendo <- TheOGDReportFinal
Myendo$OGDReportWhole <-
  gsub("2nd Endoscopist:",
       "Second endoscopist:",
       Myendo$OGDReportWhole)
EndoscTree <-
  list(
    "Hospital Number:",
    "Patient Name:",
    "General Practitioner:",
    "Date of procedure:",
    "Endoscopist:",
    "Second endoscopist:",
    "Medications",
    "Instrument",
    "Extent of Exam:",
    "Indications:",
    "Procedure Performed:",
    "Findings:",
    "Endoscopic Diagnosis:"
  )
for (i in 1:(length(EndoscTree) - 1)) {
  Myendo <-
    Extractor(
      Myendo,
      "OGDReportWhole",
      as.character(EndoscTree[i]),
      as.character(EndoscTree[i + 1]),
      as.character(EndoscTree[i])
    )
}


Histoltree <-
  list(
    "Hospital Number:",
    "Patient Name:",
    "General Practitioner:",
    "Date received:",
    "Clinical Details",
    "Nature of specimen",
    "Histology",
    "Diagnosis",
    ""
  )
for (i in 1:(length(Histoltree) - 1)) {
  Mypath <-
    Extractor(
      Mypath,
      "PathReportWhole",
      as.character(Histoltree[i]),
      as.character(Histoltree[i + 1]),
      gsub(" ", "", as.character(Histoltree[i]))
    )
}
names(Mypath)[names(Mypath) == 'Datereceived'] <- 'Dateofprocedure'
Mypath$Dateofprocedure <- as.Date(Mypath$Dateofprocedure)
v <-
  Endomerge2(
    Mypath,
    "Dateofprocedure",
    "HospitalNumber",
    Myendo,
    "Dateofprocedure",
    "HospitalNumber"
  )


##### rrrrrrrrrrrrrrrrrrrCleanUp test functions ####

#Make sure that the Endoscopist cleanup function has data in the Endoscopist 
#column
#### Extractor test ####
test_that("Extractor", {
  expect_that(names(v), equals(
    c(
      "OGDReportWhole",
      "pHospitalNum",
      "PatientName.x",
      "GeneralPractitioner.x",
      "Date.x",
      "Endoscopist",
      "Secondendoscopist",
      "Medications",
      "Instrument",
      "ExtentofExam",
      "Indications",
      "ProcedurePerformed",
      "Findings",
      "PathReportWhole",
      "eHospitalNum",
      "PatientName.y",
      "GeneralPractitioner.y",
      "Date.y",
      "ClinicalDetails",
      "Natureofspecimen",
      "Histology",
      "Diagnosis",
      "Days"
    )
  ))
  expect_true(all(!is.na(v$OGDReportWhole)))
  expect_true(all(!is.na(v$pHospitalNum)))
  expect_true(all(!is.na(v$GeneralPractitioner.x)))
  expect_true(all(!is.na(v$Date.x)))
  expect_true(all(!is.na(v$Endoscopist)))
  expect_true(all(!is.na(v$Secondendoscopist)))
  expect_true(all(!is.na(v$Medications)))
  expect_true(all(!is.na(v$Instrument)))
  expect_true(all(!is.na(v$ExtentofExam)))
  expect_true(all(!is.na(v$Indications)))
  expect_true(all(!is.na(v$GeneralPractitioner.y)))
  expect_true(all(!is.na(v$Date.y)))
  expect_true(all(!is.na(v$ClinicalDetails)))
  expect_true(all(!is.na(v$Natureofspecimen)))
  expect_true(all(!is.na(v$Histology)))
  expect_true(all(!is.na(v$Diagnosis)))
  expect_true(all(!is.na(v$Days)))
})

#### EndoscChopperEndoscopist test ####

test_that("EndoscChopperEndoscopist", {
  Myendo <- EndoscChopperEndoscopist(Myendo, "Endoscopist")
  Myendo$Endoscopist <-
    gsub("2nd [Ee]ndoscopist",
         "Second endoscopist",
         Myendo$Endoscopist)
  expect_true(all(!is.na(Myendo$Endoscopist)))
  
})

#### EndoscChopperMeds test ####

test_that("EndoscChopperMeds", {
  Myendo <- EndoscChopperMeds(Myendo, "Medications")
  expect_true(all(!is.na(Myendo$Medications)))
})

#### EndoscChopperInstrument test ####

test_that("EndoscChopperInstrument", {
  Myendo <- EndoscChopperInstrument(Myendo, "Instrument")
  expect_true(all(!is.na(Myendo$Instrument)))
})

#### EndoscChopperIndications test ####

test_that("EndoscChopperIndications", {
  Myendo <- EndoscChopperIndications(Myendo, "Indications")
  expect_true(all(!is.na(Myendo$Indications)))
})

#### EndoscChopperProcPerformed test ####

test_that("EndoscChopperProcPerformed", {
  Myendo <- EndoscChopperProcPerformed(Myendo, "ProcedurePerformed")
  expect_true(all(!is.na(Myendo$Findings)))
})

#### EndoscChopperFindings test ####

test_that("EndoscChopperFindings", {
  Myendo <- EndoscChopperFindings(Myendo, "Findings")
  expect_true(all(!is.na(Myendo$Findings)))
})

#### NegativeRemove test ####

test_that("NegativeRemove", {
  
})

#### ColumnCleanUp test ####

test_that("ColumnCleanUp", {
  
})

#### HistolChopperMacDescripCleanup test ####

test_that("HistolChopperMacDescripCleanup", {
  
})

#### HistolChopperHistol test ####

test_that("HistolChopperHistol", {
  
})


#### HistolChopperDx test ####

test_that("HistolChopperDx", {
  Mypath <- HistolChopperDx(Mypath, "Diagnosis")
  expect_true(all(!is.na(Mypath$Dx)))
})

#### HistolChopperExtrapolDx test ####

test_that("HistolChopperExtrapolDx", {
  Mypath <- HistolChopperExtrapolDx(Mypath, "Diagnosis")
  expect_false(all(!is.na(Mypath$Dysplasia)))
})

#### HistolChopperAccessionNumber test ####

test_that("HistolChopperAccessionNumber", {
  Mypath <- HistolChopperAccessionNumber(Mypath,'PathReportWhole',
                                         'SP-\\d{2}-\\d{7}')
  expect_true(all(!is.na(Mypath$AccessionNumber)))
})

#### HistolChopperMacDescrip test ####

test_that("HistolChopperMacDescrip", {
  #Mypath<-HistolChopperMacDescrip(Mypath, "Natureofspecimen")
  Mypath <- HistolChopperMacDescrip(Mypath, "Natureofspecimen")
  expect_true(all(!is.na(Mypath$BxSize)))
})

#### HistolChopperNumbOfBx test ####

test_that("HistolChopperNumbOfBx", {
  Mypath <- HistolChopperNumbOfBx(Mypath, "Natureofspecimen", "specimen")
  expect_true(all(!is.na(Mypath$NumbOfBx)))
})

#### HistolChopperBxSize test ####

test_that("HistolChopperBxSize", {
  Mypath <- HistolChopperBxSize(Mypath,'Natureofspecimen')
  expect_true(all(!is.na(Mypath$BxSize)))
})


##### rrrrrrrrrrrrrrrrrrrEndoMineR functions #####

#### SurveillanceTimeByRow test ####

test_that("SurveillanceTimeByRow", {
  em <- SurveillanceTimeByRow(Myendo, "HospitalNumber", "Dateofprocedure")
  expect_true(nrow(em) > 0)
})

#### SurveillanceLastToNow test ####

test_that("SurveillanceLastToNow", {
  em <- SurveillanceLastToNow(Myendo, "HospitalNumber", "Dateofprocedure")
  expect_true(nrow(em) > 0)
})

#### SurveillanceLastTest test ####

test_that("SurveillanceLastTest", {
  em <- SurveillanceLastTest(Myendo, "HospitalNumber", "Dateofprocedure")
  expect_true(nrow(em) > 0)
})

#### SurveillanceFirstTest test ####

test_that("SurveillanceFirstTest", {
  em <- SurveillanceFirstTest(Mypath, "HospitalNumber", "Dateofprocedure")
  expect_true(nrow(em) > 0)
})

#### SurveillanceTimeByRow test ####

test_that("SurveillanceTimeByRow", {
  em <- SurveillanceTimeByRow(Myendo, "HospitalNumber", "Dateofprocedure")
  expect_true(nrow(em) > 0)
})

#### SurveillanceCapacity test ####

test_that("SurveillanceCapacity", {
  em <- SurveillanceCapacity(Myendo, "Dateofprocedure")
})

#### HowManyTests test ####

test_that("HowManyTests", {
  Tests <- HowManyTests(Myendo, "Indications", "Dateofprocedure", "S.*")
  expect_true(class(Tests[1]) == "list")
  expect_true(class(Tests[2]) == "list")
})

#### SurveySankey test ####

test_that("SurveySankey", {
  
})

#### PatientFlow_CircosPlots test ####

test_that("PatientFlow_CircosPlots", {
  
})

#### ListLookup test ####

test_that("ListLookup", {
  
})

#### MetricByEndoscopist test ####

test_that("MetricByEndoscopist", {
  myNotableWords <- c("arrett", "oeliac")
  tt <- ListLookup(Myendo, "Findings", myNotableWords)
  expect_true(class(tt) == "data.frame")
})

#### MetricByEndoscopist test ####

test_that("MetricByEndoscopist", {
  Myendo <- EndoscChopperMeds(Myendo, "Medications")
  Fent <- MetricByEndoscopist(Myendo, "Endoscopist", "Fent")
  expect_true(class(Fent[1]) == "list")
  expect_true(class(Fent[2]) == "list")
})

#### TermStandardLocation test ####

test_that("TermStandardLocation", {
  
})

#### SampleLocator test ####

test_that("SampleLocator", {
  
})

#### PolypLocator test ####

test_that("PolypLocator", {
  
})

#### PolypTidyUpLocator test ####

test_that("PolypTidyUpLocator", {
  
})

#### GRS_Type_Assess_By_Unit test ####

test_that("GRS_Type_Assess_By_Unit", {
  
})

#### NumberPerformed test ####

test_that("NumberPerformed", {
  
})

#### TermStandardLocation test ####

test_that("TermStandardLocation", {
  
})

#### rrrrrrrrrrrrrrrrrrrrrrr Barretts Functions Test ####

#### BarrettsDataAccord_Prague ####

test_that("BarrettsDataAccord_Prague", {
  
  v<-BarrettsDataAccord_Prague(Myendo,'Findings')
  expect_true(nrow(v) > 0)
  
})


#### BarrettsDataAccord_PathStage ####

test_that("BarrettsDataAccord_PathStage", {
  b<-BarrettsDataAccord_PathStage(v,'Histology')
  
})


#### BarrettsDataAccord_Event ####

test_that("BarrettsDataAccord_Event", {

b<-BarrettsDataAccord_Event(v,'Histology','ProcedurePerformed','OGDReportWhole'
                            ,'Findings')
})


#### BarrettsDataAccord_FUGroup ####

test_that("BarrettsDataAccord_FUGroup", {
  b<-BarrettsDataAccord_PathStage(v,'Histology')
  b2<-BarrettsDataAccord_Event(b,'Histology',
  'ProcedurePerformed','OGDReportWhole','Findings')
  b3<-BarrettsDataAccord_FUGroup(b2,'Findings')
  
})

#### BarrettsPatientTracking_Enrollment_Surveillance ####

test_that("BarrettsPatientTracking_Enrollment_Surveillance", {
  
Enroll<-BarrettsPatientTracking_Enrollment_Surveillance(Myendo,'HospitalNumber',
                                              'Dateofprocedure','Indications')
  
})


#### BarrettsPatientTracking_UniqueHospNum ####

test_that("BarrettsPatientTracking_UniqueHospNum", {
  
  b1<-BarrettsDataAccord_Prague(v,'Findings')
  b2<-BarrettsDataAccord_PathStage(b1,'Histology')
  b3<-BarrettsDataAccord_Event(b2,'Histology',
  'ProcedurePerformed','OGDReportWhole','Findings')
  b4<-BarrettsDataAccord_FUGroup(b3,'Findings')
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  Rule<-BarrettsPatientTracking_UniqueHospNum(b4,'Rule1','HospitalNumber')
  
})


#### BarrettsQuality_AnalysisDocumentation ####

test_that("BarrettsQuality_AnalysisDocumentation", {
  b<-BarrettsDataAccord_PathStage(v,'Histology')
  BarrettsQuality_AnalysisDocumentation(b,'Findings')
})


#### BarrettsQuality_AnalysisBiopsyNumber ####

test_that("BarrettsQuality_AnalysisBiopsyNumber", {
  v<-HistolChopperExtrapolDx(v,'Diagnosis')
  v<-HistolChopperNumbOfBx(v,'Natureofspecimen','specimen')
  v<-HistolChopperBxSize(v,'Natureofspecimen')
  b1<-BarrettsDataAccord_Prague(v,'Findings')
  b2<-BarrettsDataAccord_PathStage(b1,'Histology')
  b3<-BarrettsDataAccord_Event(b2,'Histology','ProcedurePerformed',
                               'OGDReportWhole','Findings')
  b4<-BarrettsDataAccord_FUGroup(b3,'Findings')
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsQuality_AnalysisBiopsyNumber(b4,'Date.x','HospitalNumber',
                                       'Endoscopist')
  
})

#### BarrettsSurveillance_PathDetection ####

test_that("BarrettsSurveillance_PathDetection", {
  v<-HistolChopperDx(v,'Diagnosis')
  v<-HistolChopperExtrapolDx(v,'Diagnosis')
  v<-HistolChopperNumbOfBx(v,'Natureofspecimen','specimen')
  v<-HistolChopperBxSize(v,'Natureofspecimen')
  b1<-BarrettsDataAccord_Prague(v,'Findings')
  b2<-BarrettsDataAccord_PathStage(b1,'Histology')
  b3<-BarrettsDataAccord_Event(b2,'Histology',
  'ProcedurePerformed','OGDReportWhole','Findings')
  b4<-BarrettsDataAccord_FUGroup(b3,'Findings')
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsSurveillance_PathDetection(b4,'Myplot')
})


#### BarrettsSurveillanceDDR ####

test_that("BarrettsSurveillanceDDR", {
  v<-HistolChopperDx(v,'Diagnosis')
  v<-HistolChopperExtrapolDx(v,'Diagnosis')
  v<-HistolChopperNumbOfBx(v,'Natureofspecimen','specimen')
  v<-HistolChopperBxSize(v,'Natureofspecimen')
  b1<-BarrettsDataAccord_Prague(v,'Findings')
  b2<-BarrettsDataAccord_PathStage(b1,'Histology')
  b3<-BarrettsDataAccord_Event(b2,'Histology',
                               'ProcedurePerformed','OGDReportWhole','Findings')
  b4<-BarrettsDataAccord_FUGroup(b3,'Findings')
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsSurveillanceDDR(b4,'Endoscopist','IMorNoIM')
})


#### BarrettsTherapeuticsOnly ####

test_that("BarrettsTherapeuticsOnly", {
  
})


#### BarrettsTherapy_Numbers_EMRsByGrade ####

test_that("BarrettsTherapy_Numbers_EMRsByGrade", {
  
})


#### BarrettsBasicNumbers ####

test_that("BarrettsBasicNumbers", {
  
})


#### BarrettsTherapeuticsRFA_ByCatheter ####

test_that("BarrettsTherapeuticsRFA_ByCatheter", {
  
})


#### Barretts_LesionRecognitionEMR ####

test_that("Barretts_LesionRecognitionEMR", {
  
})


#### Barretts_CRIM ####
test_that("Barretts_CRIM", {
  
})
