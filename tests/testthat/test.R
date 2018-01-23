#load(file="/home/rstudio/EndoMineR/data/PathDataFrameFinal.rda")
#load(file="/home/rstudio/EndoMineR/data/TheOGDReportFinal.rda")

#### The data for the tests ####

# For the upper GI
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


mywords<-c("Hospital Number","Patient Name","DOB:","General Practitioner:",
           "Date of procedure:","Clinical Details:",
           "Macroscopic description:",
           "Histology:","Diagnosis:")
           Extractor2(Mypath,PathReportWhole,mywords)
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







#For the colon data
MypathColon<-PathDataFrameFinalColon
MyendoColon <- ColonFinal
MyendoColon$OGDReportWhole <-
  gsub("2nd Endoscopist:",
       "Second endoscopist:",
       MyendoColon$OGDReportWhole)
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
  MyendoColon <-
    Extractor(
      MyendoColon,
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
  MypathColon <-
    Extractor(
      MypathColon,
      "PathReportWhole",
      as.character(Histoltree[i]),
      as.character(Histoltree[i + 1]),
      gsub(" ", "", as.character(Histoltree[i]))
    )
}
names(MypathColon)[names(MypathColon) == 'Datereceived'] <- 'Dateofprocedure'
MypathColon$Dateofprocedure <- as.Date(MypathColon$Dateofprocedure)

vColon <-
  Endomerge2(
    MypathColon,
    "Dateofprocedure",
    "HospitalNumber",
    MyendoColon,
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

#### ChopperNewLines test ####
test_that("ChopperNewLines", {
ChopperNewLines(Myendo,'OGDReportWhole')
})

#### NegativeRemove test ####

test_that("NegativeRemove", {
  
  anexample<-c("There is no evidence of polyp here",
               "Although the prep was poor,there was no adenoma found",
              "The colon was basically inflammed, but no polyp was seen",
               "The Barrett's segment was not biopsied",
               "The C0M7 stretch of Barrett's was flat")
               anexample<-data.frame(anexample)
               names(anexample)<-"Thecol"
               res<-NegativeRemove(anexample,"Thecol")
               
               res2<-res[grep("Although the prep was poor.",res$Thecol),]

               expect_true( length(res2)>0)
     
})

#### ColumnCleanUp test ####

test_that("ColumnCleanUp", {
  pp<-c("The rain in spain falls mainly",".\n",":What")
  me<-ColumnCleanUp(pp)
  
  me2<-me[grep("What",me)]
  
  expect_true( length(me2)>0)
})

#### HistolChopperMacDescripCleanup test ####

test_that("HistolChopperMacDescripCleanup", {
  ff<-"The report was Dictated by Dr John Di john"
  ff<-data.frame(ff)
  names(ff)<-"Thecol"

  HistolChopperMacDescripCleanup(ff,"Thecol")
})

#### HistolChopperHistol test ####

test_that("HistolChopperHistol", {
  HistolChopperHistol(Mypath,'Histology')
  expect_true(all(!is.na(Mypath$Histol_Simplified)))
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
names(Myendo)[names(Myendo) == 'HospitalNumber'] <- 'PatientID'
SurveySankey(Myendo,"ProcedurePerformed","PatientID")
})

#### PatientFlow_CircosPlots test ####

test_that("PatientFlow_CircosPlots", {
  Event <- list(x1 = "Therapeutic- Dilatation",
                x2 = "Other-", x3 = "Surveillance",
                x4 = "APC", x5 = "Therapeutic- RFA TTS",
                x5 = "Therapeutic- RFA 90",
                x6 = "Therapeutic- EMR", x7 = "Therapeutic- RFA 360")
  EndoEvent<-replicate(2000,sample(Event,1, replace = F))
  fff<-unlist(EndoEvent)
  fff<-data.frame(fff)
  names(fff)<-"col1"
  Myendo<-cbind(fff$col1,Myendo)
  names(Myendo)[names(Myendo) == 'fff$col1'] <- 'EndoEvent'
  Myendo$EndoEvent<-unlist(Myendo$EndoEvent)
  Myendo$EndoEvent<-as.character(Myendo$EndoEvent)
  names(Myendo)[names(Myendo) == 'HospitalNumber'] <- 'PatientID'
  PatientFlow_CircosPlots(Myendo,"Dateofprocedure","PatientID","EndoEvent")
})

#### ListLookup test ####

test_that("ListLookup", {
  myNotableWords<-c('arrett','oeliac')
  tt<-ListLookup(Myendo,'Findings',myNotableWords)
  expect_true(nrow(tt)>0)
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
  Histoltree <-list("Hospital Number:","Patient Name:",
 "General Practitioner:","Date received:","Clinical Details",
  "Nature of specimen","Histology","Diagnosis",""
)

 for (i in 1:(length(Histoltree) - 1)) {
   PathDataFrameFinalColon <-Extractor(
    PathDataFrameFinalColon, "PathReportWhole",as.character(Histoltree[i]),
      as.character(Histoltree[i + 1]),
      gsub(" ", "", as.character(Histoltree[i]))
    )
}
names(PathDataFrameFinalColon)[names(PathDataFrameFinalColon) == 
                                 'Datereceived'] <- 'Dateofprocedure'
Mypath$Dateofprocedure <- as.Date(Mypath$Dateofprocedure)
f<-TermStandardLocation(PathDataFrameFinalColon,'Histology')
f<-PolypLocator(f,'SampleLocation')
  
})

#### SampleLocator test ####

test_that("SampleLocator", {
  
  Histoltree <-list("Hospital Number:","Patient Name:",
               "General Practitioner:","Date received:","Clinical Details",
                "Nature of specimen","Histology","Diagnosis",""
  )
  
  for (i in 1:(length(Histoltree) - 1)) {
    PathDataFrameFinalColon <-Extractor(
      PathDataFrameFinalColon, "PathReportWhole",as.character(Histoltree[i]),
      as.character(Histoltree[i + 1]),
      gsub(" ", "", as.character(Histoltree[i]))
    )
  }
  names(PathDataFrameFinalColon)[names(PathDataFrameFinalColon) ==
                                   'Datereceived'] <- 'Dateofprocedure'
  Mypath$Dateofprocedure <- as.Date(PathDataFrameFinalColon$Dateofprocedure)
  f<-TermStandardLocation(PathDataFrameFinalColon,'Histology')
  f<-SampleLocator(f,'SampleLocation')
  
})

#### PolypLocator test ####

test_that("PolypLocator", {
  Histoltree <-list("Hospital Number:","Patient Name:",
          "General Practitioner:","Date received:","Clinical Details",
                    "Nature of specimen","Histology","Diagnosis",""
  )
  
  for (i in 1:(length(Histoltree) - 1)) {
    PathDataFrameFinalColon <-Extractor(
      PathDataFrameFinalColon, "PathReportWhole",as.character(Histoltree[i]),
      as.character(Histoltree[i + 1]),
      gsub(" ", "", as.character(Histoltree[i]))
    )
  }
  names(PathDataFrameFinalColon)[names(PathDataFrameFinalColon) ==
                                   'Datereceived'] <- 'Dateofprocedure'
  Mypath$Dateofprocedure <- as.Date(PathDataFrameFinalColon$Dateofprocedure)
  f<-TermStandardLocation(PathDataFrameFinalColon,'Histology')
  f<-PolypLocator(f,'SampleLocation')
  
})

#### PolypTidyUpLocator test ####

test_that("PolypTidyUpLocator", {
  Histoltree <-list("Hospital Number:","Patient Name:",
              "General Practitioner:","Date received:","Clinical Details",
                    "Nature of specimen","Histology","Diagnosis",""
  )
  
  for (i in 1:(length(Histoltree) - 1)) {
    PathDataFrameFinalColon <-Extractor(
      PathDataFrameFinalColon, "PathReportWhole",as.character(Histoltree[i]),
      as.character(Histoltree[i + 1]),
      gsub(" ", "", as.character(Histoltree[i]))
    )
  }
  names(PathDataFrameFinalColon)[names(PathDataFrameFinalColon) ==
                                   'Datereceived'] <- 'Dateofprocedure'
  Mypath$Dateofprocedure <- as.Date(PathDataFrameFinalColon$Dateofprocedure)
  f<-TermStandardLocation(PathDataFrameFinalColon,'Histology')
  f<-PolypTidyUpLocator(f,'SampleLocation')
})

#### GRS_Type_Assess_By_Unit test ####

test_that("GRS_Type_Assess_By_Unit", {
 #  
 
 vColon2<-HistolChopperDx(vColon,'Diagnosis')
 vColon2<-HistolChopperExtrapolDx(vColon2,'Diagnosis')
 vColon2<-HistolChopperNumbOfBx(vColon2,'Natureofspecimen','specimen')
 vColon2<-HistolChopperBxSize(vColon2,'Natureofspecimen')
 GRSTable<-GRS_Type_Assess_By_Unit(vColon2,'ProcedurePerformed', 
 'Endoscopist','Diagnosis','Histology')
  
})

#### NumberPerformed test ####

test_that("NumberPerformed", {
  Myendo<-Myendo[grepl('Gastroscopy',Myendo$ProcedurePerformed),]
  NumberPerformed(Myendo,'Endoscopist','Indications')
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
  
Enroll<-BarrettsPatientTracking_Enrollment_Surveillance(Myendo,
                     'HospitalNumber','Dateofprocedure','Indications')
  
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
                               'ProcedurePerformed','OGDReportWhole',
                               'Findings')
  b4<-BarrettsDataAccord_FUGroup(b3,'Findings')
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsSurveillanceDDR(b4,'Endoscopist','IMorNoIM')
})



#### BarrettsTherapy_Numbers_EMRsByGrade ####

test_that("BarrettsTherapy_Numbers_EMRsByGrade", {
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
  BarrettsTherapy_Numbers_EMRsByGrade(b4)
})


#### BarrettsBasicNumbers ####

test_that("BarrettsBasicNumbers", {
  v<-HistolChopperDx(Mypath,'Diagnosis')
  v<-HistolChopperExtrapolDx(v,'Diagnosis')
  v<-HistolChopperNumbOfBx(v,'Natureofspecimen','specimen')
  v<-HistolChopperBxSize(v,'Natureofspecimen')
  v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,
                "Dateofprocedure","HospitalNumber")
  b1<-BarrettsDataAccord_Prague(v,'Findings')
  b2<-BarrettsDataAccord_PathStage(b1,'Histology')
  b3<-BarrettsDataAccord_Event(b2,'Histology',
                         'ProcedurePerformed','OGDReportWhole','Findings')
  b4<-BarrettsDataAccord_FUGroup(b3,'Findings')
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsBasicNumbers(b4,"Date.x")
})


#### BarrettsTherapeuticsRFA_ByCatheter ####

test_that("BarrettsTherapeuticsRFA_ByCatheter", {
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
  BarrettsTherapeuticsRFA_ByCatheter(b4,"ProcedurePerformed","Findings")
})


#### Barretts_LesionRecognitionEMR ####

test_that("Barretts_LesionRecognitionEMR", {
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
  Barretts_LesionRecognitionEMR(b4,"ProcedurePerformed","Findings")
})


#### Barretts_CRIM ####
test_that("Barretts_CRIM", {
  v<-HistolChopperDx(v,'Diagnosis')
  v<-HistolChopperExtrapolDx(v,'Diagnosis')
  v<-HistolChopperNumbOfBx(v,'Natureofspecimen','specimen')
  v<-HistolChopperBxSize(v,'Natureofspecimen')
  b1<-BarrettsDataAccord_Prague(v,'Findings')
  b2<-BarrettsDataAccord_PathStage(b1,'Histology')
  b3<-BarrettsDataAccord_Event(b2,'Histology',
                     'ProcedurePerformed','OGDReportWhole','Findings')
  colnames(b3)[colnames(b3) == 'pHospitalNum'] <- 'HospitalNumber'
  Barretts_CRIM(b3,'HospitalNumber',"EVENT")
})
