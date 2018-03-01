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
mywords<-c("Hospital Number:",
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
  Myendo <-Extractor(Myendo,"OGDReportWhole",mywords)

mywords<-c("Hospital Number","Patient Name","DOB:","General Practitioner:",
           "Date of procedure:","Clinical Details:",
           "Macroscopic description:",
           "Histology:","Diagnosis:")
rm(Mypath)
           Extractor(Mypath,"PathReportWhole",mywords)
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
  c(
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

MyendoColon <-Extractor(MyendoColon,"OGDReportWhole",EndoscTree)



mywords<-c("Hospital Number","Patient Name","DOB:","General Practitioner:",
           "Date received:","Clinical Details:",
           "Macroscopic description:",
           "Histology:","Diagnosis:")

MypathColon <-Extractor(MypathColon,"PathReportWhole",mywords)
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
    c("Original",
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
      "EndoscopicDiagnosis",
      "PathReportWhole",
      "eHospitalNum",
      "PatientName.y",
      "DOB",
      "GeneralPractitioner.y",
      "Date.y",
      "ClinicalDetails",
      "Macroscopicdescription",
      "Histology",
      "Diagnosis",
      "Days"
    )
  ))
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

#### EndoscEndoscopist test ####

test_that("EndoscEndoscopist", {
  MyEndoscEndoscopistTest<-data.frame(c("Dr Jeremiah Stubborn"))
  names(MyEndoscEndoscopistTest)<-"Endoscopist"
  
  MyEndoscEndoscopistTest <- EndoscEndoscopist(MyEndoscEndoscopistTest, "Endoscopist")
  MyEndoscEndoscopistTest$Endoscopist <-
  expect_true(all(!is.na(Myendo$Endoscopist)))
  Result<-as.character(Myendo$Endoscopist)
  expect_identical(Result,"Jeremiah Stubborn")
})

#### EndoscMeds test ####

test_that("EndoscMeds", {
  MyEndoscMedsTest<-data.frame(c("Fentanyl 125mcg, Midazolam 5mg"))
  names(MyEndoscMedsTest)<-"Medications"
  MyendoMeds <- EndoscMeds(MyEndoscMedsTest, "Medications")
  expect_true(all(!is.na(MyendoMeds$Medications)))
  expect_true(all(!is.na(MyendoMeds$Fent)))
  expect_true(all(!is.na(MyendoMeds$Midaz)))
  expect_identical(MyendoMeds$Fent,125)
  expect_identical(MyendoMeds$Midaz,5)
})

#### EndoscInstrument test ####

test_that("EndoscInstrument", {
  MyEndoscInstrTest<-data.frame(c("Loan Scope FG5"))
  names(MyEndoscInstrTest)<-"Instrument"
  MyEndoscInstrTest <- EndoscInstrument(MyEndoscInstrTest, "Instrument")
  expect_true(all(!is.na(MyEndoscInstrTest$Instrument)))
  expect_identical(MyEndoscInstrTest$Instrument,"FG5")
})

#### EndoscIndications test ####

test_that("EndoscIndications", {
  MyEndoscIndicTest<-data.frame(c("Follow up ulcer healing.\n.\n"))
  names(MyEndoscIndicTest)<-"Indication"
  MyEndoscIndicTest <- EndoscIndications(MyEndoscIndicTest, "Indication")
  expect_true(all(!is.na(MyEndoscIndicTest$Indication)))
  expect_identical(MyEndoscIndicTest$Indication,"Follow up ulcer healing.")
})

#### EndoscProcPerformed test ####

test_that("EndoscProcPerformed", {
  MyEndoscProcPerfTest<-data.frame(c("OGD - Quality of Procedure: Adequate"))
  names(MyEndoscProcPerfTest)<-"ProcPerformed"
  MyEndoscProcPerfTest <- EndoscProcPerformed(MyEndoscProcPerfTest, "ProcPerformed")
  expect_true(all(!is.na(MyEndoscProcPerfTest$ProcPerformed)))
  expect_identical(MyEndoscProcPerfTest$ProcPerformed,"OGD ")
})

#### EndoscFindings test ####

test_that("EndoscFindings", {
  MyEndoscFindingsTest<-data.frame(c("At 25cm A"))
  names(MyEndoscFindingsTest)<-"EndoscFindings"
  MyEndoscFindingsTest <- EndoscFindings(MyEndoscFindingsTest, "EndoscFindings")
  expect_true(all(!is.na(MyEndoscFindingsTest$EndoscFindings)))
  expect_identical(MyEndoscFindingsTest$EndoscFindings,"At 25cm\n")
})

#### NewLines test ####
test_that("NewLines", {
  #rm(MyendoColon)
NewLines(MyendoColon,'Original')
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
               expect_identical(res2,"Although the prep was poor.\n")
     
})

#### ColumnCleanUp test ####

test_that("ColumnCleanUp", {
  pp<-c("The rain in spain falls mainly",".\n",":What")
  pp<-data.frame(pp)
  me<-ColumnCleanUp(pp,"pp")
  
  me2<-me[grep("What",me)]
  expect_true(length(me2)>0)
  expect_identical(me2,"What")
})


#### HistolHistol test ####

test_that("HistolHistol", {
ff<-"There is no evidence of coeliac disease\n"
  ff<-data.frame(ff)
  names(ff)<-"Histology"
  HistolHistolTest<-HistolHistol(ff,'Histology')
  expect_true(all(!is.na(HistolHistolTest$Histol_Simplified)))
  expect_identical(HistolHistolTest$Histol_Simplified,
                   "There is no evidence of coeliac disease ")
})


#### HistolDx test ####

test_that("HistolDx", {
  ff<-"Barrett's oesophagus,Sigmoid colon, biopsy - Adenocarcinoma\n"
  ff<-data.frame(ff)
  names(ff)<-"Diagnosis"
  HistolDxTest<-HistolDx(ff,'Diagnosis')
  expect_true(all(!is.na(HistolDxTest$Dx_Simplified)))
  expect_identical(HistolDxTest$Dx_Simplified,
                   "Adenocarcinoma\n")
})

#### HistolExtrapolDx test ####

test_that("HistolExtrapolDx", {
 
  ff<-"Barrett's oesophagus- dysplasia seen"
  ff<-data.frame(ff)
  names(ff)<-"Diagnosis"
  HistolExtrapolDxTest<-HistolExtrapolDx(ff,'Diagnosis')
  expect_true(all(!is.na(HistolExtrapolDxTest$Dysplasia)))
  expect_identical(HistolExtrapolDxTest$Dysplasia,
                   "dyspla")
})

#### HistolAccessionNumber test ####

test_that("HistolAccessionNumber", {
  
  ff<-"Barrett's oesophagus- dysplasia seen SP-56-1500801"
  ff<-data.frame(ff)
  names(ff)<-"AccessionNumber"
  HistolAccessionNumberTest<-HistolAccessionNumber(ff,'AccessionNumber',
                                                   "SP-\\d{2}-\\d{7}")
  expect_true(all(!is.na(HistolAccessionNumberTest$AccessionNumber)))
  expect_identical(HistolAccessionNumberTest$AccessionNumber,
                   "SP-56-1500801")
})

#### HistolMacDescrip test ####

test_that("HistolMacDescrip", {
  
  ff<-"Three specimens collected the largest 
  measuring 3 x 2 x 1 mm and the smallest 2 x 1 x 5 mm"
  ff<-data.frame(ff)
  names(ff)<-"Macroscopicdescription"
  HistolHistolMacDescripTest<-HistolMacDescrip(ff,
                              'Macroscopicdescription')
  expect_true(all(!is.na(HistolHistolMacDescripTest$Macroscopicdescription)))
  expect_identical(HistolHistolMacDescripTest$Macroscopicdescription,
"3 specimens collected the largest
measuring 3 x 2 x 1 mm and the smallest 2 x 1 x 5 mm")
})

#### HistolNumbOfBx test ####

test_that("HistolNumbOfBx", {
  
  ff<-"3 specimens collected the largest 
  measuring 3 x 2 x 1 mm and the smallest 2 x 1 x 5 mm"
  ff<-data.frame(ff)
  names(ff)<-"Macroscopicdescription"
  HistolNumBxTest<-HistolNumbOfBx(ff,'Macroscopicdescription',"specimen")
  expect_true(all(!is.na(HistolNumBxTest$NumbOfBx)))
  expect_identical(HistolNumBxTest$NumbOfBx,3)

})

#### HistolBxSize test ####

test_that("HistolBxSize", {
  
  ff<-"3 specimens collected the largest measuring 3 x 2 x 1 mm"
  ff<-data.frame(ff)
  names(ff)<-"Macroscopicdescription"
  ff$Macroscopicdescription<-as.character(ff$Macroscopicdescription)
  HistolBxSizeTest<-HistolBxSize(ff,'Macroscopicdescription')
  expect_true(all(!is.na(HistolBxSizeTest$BxSize)))
  expect_identical(HistolBxSizeTest$BxSize,6)
})


##### rrrrrrrrrrrrrrrrrrrEndoMineR functions #####

#### SurveilTimeByRow test ####

test_that("SurveilTimeByRow", {
  em <- SurveilTimeByRow(Myendo, "HospitalNumber", "Dateofprocedure")
  expect_true(nrow(em) > 0)
})

#### SurveilLastToNow test ####

test_that("SurveilLastToNow", {
  em <- SurveilLastToNow(Myendo, "HospitalNumber", "Dateofprocedure")
  expect_true(nrow(em) > 0)
})

#### SurveilLastTest test ####

test_that("SurveilLastTest", {
  em <- SurveilLastTest(Myendo, "HospitalNumber", "Dateofprocedure")
  expect_true(nrow(em) > 0)
})

#### SurveilFirstTest test ####

test_that("SurveilFirstTest", {
  em <- SurveilFirstTest(Mypath, "HospitalNumber", "Dateofprocedure")
  expect_true(nrow(em) > 0)
})

#### SurveilTimeByRow test ####

test_that("SurveilTimeByRow", {
  em <- SurveilTimeByRow(Myendo, "HospitalNumber", "Dateofprocedure")
  expect_true(nrow(em) > 0)
})

#### SurveilCapacity test ####

test_that("SurveilCapacity", {
  em <- SurveilCapacity(Myendo, "Dateofprocedure")
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
  Myendo <- EndoscMeds(Myendo, "Medications")
  Fent <- MetricByEndoscopist(Myendo, "Endoscopist", "Fent")
  expect_true(class(Fent[1]) == "list")
  expect_true(class(Fent[2]) == "list")
})

#### TermStandardLocation test ####

test_that("TermStandardLocation", {
  Histoltree <-c("Hospital Number:","Patient Name:",
 "General Practitioner:","Date received:","Clinical Details",
  "Nature of specimen","Histology","Diagnosis",""
)


PathDataFrameFinalColon <-Extractor(PathDataFrameFinalColon,"PathReportWhole",
                                    Histoltree)
names(PathDataFrameFinalColon)[names(PathDataFrameFinalColon) == 
                                 'Datereceived'] <- 'Dateofprocedure'
Mypath$Dateofprocedure <- as.Date(Mypath$Dateofprocedure)
f<-TermStandardLocation(PathDataFrameFinalColon,'Histology')
f<-PolypLocator(f,'SampleLocation')
  
})


#### PolypLocator test ####

test_that("PolypLocator", {
  
  Histoltree <-c("Hospital Number","Patient Name:","DOB:",
                 "General Practitioner:",
                 "Date received:","Clinical Details:",
                 "Macroscopic description:",
                 "Histology:","Diagnosis:"
  )
  PathDataFrameFinalColon <-
    Extractor(PathDataFrameFinalColon,"PathReportWhole",Histoltree)
  names(PathDataFrameFinalColon)[names(PathDataFrameFinalColon) ==
                                   'Datereceived'] <- 'Dateofprocedure'
  Mypath$Dateofprocedure <- as.Date(PathDataFrameFinalColon$Dateofprocedure)
  f<-TermStandardLocation(PathDataFrameFinalColon,'Histology')
  f<-PolypLocator(f,'SampleLocation')
})

#### GRS_Type_Assess_By_Unit test ####

test_that("GRS_Type_Assess_By_Unit", {
 #  
 
 vColon2<-HistolDx(vColon,'Diagnosis')
 vColon2<-HistolExtrapolDx(vColon2,'Diagnosis')
 vColon2<-HistolNumbOfBx(vColon2,'Macroscopicdescription','specimen')
 vColon2<-HistolBxSize(vColon2,'Macroscopicdescription')
 GRSTable<-GRS_Type_Assess_By_Unit(vColon2,'ProcedurePerformed', 
 'Endoscopist','Diagnosis','Histology')
  
})

#### NumberPerformed test ####

test_that("NumberPerformed", {
  Myendo<-Myendo[grepl('Gastroscopy',Myendo$ProcedurePerformed),]
  NumberPerformed(Myendo,'Endoscopist','Indications')
})

#### rrrrrrrrrrrrrrrrrrrrrrr Barretts Functions Test ####

#### Barretts_PragueScore ####

test_that("Barretts_PragueScore", {
  
  v<-Barretts_PragueScore(Myendo,'Findings')
  expect_true(nrow(v) > 0)
  
})


#### Barretts_PathStage ####

test_that("Barretts_PathStage", {
  b<-Barretts_PathStage(v,'Histology')
  
})


#### Barretts_EventType ####

test_that("Barretts_EventType", {

b<-Barretts_EventType(v,'Histology','ProcedurePerformed','EndoscopicDiagnosis'
                            ,'Findings')
})


#### Barretts_FUType ####

test_that("Barretts_FUType", {
  b<-Barretts_PathStage(v,'Histology')
  b2<-Barretts_EventType(b,'Histology',
  'ProcedurePerformed','Original','Findings')
  b3<-Barretts_FUType(b2,'Findings')
  
})

#### BarrettsSurveil ####

test_that("BarrettsSurveil", {
  
Enroll<-BarrettsSurveil(Myendo,
                     'HospitalNumber','Dateofprocedure','Indications')
  
})


#### BarrettsSurveil_HospNum ####

test_that("BarrettsSurveil_HospNum", {
  
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')
  b3<-Barretts_EventType(b2,'Histology',
  'ProcedurePerformed','Original','Findings')
  b4<-Barretts_FUType(b3,'Findings')
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  Rule<-BarrettsSurveil_HospNum(b4,'Rule1','HospitalNumber')
  
})


#### BarrettsDocumentQual ####

test_that("BarrettsDocumentQual", {
  b<-Barretts_PathStage(v,'Histology')
  BarrettsDocumentQual(b,'Findings')
})


#### BarrettsBxQual ####

test_that("BarrettsBxQual", {
  v<-HistolExtrapolDx(v,'Diagnosis')
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')
  b3<-Barretts_EventType(b2,'Histology','ProcedurePerformed',
                               'Original','Findings')
  b4<-Barretts_FUType(b3,'Findings')
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsBxQual(b4,'Date.x','HospitalNumber',
                                       'Endoscopist')
  
})

#### BarrettsPathDetectQual ####

test_that("BarrettsPathDetectQual", {
  v<-HistolDx(v,'Diagnosis')
  v<-HistolExtrapolDx(v,'Diagnosis')
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')
  b3<-Barretts_EventType(b2,'Histology',
  'ProcedurePerformed','Original','Findings')
  b4<-Barretts_FUType(b3,'Findings')
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsPathDetectQual(b4,'Myplot')
})


#### BarrettsDDRQual ####

test_that("BarrettsDDRQual", {
  v<-HistolDx(v,'Diagnosis')
  v<-HistolExtrapolDx(v,'Diagnosis')
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')
  b3<-Barretts_EventType(b2,'Histology',
                               'ProcedurePerformed','Original',
                               'Findings')
  b4<-Barretts_FUType(b3,'Findings')
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsDDRQual(b4,'Endoscopist','IMorNoIM')
})



#### BarrettsEMRGrades  ####

test_that("BarrettsEMRGrades", {
  v<-HistolDx(v,'Diagnosis')
  v<-HistolExtrapolDx(v,'Diagnosis')
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')
  b3<-Barretts_EventType(b2,'Histology',
  'ProcedurePerformed','Original','Findings')
  b4<-Barretts_FUType(b3,'Findings')
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsEMRGrades(b4)
})


#### BarrettsBasicNumbers ####

test_that("BarrettsBasicNumbers", {
  v<-HistolDx(Mypath,'Diagnosis')
  v<-HistolExtrapolDx(v,'Diagnosis')
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,
                "Dateofprocedure","HospitalNumber")
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')
  b3<-Barretts_EventType(b2,'Histology',
                         'ProcedurePerformed','Original','Findings')
  b4<-Barretts_FUType(b3,'Findings')
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsBasicNumbers(b4,"Date.x")
})


#### BarrettssRFACath ####

test_that("BarrettssRFACath", {
  v<-HistolDx(v,'Diagnosis')
  v<-HistolExtrapolDx(v,'Diagnosis')
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')
  b3<-Barretts_EventType(b2,'Histology',
                       'ProcedurePerformed','Original','Findings')
  b4<-Barretts_FUType(b3,'Findings')
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettssRFACath(b4,"ProcedurePerformed","Findings")
})


#### BarrettsParisEMR ####

test_that("BarrettsParisEMR", {
  v<-HistolDx(v,'Diagnosis')
  v<-HistolExtrapolDx(v,'Diagnosis')
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')
  b3<-Barretts_EventType(b2,'Histology',
                     'ProcedurePerformed','Original','Findings')
  b4<-Barretts_FUType(b3,'Findings')
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsParisEMR(b4,"ProcedurePerformed","Findings")
})


#### Barretts_CRIM ####
test_that("Barretts_CRIM", {
  v<-HistolDx(v,'Diagnosis')
  v<-HistolExtrapolDx(v,'Diagnosis')
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')
  b3<-Barretts_EventType(b2,'Histology',
                     'ProcedurePerformed','Original','Findings')
  colnames(b3)[colnames(b3) == 'pHospitalNum'] <- 'HospitalNumber'
  Barretts_CRIM(b3,'HospitalNumber',"EVENT")
})
