

#### The data for the tests ####

# For the upper GI
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

MypathColon <-Extractor(PathDataFrameFinalColon,"PathReportWhole",mywords)
names(MypathColon)[names(MypathColon) == 'Datereceived'] <- 'Dateofprocedure'
MypathColon$Dateofprocedure <- as.Date(MypathColon$Dateofprocedure)

vColon <-
  Endomerge2(
    MyendoColon,
    "Dateofprocedure",
    "HospitalNumber",
    MypathColon,
    "Dateofprocedure",
    "HospitalNumber"
  )


##### rrrrrrrrrrrrrrrrrrrCleanUp test functions ####

#Make sure that the Endoscopist cleanup function has data in the Endoscopist 
#column
#### Extractor test ####
test_that("Extractor", {
  expect_that(names(v), equals(
    c("OGDReportWhole",
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
  
  MyEndoscEndoscopistTest <- EndoscEndoscopist(MyEndoscEndoscopistTest,
                                               "Endoscopist")
 
  expect_true(all(!is.na(MyEndoscEndoscopistTest$Endoscopist)))
  Result<-as.character(MyEndoscEndoscopistTest$Endoscopist)
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



#### EndoscProcPerformed test ####

test_that("EndoscProcPerformed", {
  MyEndoscProcPerfTest<-data.frame(c("OGD - Quality of Procedure: Adequate"))
  names(MyEndoscProcPerfTest)<-"ProcPerformed"
  MyEndoscProcPerfTest <- EndoscProcPerformed(MyEndoscProcPerfTest, 
                                              "ProcPerformed")
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



#### NegativeRemove test ####

test_that("NegativeRemove", {
  
  anexample<-c("There is no evidence of polyp here",
               "Although the prep was poor,there was no adenoma found",
              "The colon was basically inflammed, but no polyp was seen",
               "The Barrett's segment was not biopsied",
               "The C0M7 stretch of Barrett's was flat")
               anexample<-data.frame(anexample,stringsAsFactors = FALSE)
               names(anexample)<-"Thecol"
               res<-NegativeRemove(anexample,"Thecol")
               res<-data.frame(res,stringsAsFactors = FALSE)
               res2<-res[grep("Although the prep was poor.",res$res),]
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





#### HistolDx test ####

test_that("HistolDx", {
  ff<-"Barrett's oesophagus,Sigmoid colon, biopsy - Adenocarcinoma\n"
  ff<-data.frame(ff)
  names(ff)<-"Diagnosis"
  HistolDxTest<-HistolDx(ff,'Diagnosis')
  expect_true(all(!is.na(HistolDxTest$Dx_Simplified)))
  expect_identical(HistolDxTest$Dx_Simplified,
            "Barrett's oesophagus,Sigmoid colon, biopsy \nAdenocarcinoma\n")
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
#  em <- SurveilLastTest(Myendo, "HospitalNumber", "Dateofprocedure")
#  expect_true(nrow(em) > 0)
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
ff<-"The sigmoid polyp is a pedunculated tubular adenoma with moderate 
dysplasia\n,inflammation, gastric metaplasia, dysplasia or neoplasia is seen,
patchy fibrosis and myxoid areas and contains numerous eosinophils, 
as well as scattered\n,These biopsies of non-specialised gastric-type mucosa 
showed minimal chronic, focally active\n,Lymphovascular invasion: 
Not identified\n,into submucosa for less than 1 mm \n\n"
ff<-data.frame(ff)
names(ff)<-"Histology"
f<-TermStandardLocation(ff,'Histology')

f$Histology<-as.character(f$Histology)
TermStandardLocationTest<-TermStandardLocation(f,'Histology')
TermStandardLocationTest<-PolypLocator(TermStandardLocationTest,
                                       'SampleLocation')

TermStandardLocationTest$PolypLocator<-
  as.character(TermStandardLocationTest$PolypLocator)
expect_true(all(!is.na(TermStandardLocationTest$PolypLocator)))
expect_identical(TermStandardLocationTest$PolypLocator,"Sigmoid")
})



#### GRS_Type_Assess_By_Unit test ####

test_that("GRS_Type_Assess_By_Unit", {
 #  
 vColon2<-HistolDx(vColon,'Diagnosis')
 vColon2<-HistolExtrapolDx(vColon2,'Diagnosis',"")
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
  
  ff<-"Barrett's oesophagus C1 M5"
  ff<-data.frame(ff)
  names(ff)<-"Findings"
  
  v<-Barretts_PragueScore(ff,'Findings')
  expect_true(nrow(v) > 0)
  expect_identical(v$CStage,1)
  expect_identical(v$MStage,5)
  
})


#### Barretts_PathStage ####

test_that("Barretts_PathStage", {
  ff<-"  Two biopsies consist of small bowel mucosa and are within 
  normal histological limits\n\n"
  ff<-data.frame(ff)
  names(ff)<-"Histology"
  
  ff<-Barretts_PathStage(ff,'Histology')
  expect_true(nrow(ff) > 0)
  expect_identical(ff$IMorNoIM,"No_IM")
})



#### Barretts_FUType ####

test_that("Barretts_FUType", {
  
  f1<-"  Two biopsies consist of small bowel mucosa and are within 
  normal histological limits with intestinal metaplasia\n\n"
  f2<-"Lots of biopsies taken"
  f3<-"  More text here"
  f4<-"The Barrett's length was C1M2"
  ff<-data.frame(f1,f2,f3,f4)
  names(ff)<-c("Histology","ProcedurePerformed","EndoscopicDiagnosis",
               "Findings")
  
  ff1<-Barretts_PathStage(ff,'Histology')
  b3<-Barretts_FUType(ff1)
  expect_true(nrow(b3) > 0)
  expect_identical(b3$FU_Group,"Rule2")
  
})

#### BarrettsSurveil ####

test_that("BarrettsSurveil", {
  
#Enroll<-BarrettsSurveil(Myendo,
#                     'HospitalNumber','Dateofprocedure','Indications')
#expect_true(nrow(Enroll) > 0)
  
})


#### BarrettsSurveil_HospNum ####

test_that("BarrettsSurveil_HospNum", {
  
  f1<-"  Two biopsies consist of small bowel mucosa and are within 
  normal histological limits with intestinal metaplasia\n\n"
  f2<-"Lots of biopsies taken"
  f3<-"  More text here"
  f4<-"The Barrett's length was C1M2"
  f5<-"Z433255"
  ff<-data.frame(f1,f2,f3,f4,f5)
  names(ff)<-c("Histology","ProcedurePerformed","EndoscopicDiagnosis",
               "Findings","HospitalNumber")
  
  ff1<-Barretts_PathStage(ff,'Histology')

  b3<-Barretts_FUType(ff1)
  colnames(b3)[colnames(b3) == 'pHospitalNum'] <- 'HospitalNumber'
  Rule<-BarrettsSurveil_HospNum(b3,'Rule2','HospitalNumber')
  expect_identical(as.character(Rule$x),"Z433255")
})


#### BarrettsDocumentQual ####

test_that("BarrettsDocumentQual", {

  b1<-Barretts_PragueScore(Myendo,'Findings')
  BarrettsDocumentQual(b1,'Findings')
})


#### BarrettsBxQual ####

test_that("BarrettsBxQual", {
  
  v<-HistolExtrapolDx(v,'Diagnosis',"")
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')

  b4<-Barretts_FUType(b2)
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsBxQual(b4,'Date.x','HospitalNumber',
                                       'Endoscopist')
})

#### BarrettsPathDetectQual ####

test_that("BarrettsPathDetectQual", {
  v<-HistolDx(v,'Diagnosis')
  v<-HistolExtrapolDx(v,'Diagnosis',"")
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')
  b3<-Barretts_EventType(b2,'Histology',
  'ProcedurePerformed','OGDReportWhole','Findings')
  b4<-Barretts_FUType(b3)
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsPathDetectQual(b4,'Myplot')
})


#### BarrettsDDRQual ####

test_that("BarrettsDDRQual", {
  v<-HistolDx(v,'Diagnosis')
  v<-HistolExtrapolDx(v,'Diagnosis',"")
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')

  b4<-Barretts_FUType(b2)
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsDDRQual(b4,'Endoscopist','IMorNoIM')
})



#### BarrettsEMRGrades  ####

test_that("BarrettsEMRGrades", {
  v<-HistolDx(v,'Diagnosis')
  v<-HistolExtrapolDx(v,'Diagnosis',"")
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')

  b4<-Barretts_FUType(b2)
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsEMRGrades(b4)
})


#### BarrettsBasicNumbers ####

test_that("BarrettsBasicNumbers", {
  v<-HistolDx(Mypath,'Diagnosis')
  v<-HistolExtrapolDx(v,'Diagnosis',"")
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,
                "Dateofprocedure","HospitalNumber")
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')

  b4<-Barretts_FUType(b2)
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsBasicNumbers(b4,"Date.x")
})


#### BarrettssRFACath ####

test_that("BarrettssRFACath", {
  v<-HistolDx(v,'Diagnosis')
  v<-HistolExtrapolDx(v,'Diagnosis',"")
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')

  b4<-Barretts_FUType(b2)
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettssRFACath(b4,"ProcedurePerformed","Findings")
})


#### BarrettsParisEMR ####

test_that("BarrettsParisEMR", {
  v<-HistolDx(v,'Diagnosis')
  v<-HistolExtrapolDx(v,'Diagnosis',"")
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')
  b4<-Barretts_FUType(b2)
  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
  BarrettsParisEMR(b4,"ProcedurePerformed","Findings")
})


#### Barretts_CRIM ####
test_that("Barretts_CRIM", {
  v<-HistolDx(v,'Diagnosis')
  v<-HistolExtrapolDx(v,'Diagnosis',"")
  v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
  v<-HistolBxSize(v,'Macroscopicdescription')
  b1<-Barretts_PragueScore(v,'Findings')
  b2<-Barretts_PathStage(b1,'Histology')
  colnames(b2)[colnames(b2) == 'pHospitalNum'] <- 'HospitalNumber'
  sz<-Barretts_CRIM(b3,'HospitalNumber',"EVENT")
  sz<-data.frame(sz)
  expect_true(nrow(sz) > 0)
  
})
