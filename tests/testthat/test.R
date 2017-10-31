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

test_that("EndoscChopperEndoscopist", {
  Myendo<-EndoscChopperEndoscopist(Myendo,"Endoscopist")
  Myendo$Endoscopist<-gsub("2nd [Ee]ndoscopist","Second endoscopist",Myendo$Endoscopist)
  expect_true(all(!is.na(Myendo$Endoscopist)))
  
})

test_that("EndoscChopperMeds", {
  Myendo<-EndoscChopperMeds(Myendo,"Medications")
  expect_true(all(!is.na(Myendo$Medications)))
})

test_that("EndoscChopperInstrument", {
  Myendo<-EndoscChopperInstrument(Myendo,"Instrument")
  expect_true(all(!is.na(Myendo$Instrument)))
})

test_that("EndoscChopperIndications", {
  Myendo<-EndoscChopperIndications(Myendo,"Indications")
  expect_true(all(!is.na(Myendo$Indications)))
})

test_that("EndoscChopperProcPerformed", {
  Myendo<-EndoscChopperProcPerformed(Myendo,"ProcedurePerformed")
  expect_true(all(!is.na(Myendo$Findings)))
})

test_that("EndoscChopperFindings", {
  Myendo<-EndoscChopperFindings(Myendo,"Findings")
  expect_true(all(!is.na(Myendo$Findings)))
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
  expect_true(all(!is.na(Mypath$Dx)))
})

test_that("HistolChopperExtrapolDx", {
  Mypath<-HistolChopperExtrapolDx(Mypath,"Diagnosis")
  expect_false(all(!is.na(Mypath$Dysplasia)))
})


test_that("HistolChopperNumbOfBx", {
  Mypath<-HistolChopperNumbOfBx(Mypath,"Natureofspecimen","specimen")
  expect_true(all(!is.na(Mypath$NumbOfBx)))
})

test_that("HistolChopperNumbOfBx", {
  #Mypath<-HistolChopperMacDescrip(Mypath, "Natureofspecimen")
  Mypath<-HistolChopperBxSize(Mypath,"Natureofspecimen")
  expect_true(all(!is.na(Mypath$BxSize)))
})


##### EndoMineR functions #####

test_that("SurveillanceTimeByRow", {
  em<-SurveillanceTimeByRow(Myendo,"HospitalNumber","Dateofprocedure")
  expect_true(nrow(em)>0)
})

test_that("SurveillanceLastToNow", {
  em<-SurveillanceLastToNow(Myendo,"HospitalNumber","Dateofprocedure")
  expect_true(nrow(em)>0)
})

test_that("SurveillanceLastTest", {
  em<-SurveillanceLastTest(Myendo,"HospitalNumber","Dateofprocedure")
  expect_true(nrow(em)>0)
})

test_that("SurveillanceFirstTest", {
  em<-SurveillanceFirstTest(Mypath,"HospitalNumber","Dateofprocedure")
  expect_true(nrow(em)>0)
})

test_that("SurveillanceTimeByRow", {
  em<-SurveillanceTimeByRow(Myendo,"HospitalNumber","Dateofprocedure")
  expect_true(nrow(em)>0)
})

test_that("SurveillanceCapacity", {
  em<-SurveillanceCapacity(Myendo,"Dateofprocedure")
})


test_that("HowManyTests", {
  Tests<-HowManyTests(Myendo,"Indications","Dateofprocedure","S.*")
  expect_true(class(Tests[1])=="list")
  expect_true(class(Tests[2])=="list")
})

test_that("SurveySankey", {
})

test_that("PatientFlow_CircosPlots", {
})

test_that("ListLookup", {
})

test_that("MetricByEndoscopist", {
  myNotableWords<-c("arrett","oeliac")
  tt<-ListLookup(Myendo,"Findings",myNotableWords)
  expect_true(class(tt)=="data.frame")
})



test_that("MetricByEndoscopist", {
  Myendo<-EndoscChopperMeds(Myendo,"Medications")
  Fent<-MetricByEndoscopist(Myendo,"Endoscopist","Fent")
  expect_true(class(Fent[1])=="list")
  expect_true(class(Fent[2])=="list")
})

test_that("TermStandardLocation", {
})

test_that("SampleLocator", {
})

test_that("PolypLocator", {
})

test_that("PolypTidyUpLocator", {
})

test_that("GRS_Type_Assess_By_Unit", {
})

test_that("NumberPerformed", {
})

test_that("TermStandardLocation", {
})




