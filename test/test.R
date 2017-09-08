
#### Create the fake data Upper GI####

##### CleanUp test functions ####
Mypath<-pathRep()
Myendo<-EndoRaw()
v<-Endomerge(Mypath,Myendo)
names(v)<-c("Endo_ResultText","Endo_ResultPerformed","HospNumId","Histo_ResultText","Histo_ResultPerformed","HospNumId2","Days")
EndoscTree<-list("Hospital Number:","Patient Name:","General Practitioner:","Date of procedure:","Endoscopist:","Endoscopist:","Medications","Instrument","Extent of Exam:","Indications:","Procedure Performed:","Findings:","Endoscopic Diagnosis:")

v<-ChopperNewLines(v,"Endo_ResultText")
for(i in 1:(length(EndoscTree)-1)) {
  v<-Extractor(v,"Endo_ResultText",as.character(EndoscTree[i]),as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
}

Histoltree<-list("Clinical Details","Nature of specimen","Histology","Diagnosis","")
v<-ChopperNewLines(v,"Histo_ResultText")
for(i in 1:(length(Histoltree)-1)) {
  v<-Extractor(v,"Histo_ResultText",as.character(Histoltree[i]),as.character(Histoltree[i+1]),gsub(" ","",as.character(Histoltree[i])))
}


#Make sure that the Endoscopist cleanup function has data in the Endoscopist column
test_that("Extractor", {
  expect_that(names(v),equals(c("Endo_ResultText","Endo_ResultPerformed","HospNumId","Histo_ResultText","Histo_ResultPerformed","HospNumId2",
                                 "Days","HospitalNumber","PatientName","GeneralPractitioner","Dateofprocedure","Endoscopist","Medications","Instrument",
                                 "ExtentofExam","Indications","ProcedurePerformed","Findings","ClinicalDetails","Natureofspecimen","Histology","Diagnosis")))
  expect_that(all(!is.na(v$Endo_ResultText)),equals(TRUE))
  expect_that(all(!is.na(v$Endo_ResultPerformed)),equals(TRUE))
  expect_that(all(!is.na(v$HospNumId)),equals(TRUE))
  expect_that(all(!is.na(v$Histo_ResultText)),equals(TRUE))
  expect_that(all(!is.na(v$Histo_ResultPerformed)),equals(TRUE))
  expect_that(all(!is.na(v$Days)),equals(TRUE))
  expect_that(all(!is.na(v$HospitalNumber)),equals(TRUE))
  expect_that(all(!is.na(v$PatientName)),equals(TRUE))
  expect_that(all(!is.na(v$GeneralPractitioner)),equals(TRUE))
  expect_that(all(!is.na(v$Dateofprocedure)),equals(TRUE))
  expect_that(all(!is.na(v$Endoscopist)),equals(TRUE))
  expect_that(all(!is.na(v$Medications)),equals(TRUE))
  expect_that(all(!is.na(v$Instrument)),equals(TRUE))
  expect_that(all(!is.na(v$ExtentofExam)),equals(TRUE))
  expect_that(all(!is.na(v$Indications)),equals(TRUE))
  expect_that(all(!is.na(v$ProcedurePerformed)),equals(TRUE))
  expect_that(all(!is.na(v$Findings)),equals(TRUE))
  })

test_that("EndoscDate", {
v$Endo_ResultPerformed<-as.Date(v$Endo_ResultPerformed,format="%d/%m/%Y")
expect_that(all(!is.na(v$Endo_ResultPerformed)),equals(TRUE))
})

test_that("EndoscChopperEndoscopist", {
  v<-EndoscChopperEndoscopist(v,"Endoscopist")
  v$Endoscopist<-gsub("Second","",v$Endoscopist)
  expect_that(all(!is.na(v$Endoscopist)),equals(TRUE))
  
})

test_that("EndoscChopperMeds", {
  v<-EndoscChopperMeds(v,"Medications")
  expect_that(all(!is.na(v$Medications)),equals(TRUE))
})

test_that("EndoscChopperInstrument", {
  v<-EndoscChopperInstrument(v,"Instrument")
  expect_that(all(!is.na(v$Instrument)),equals(TRUE))
})

test_that("EndoscChopperIndications", {
  v<-EndoscChopperIndications(v,"Indications")
  expect_that(all(!is.na(v$Indications)),equals(TRUE))
})

test_that("EndoscChopperProcPerformed", {
  v<-EndoscChopperProcPerformed(v,"ProcedurePerformed")
  expect_that(all(!is.na(v$Findings)),equals(TRUE))
})

test_that("EndoscChopperFindings", {
  v<-EndoscChopperFindings(v,"Findings")
  expect_that(all(!is.na(v$Findings)),equals(TRUE))
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
  v<-HistolChopperDx(v,"Diagnosis")
  expect_that(all(!is.na(v$Dx)),equals(TRUE))
})

test_that("HistolChopperExtrapolDx", {
  v<-HistolChopperExtrapolDx(v,"Diagnosis")
  expect_that(all(!is.na(v$Dysplasia)),equals(FALSE))
})

test_that("HistolChopperMacDescrip", {

})

test_that("HistolChopperNumbOfBx", {
  v<-HistolChopperNumbOfBx(v,"Natureofspecimen","specimen")
  expect_that(all(!is.na(v$NumbOfBx)),equals(TRUE))
})

test_that("HistolChopperNumbOfBx", {
  HistolChopperMacDescrip()
v<-HistolChopperBxSize(v,"Natureofspecimen")
expect_that(all(!is.na(v$BxSize)),equals(TRUE))
})

##### Barrett's test functions ####
##EndoMineR functions

test_that("SurveillanceTimeByRow", {
em<-SurveillanceTimeByRow(v,"HospitalNumber","Endo_ResultPerformed")
expect_that(nrow(em)>0,equals(TRUE))
})

test_that("SurveillanceLastToNow", {
em<-SurveillanceLastToNow(v,"HospitalNumber","Endo_ResultPerformed")
expect_that(nrow(em)>0,equals(TRUE))
})

test_that("SurveillanceLastTest", {
em<-SurveillanceLastTest(v,"HospitalNumber","Endo_ResultPerformed")
expect_that(nrow(em)>0,equals(TRUE))
})

test_that("SurveillanceFirstTest", {
em<-SurveillanceFirstTest(v,"HospitalNumber","Endo_ResultPerformed")
expect_that(nrow(em)>0,equals(TRUE))
})

test_that("SurveillanceTimeByRow", {
em<-SurveillanceTimeByRow(v,"HospitalNumber","Endo_ResultPerformed")
expect_that(nrow(em)>0,equals(TRUE))
})

test_that("SurveillanceCapacity", {
em<-SurveillanceCapacity(v)
})


test_that("HowManyTests", {
Tests<-HowManyTests(v,"Indications","Endo_ResultPerformed","S.*")
expect_that(class(Tests[1])=="list",equals(TRUE))
expect_that(class(Tests[2])=="list",equals(TRUE))
})

test_that("MetricByEndoscopist", {
myNotableWords<-c("arrett","oeliac")
tt<-ListLookup(v,"Findings",myNotableWords)
expect_that(class(tt)=="data.frame",equals(TRUE))
})



test_that("MetricByEndoscopist", {
Fent<-MetricByEndoscopist(v,"Endoscopist","Fent")
expect_that(class(Fent[1])=="list",equals(TRUE))
expect_that(class(Fent[2])=="list",equals(TRUE))
})




