

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





##### rrrrrrrrrrrrrrrrrrrCleanUp test functions ####

# Make sure that the Endoscopist cleanup function has data in the Endoscopist
# column
#### Extractor test ####
test_that("Extractor", {
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
  MyEndoscEndoscopistTest <- data.frame(c("Dr Jeremiah Stubborn"))
  names(MyEndoscEndoscopistTest) <- "Endoscopist"

  MyEndoscEndoscopistTest <- EndoscEndoscopist(tolower(MyEndoscEndoscopistTest$Endoscopist))

  expect_true(all(!is.na(MyEndoscEndoscopistTest)))
  Result <- as.character(MyEndoscEndoscopistTest)
  expect_identical(Result, "jeremiah stubborn")
})

#### EndoscMeds test ####

test_that("EndoscMeds", {
  MyEndoscMedsTest <- data.frame(c("Fentanyl 125mcg, Midazolam 5mg"))
  names(MyEndoscMedsTest) <- "Medications"
  MyendoMeds <- EndoscMeds(MyEndoscMedsTest$Medications)
  expect_true(all(!is.na(MyendoMeds$Medications)))
  expect_true(all(!is.na(MyendoMeds$Fent)))
  expect_true(all(!is.na(MyendoMeds$Midaz)))
  expect_identical(MyendoMeds$Fent, 125)
  expect_identical(MyendoMeds$Midaz, 5)
})

#### EndoscInstrument test ####

test_that("EndoscInstrument", {
  MyEndoscInstrTest <- data.frame(c("Loan Scope FG5"))
  names(MyEndoscInstrTest) <- "Instrument"
  MyEndoscInstrTest <- EndoscInstrument(MyEndoscInstrTest$Instrument)
  expect_true(all(!is.na(MyEndoscInstrTest)))
  expect_identical(MyEndoscInstrTest, "FG5")
})



#### NegativeRemove test ####

test_that("NegativeRemove", {
  anexample <- c(
    "There is no evidence of polyp here",
    "Although the prep was poor,there was no adenoma found",
    "The colon was basically inflammed, but no polyp was seen",
    "The Barrett's segment was not biopsied",
    "The C0M7 stretch of Barrett's was flat"
  )
  anexample <- data.frame(anexample, stringsAsFactors = FALSE)
  names(anexample) <- "Thecol"
  res <- NegativeRemove(anexample$Thecol)
  res <- data.frame(res, stringsAsFactors = FALSE)
  res2 <- res[grep("Although the prep was poor.", res$res), ]
  expect_true(length(res2) > 0)
  expect_identical(res2, "Although the prep was poor.\n")
})

#### ColumnCleanUp test ####

test_that("ColumnCleanUp", {
  pp <- c("The rain in spain falls mainly", ".\n", ":What")
  pp <- data.frame(pp)
  me <- ColumnCleanUp(pp$pp)

  me2 <- me[grep("What", me)]
  expect_true(length(me2) > 0)
  expect_identical(me2, "What")
})












#### HistolNumbOfBx test ####

test_that("HistolNumbOfBx", {
  ff <- "3 specimens collected the largest 
  measuring 3 x 2 x 1 mm and the smallest 2 x 1 x 5 mm"
  ff <- data.frame(ff)
  names(ff) <- "Macroscopicdescription"
  HistolNumBxTest <- HistolNumbOfBx(ff$Macroscopicdescription, "specimen")
  expect_true(all(!is.na(HistolNumBxTest)))
  expect_identical(HistolNumBxTest, 3)
})

#### HistolBxSize test ####

test_that("HistolBxSize", {
  ff <- "3specimens collected the largest measuring 3 x 2 x 1 mm and the smallest 2 x 1 x 5 mm"
  ff <- data.frame(ff)
  names(ff) <- "Macroscopicdescription"
  ff$Macroscopicdescription <- as.character(ff$Macroscopicdescription)
  HistolBxSizeTest <- HistolBxSize(ff$Macroscopicdescription)
  expect_true(all(!is.na(HistolBxSizeTest)))
  expect_identical(HistolBxSizeTest, 6)
})


##### rrrrrrrrrrrrrrrrrrrEndoMineR functions #####


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


#### HowManyTests test ####

test_that("HowManyOverTime", {
  Tests <- HowManyOverTime(Myendo, "Indications", "Dateofprocedure", "S.*")
  expect_true(class(Tests) == "data.frame")
})

#### SurveySankey test ####

test_that("SurveySankey", {
  names(Myendo)[names(Myendo) == "HospitalNumber"] <- "PatientID"
  SurveySankey(Myendo, "ProcedurePerformed", "PatientID")
})

#### PatientFlow_CircosPlots test ####

test_that("PatientFlow_CircosPlots", {
  Event <- list(
    x1 = "Therapeutic- Dilatation",
    x2 = "Other-", x3 = "Surveillance",
    x4 = "APC", x5 = "Therapeutic- RFA TTS",
    x5 = "Therapeutic- RFA 90",
    x6 = "Therapeutic- EMR", x7 = "Therapeutic- RFA 360"
  )
  EndoEvent <- replicate(2000, sample(Event, 1, replace = F))
  fff <- unlist(EndoEvent)
  fff <- data.frame(fff)
  names(fff) <- "col1"
  Myendo <- cbind(fff$col1, Myendo)
  names(Myendo)[names(Myendo) == "fff$col1"] <- "EndoEvent"
  Myendo$EndoEvent <- unlist(Myendo$EndoEvent)
  Myendo$EndoEvent <- as.character(Myendo$EndoEvent)
  names(Myendo)[names(Myendo) == "HospitalNumber"] <- "PatientID"
  PatientFlow_CircosPlots(Myendo, "Dateofprocedure", "PatientID", "EndoEvent")
})



#### MetricByEndoscopist test ####

test_that("MetricByEndoscopist", {
  myNotableWords <- c("arrett", "oeliac")
  tt <- ListLookup(Myendo, "Findings", myNotableWords)
  expect_true(class(tt) == "data.frame")
})

#### MetricByEndoscopist test ####

test_that("MetricByEndoscopist", {
  Myendo <- cbind(EndoscMeds(Myendo$Medications), Myendo)
  Fent <- MetricByEndoscopist(Myendo, "Endoscopist", "Fent")
  expect_true(class(Fent) == "data.frame")
})




#### GRS_Type_Assess_By_Unit test ####

test_that("GRS_Type_Assess_By_Unit", {
  data(vColon)
  GRSTable <- GRS_Type_Assess_By_Unit(
    vColon, "ProcedurePerformed",
    "Endoscopist", "Diagnosis", "Histology"
  )
  expect_true(nrow(GRSTable) > 0)
})



#### rrrrrrrrrrrrrrrrrrrrrrr Barretts Functions Test ####

#### Barretts_PragueScore ####

test_that("Barretts_PragueScore", {
  ff <- "Barrett's oesophagus C1 M5"
  ff <- data.frame(ff)
  names(ff) <- "Findings"

  v <- Barretts_PragueScore(ff, "Findings")
  expect_true(nrow(v) > 0)
  expect_identical(as.numeric(v$CStage), 1)
  expect_identical(as.numeric(v$MStage), 5)
})


#### Barretts_PathStage ####

test_that("Barretts_PathStage", {
  ff <- "  Two biopsies consist of small bowel mucosa and are within 
  normal histological limits\n\n"
  ff <- data.frame(ff)
  names(ff) <- "Histology"

  ff <- Barretts_PathStage(ff, "Histology")
  expect_true(length(ff) > 0)
  expect_identical(ff, "Insufficient")
})



#### Barretts_FUType ####

test_that("Barretts_FUType", {
  f1 <- "  Two biopsies consist of small bowel mucosa and are within 
  normal histological limits with intestinal metaplasia\n\n"
  f2 <- "Lots of biopsies taken"
  f3 <- "  More text here"
  f4 <- "The Barrett's length was C1M2"
  ff <- data.frame(f1, f2, f3, f4)
  names(ff) <- c(
    "Histology", "ProcedurePerformed", "EndoscopicDiagnosis",
    "Findings"
  )
  ff <- Barretts_PragueScore(ff, "Findings")
  ff$PathStage <- Barretts_PathStage(ff, "Histology")
  ff$FU_Type <- Barretts_FUType(ff, "CStage", "MStage", "PathStage")
  expect_true(nrow(ff) > 0)
  expect_identical(ff$FU_Type, "Rule2")
})
