## ----exampleChopperNewLines, eval = FALSE--------------------------------
#  v<-ChopperNewLines(Myendo,'OGDReportWhole')

## ----exampleExtractor, eval = FALSE--------------------------------------
#  Mypath<-data(PathDataFrameFinalColon)
#  HistolTree<-list("Hospital Number","Patient Name","DOB:","General Practitioner:",
#  "Date of procedure:","Clinical Details:","Macroscopic description:","Histology:","Diagnosis:","")
#  for(i in 1:(length(HistolTree)-1)) {
#  Mypath<-Extractor(Mypath,"PathReportWhole",as.character(HistolTree[i]),
#  as.character(HistolTree[i+1]),as.character(HistolTree[i]))
#  }

## ----exampleEndoscChopperEndoscopist, eval = FALSE-----------------------
#  EndoscChopperEndoscopist(Myendo,'Endoscopist')

## ----exampleEndoscChopperEndoscopistWhole, eval = FALSE------------------
#  EndoscChopperEndoscopist <- function(x, y) {
#      # Extraction of the Endoscopist
#      x <- data.frame(x)
#      x[, y] <- gsub("Dr", "", x[, y], fixed = TRUE)
#      x[, y] <- gsub("Mr", "", x[, y], fixed = TRUE)
#      x[, y] <- gsub("[^[:alnum:],]", "", x[, y])
#      # Put gaps between names
#      x[, y] <- gsub("([a-z])([A-Z])", "\\1 \\2", x[, y])
#      x[, y] <- gsub("2nd.*", "", x[, y])
#      x[, y] <- trimws(x[, y], which = c("both"))
#  
#      return(x)
#  }
#  

## ----exampleEndoCleaningFunc, eval = FALSE-------------------------------
#  v<-EndoscChopperMeds(Myendo,'Medications')
#  v<-EndoscChopperInstrument(Myendo,'Instrument')
#  v<-EndoscChopperIndications(Myendo,'Indications')
#  v<-EndoscChopperProcPerformed(Myendo,'ProcedurePerformed')

## ----exampleEHistolChopperHistol, eval = FALSE---------------------------
#  t<-HistolChopperHistol(Mypath,'Histology')

## ----exampleHistolChopperMacDescrip, eval = FALSE------------------------
#  HistolChopperNumbOfBx <- function(x, y, z) {
#      x <- data.frame(x)
#      x <- HistolChopperMacDescrip(x, y)
#      mylist <- str_match_all(x[, y], paste("[0-9]{1,2}.{0,3}", z, sep = ""))
#      x$NumbOfBx <- sapply(mylist, function(p) sum(as.numeric(gsub(z, "", p))))
#      return(x)
#  }

## ----exampleHistolChopperNumbOfBx, eval = FALSE--------------------------
#  v<-HistolChopperNumbOfBx(Mypath,'Macroscopicdescription','specimen')

## ----exampleOtherFunctionsHistology, eval = FALSE------------------------
#  v<-HistolChopperMacDescripCleanup(Mypath,"Macroscopicdescription")
#  v<-HistolChopperExtrapolDx(Mypath,"Diagnosis")
#  v<-HistolChopperAccessionNumber(Mypath,"Histology","SP-\\d{2}-\\d{7}")

## ----exampleSurveillanceTimeByRow, eval = FALSE--------------------------
#  em<-SurveillanceTimeByRow(Myendo,'HospitalNumber','Dateofprocedure')
#  em<-SurveillanceLastToNow(Myendo,'HospitalNumber','Dateofprocedure')
#  em<-SurveillanceLastTest(Myendo,'HospitalNumber','Dateofprocedure')
#  em<-SurveillanceFirstTest(Myendo,'HospitalNumber','Dateofprocedure')

## ----exampleSurveillanceCapacity, eval = FALSE---------------------------
#  em<-SurveillanceCapacity(Myendo,"Dateofprocedure)

## ----exampleHowManyTests, eval = FALSE-----------------------------------
#  how<-HowManyTests(Myendo,'Indications','Dateofprocedure','Surv')

## ----exampleSurveySankey, eval = FALSE-----------------------------------
#  how<-SurveySankey(Myendo,"ProcPerformed")

## ----examplePatientFlow_CircosPlots, eval = FALSE------------------------
#  flow<-PatientFlow_CircosPlots(v,"Dateofprocedure","HospitalNumber","ProcedurePerformed")

## ----exampleListLookup, eval = FALSE-------------------------------------
#  tt<-ListLookup(Myendo,'Findings',myNotableWords)

## ----exampleEndoscChopperMeds, eval = FALSE------------------------------
#  Myendo<-EndoscChopperMeds(Myendo,'Medications')
#  MetricByEndoscopist(Myendo,'Endoscopist','Fent')

## ----exampleTermStandardLocation, eval = FALSE---------------------------
#  f<-TermStandardLocation(Mypath,'Histology')

## ----examplePolypTidyUpLocator, eval = FALSE-----------------------------
#  
#  f<-PolypTidyUpLocator(f,'SampleLocation')
#  f<-PolypLocator(f,'SampleLocation')
#  f<-SampleLocator(f,'SampleLocation')

## ----exampleGRS_Type_Assess_By_Unit, eval = FALSE------------------------
#  GRSTable<-GRS_Type_Assess_By_Unit(v,'ProcedurePerformed','Endoscopist','Diagnosis','Histology')

## ----exampleBarrettsDataAccord_Prague, eval = FALSE----------------------
#  v<-BarrettsDataAccord_Prague(Myendo,'Findings')

## ----exampleBarrettsDataAccord_PathStage, eval = FALSE-------------------
#  b<-BarrettsDataAccord_PathStage(v,'Histology')

## ----exampleBarrettsDataAccord_Event, eval = FALSE-----------------------
#  v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",Mypath,"Dateofprocedure","HospitalNumber")
#  b<-BarrettsDataAccord_Event(v,'Histology', 'ProcedurePerformed','OGDReportWhole','Findings')

## ----exampleBarrettsDataAccord_FUGroup, eval = FALSE---------------------
#  v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",Mypath,"Dateofprocedure","HospitalNumber")
#  b<-BarrettsDataAccord_PathStage(v,'Histology')
#  b2<-BarrettsDataAccord_Event(b,'Histology','ProcedurePerformed','OGDReportWhole','Findings')
#  b3<-BarrettsDataAccord_FUGroup(b2,'Findings')

## ----exampleBarrettsPatientTracking_Enrollment_Surveillance, eval = FALSE----
#  Enroll<-BarrettsPatientTracking_Enrollment_Surveillance(Myendo,'HospitalNumber','Dateofprocedure','Indications')

## ----exampleBarrettsPatientTracking_UniqueHospNum, eval = FALSE----------
#  v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,"Dateofprocedure","HospitalNumber")
#  b1<-BarrettsDataAccord_Prague(v,'Findings')
#  b2<-BarrettsDataAccord_PathStage(b1,'Histology')
#  b3<-BarrettsDataAccord_Event(b2,'Histology','ProcedurePerformed','OGDReportWhole','Findings')
#  b4<-BarrettsDataAccord_FUGroup(b3,'Findings')
#  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#  Rule<-BarrettsPatientTracking_UniqueHospNum(b4,'Rule1','HospitalNumber')

## ----exampleBarrettsQuality_AnalysisDocumentation, eval = FALSE----------
#  v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,"Dateofprocedure","HospitalNumber")
#  b<-BarrettsDataAccord_PathStage(v,'Histology')
#  BarrettsQuality_AnalysisDocumentation(b,"Findings")

## ----exampleBarrettsQuality_AnalysisBiopsyNumber, eval = FALSE-----------
#  # Firstly relevant columns are extrapolated from the
#   # Mypath demo dataset. These functions are all part of Histology data
#   # cleaning as part of the package.
#   v<-HistolChopperDx(Mypath,'Diagnosis')
#   v<-HistolChopperExtrapolDx(v,'Diagnosis')
#   v<-HistolChopperNumbOfBx(v,'Macroscopicdescription','specimen')
#   v<-HistolChopperBxSize(v,'Macroscopicdescription')
#   # The histology is then merged with the Endoscopy dataset. The merge occurs
#   # according to date and Hospital number
#   v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#   'HospitalNumber')
#   # The function relies on the other Barrett's functions being run as well:
#   b1<-BarrettsDataAccord_Prague(v,'Findings')
#   b2<-BarrettsDataAccord_PathStage(b1,'Histology')
#   b3<-BarrettsDataAccord_Event(b2,'Histology',
#   'ProcedurePerformed','OGDReportWhole','Findings')
#   # The follow-up group depends on the histology and the Prague score for a
#   # patient so it takes the processed Barrett's data and then looks in the
#  # Findings column for permutations of the Prague score.
#   b4<-BarrettsDataAccord_FUGroup(b3,'Findings')
#   colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#   # The number of average number of biopsies is then calculated and
#   # compared to the average Prague C score so that those who are taking
#   # too few biopsies can be determined
#   BarrettsQuality_AnalysisBiopsyNumber(b4,'Date.x','HospitalNumber',
#                                        'Endoscopist')

## ----exampleBarrettsSurveillance_PathDetection, eval = FALSE-------------
#  Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,"Dateofprocedure","HospitalNumber")
#  b1<-BarrettsDataAccord_Prague(v,'Findings')
#  b2<-BarrettsDataAccord_PathStage(b1,'Histology')
#  b3<-BarrettsDataAccord_Event(b2,'Histology','ProcedurePerformed','OGDReportWhole','Findings')
#  b4<-BarrettsDataAccord_FUGroup(b3,'Findings')
#  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#  BarrettsSurveillance_PathDetection(b4,'Myplot')

## ----exampleBarrettsSurveillanceDDR, eval = FALSE------------------------
#   v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,"Dateofprocedure","HospitalNumber")
#  b1<-BarrettsDataAccord_Prague(v,'Findings')
#  b2<-BarrettsDataAccord_PathStage(b1,'Histology')
#  b3<-BarrettsDataAccord_Event(b2,'Histology','ProcedurePerformed','OGDReportWhole','Findings')
#  b4<-BarrettsDataAccord_FUGroup(b3,'Findings')
#  colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#  BarrettsSurveillanceDDR(b4,'Endoscopist','IMorNoIM')

## ----exampleBarretts_LesionRecognitionEMR, eval = FALSE------------------
#  Barretts_LesionRecognitionEMR(dataframe,y,z)

## ----exampleBarrettsBasicNumbers, eval = FALSE---------------------------
#  BarrettsBasicNumbers(dataframe)

## ----exampleBarrettsTherapeuticsRFA_ByCatheter, eval = FALSE-------------
#  BarrettsTherapeuticsRFA_ByCatheter(dataframe)

## ----exampleBarrettsCRIM, eval = FALSE-----------------------------------
#  BarrettsCRIM(dataframe,HospNum,EVENT)

