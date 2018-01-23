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

## ----exampleEndoCleaningFunc, eval = FALSE-------------------------------
#  v<-EndoscChopperMeds(Myendo,'Medications')
#  v<-EndoscChopperInstrument(Myendo,'Instrument')
#  v<-EndoscChopperIndications(Myendo,'Indications')
#  v<-EndoscChopperProcPerformed(Myendo,'ProcedurePerformed')

## ----exampleEHistolChopperHistol, eval = FALSE---------------------------
#  t<-HistolChopperHistol(Mypath,'Histology')

## ----exampleHistolChopperNumbOfBx, eval = FALSE--------------------------
#  v<-HistolChopperNumbOfBx(Mypath,'Macroscopicdescription','specimen')

## ----exampleOtherFunctionsHistology, eval = FALSE------------------------
#  v<-HistolChopperMacDescripCleanup(Mypath,"Macroscopicdescription")
#  v<-HistolChopperExtrapolDx(Mypath,"Diagnosis")
#  v<-HistolChopperAccessionNumber(Mypath,"Histology","SP-\\d{2}-\\d{7}")

