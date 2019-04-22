#Analysis_Generic

## @knitr dataGeneric


FinalDatasetBarr<-SurveilTimeByRow(FinalDataset,'pHospitalNum','Date.x')

#Gives you all the last tests done:
LastTestsDone<-SurveilLastTest(FinalDataset,'pHospitalNum','Date.x')
FirstTestsDone<-SurveilFirstTest(FinalDataset,'pHospitalNum','Date.x')

#Need to sort this one out
ff<-HowManyOverTime(FinalDataset,'indications','Date.x','.*')