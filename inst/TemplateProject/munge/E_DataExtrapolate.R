#DataExtrapolate

## @knitr dataExtrapolate

#Extract and clean the meds (need to bind to the existing dataframe)
FinalDataset<-cbind(EndoscMeds(FinalDataset$medications),FinalDataset)

#Now extrapolation with histology
FinalDataset$NumBx<-HistolNumbOfBx(FinalDataset$macroscopicdescription,'specimen')
FinalDataset$BxSize<-HistolBxSize(FinalDataset$macroscopicdescription)
names(FinalDataset)
HistolTypeAndSiteToJoin<-HistolTypeAndSite(FinalDataset$procedureperformed,FinalDataset$macroscopicdescription,FinalDataset$Original.x)
FinalDataset<-cbind(FinalDataset,HistolTypeAndSiteToJoin)