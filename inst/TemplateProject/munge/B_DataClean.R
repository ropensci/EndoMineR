#Data Clean and Merge

## @knitr dataClean
#Prepare the text for OGD
mywordsOGD<-c("hospital number:","patient name:","general practitioner","date of procedure:","endoscopist:","2nd endoscopist:","medications:","instrument:","extent of exam:","indications:", "procedure performed:","findings:","diagnosis:")
#MyOGD<-textPrep(TheOGDReportFinal$OGDReportWhole,mywordsOGD)
MyOGD<-textPrep(TheOGDReportFinal$OGDReportWhole,mywordsOGD,NegEx="TRUE",Extractor="1")

#Prepare the text for Pathology
mywordsPath<-c("hospital number:","patient name:","dob:","general practitioner:","date received:","nature of specimen:","macroscopic description:" ,"diagnosis:")
#MyPath<-textPrep(PathDataFrameFinal$PathReportWhole,mywordsPath)
MyPath<-textPrep(PathDataFrameFinal$PathReportWhole,mywordsPath,NegEx="TRUE",Extractor="1")
