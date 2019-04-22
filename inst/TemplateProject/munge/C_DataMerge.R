#Data merge

## @knitr dataMerge
FinalDataset<-Endomerge2(MyOGD,"dateofprocedure","hospitalnumber",MyPath,"datereceived","hospitalnumber")

head(FinalDataset)