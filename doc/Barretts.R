## ----setup, include=FALSE-----------------------------------------------------

library(knitr)
library(EndoMineR)
library(pander)
library(prettydoc)

## ----global_options, include=FALSE--------------------------------------------
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)

## ----fig.align='center',echo=FALSE,fig.width=18, fig.height=12----------------
knitr::include_graphics("img/BarrettsGuide.png")

## ----exampleBarretts_PragueScore,echo = TRUE,message=FALSE, warning=FALSE-----
v<-Barretts_PragueScore(Myendo,'Findings','OGDReportWhole')

## ----exampleBarretts_PragueScore2,echo = FALSE,message=FALSE, warning=FALSE----
panderOptions('table.split.table', Inf)
pander(v[23:27,(ncol(v)-4):ncol(v)])

## ----exampleBarretts_PathStage, echo = TRUE,message=FALSE, warning=FALSE------
#The histology column is the one we are interested in:
Mypath$b <- Barretts_PathStage(Mypath, "Histology")

## ----exampleBarretts_PathStage2, echo = TRUE,message=FALSE, warning=FALSE,echo=FALSE----
panderOptions('table.split.table', Inf)
pander(Mypath[2:3,(ncol(Mypath)-4):ncol(Mypath)])

## ----exampleBarretts_FUType, echo = TRUE,message=FALSE, warning=FALSE---------
#Create the merged dataset
v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",Mypath,"Dateofprocedure","HospitalNumber")
#Find the worst pathological grade for that endoscopy
v$IMorNoIM <- Barretts_PathStage(v, "Histology")
#Find the Prague score for that endoscopy
b1<-Barretts_PragueScore(v, "Findings", "OGDReportWhole")
#Get the follow-up type for that endoscopy
b1$FU_Type<-Barretts_FUType(b1,"CStage","MStage","IMorNoIM")


## ----exampleBarretts_FUType2, echo = FALSE,message=FALSE, warning=FALSE,echo=FALSE----
panderOptions('table.split.table', Inf)
pander(b1[23:27,(ncol(b1)-4):ncol(b1)])

## ----exampleBarrettsQuality_AnalysisBiopsyNumber,echo = FALSE,message=FALSE, warning=FALSE----
 # The number of average number of biopsies is then calculated and
 # compared to the average Prague C score so that those who are taking
 # too few biopsies can be determined
#Lets just use the Surveillance endoscopies:
b1<-b1[grepl("[Ss]urv",b1$Indications),]
b1$NumBx<-HistolNumbOfBx(b1$Macroscopicdescription,'specimen')
b1$BxSize <- HistolBxSize(b1$Macroscopicdescription)
b2<-BarrettsBxQual(b1,'Date.x','HospitalNumber',
                                    'Endoscopist')

## ----exampleBarrettsQuality_AnalysisBiopsyNumbertbl, echo = FALSE,message=FALSE, warning=FALSE,echo=FALSE----

#panderOptions('table.split.table', Inf)
panderOptions('table.split.table', Inf)
pander(b2)

