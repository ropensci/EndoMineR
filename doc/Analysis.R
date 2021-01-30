## ----setup, include=FALSE-----------------------------------------------------
library(pander)
library(EndoMineR)
library(dplyr)
knitr::opts_chunk$set(echo = TRUE)


## ----global_options, include=FALSE--------------------------------------------
knitr::opts_chunk$set( warning=FALSE, message=FALSE)

## ----fig.width=12, fig.height=8,fig.align='center',echo=FALSE-----------------
knitr::include_graphics("img/EndoMineR_Surveillance.svg")

## ----exampleSurveillanceTimeByRow, eval = TRUE--------------------------------
em1<-SurveilTimeByRow(Myendo,'HospitalNumber','Dateofprocedure')

## ----exampleSurveillanceTimeByRowtbl, echo = FALSE----------------------------
pander(head(data.frame(em1[2],em1[ncol(em1)]),5))


## ----exampleSurveilLastTest, echo = TRUE--------------------------------------
em3<-SurveilLastTest(Myendo,'HospitalNumber','Dateofprocedure')

## ----exampleSurveilLastTesttbl, echo = FALSE----------------------------------
pander(head(data.frame(em3[2],em3[5]),5))

## ----exampleSurveilFirstTest, echo = TRUE-------------------------------------
em4<-SurveilFirstTest(Myendo,'HospitalNumber','Dateofprocedure')

## ----exampleSurveilFirstTesttbl, echo = FALSE---------------------------------
pander(head(data.frame(em4[2],em4[5]),5))

## ----exampleHowManyTests, echo = TRUE-----------------------------------------
how<-HowManyOverTime(Myendo,'Indications','Dateofprocedure','Surv')

## ----exampleHowManyTeststbl, echo = FALSE-------------------------------------
pander(head(data.frame(how),5))

## ----exampleListLookup, eval = TRUE,echo=FALSE--------------------------------
panderOptions('table.split.table', Inf)
pander(head(data.frame(Myendo[2:3],Myendo[13])))

## ----exampleListLookup2, eval = TRUE------------------------------------------
library(tm)
myNotableWords <- c("barrett", "coeliac")
ListLookup(Myendo,'Findings',myNotableWords)

## ----exampleListLookup3, echo=FALSE-------------------------------------------
#pander::panderOptions('table.split.table', Inf)
#pander(head(tt))

## ----exampleEndoscChopperMeds,echo=TRUE---------------------------------------
#We have to attach the output of EndoscMeds to the original dataframe
MyendoNew<-cbind(EndoscMeds(Myendo$Medications),Myendo)
#Average Fentanyl use by endoscopist:
Mytable<-MetricByEndoscopist(MyendoNew,'Endoscopist','Fent')

## ----exampleEndoscChopperMedstbl,echo=FALSE-----------------------------------

pander(head(data.frame(MyendoNew$Endoscopist,MyendoNew$Fent),10))

## ----exampleSurveySankey, eval = FALSE----------------------------------------
#  #how<-SurveySankey(Myendo,"ProcedurePerformed")

## ----fig.width=12, fig.height=8,fig.align='center',echo=FALSE,out.width = "100%"----
knitr::include_graphics("img/EndoMineR_Sankey.svg")

## ----examplePatientFlow_CircosPlots, eval = FALSE-----------------------------
#  #flow<-PatientFlow_CircosPlots(v,"Date.y","pHospitalNum","ProcedurePerformed")

## ----fig.width=12, fig.height=8,fig.align='center',echo=FALSE,out.width = "60%"----
knitr::include_graphics("img/EndoMineR_Circos.svg")

