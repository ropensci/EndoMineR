## ----setup, include=FALSE-----------------------------------------------------
library(pander)
library(EndoMineR)
knitr::opts_chunk$set(echo = TRUE)


## ----global_options, include=FALSE--------------------------------------------
knitr::opts_chunk$set( warning=FALSE, message=FALSE)

## ----exampleGRS---------------------------------------------------------------
  data(vColon)
  nn<-GRS_Type_Assess_By_Unit(
  vColon, "ProcedurePerformed",
  "Endoscopist", "Diagnosis", "Histology")

## ----exampleGRStbl,echo=FALSE-------------------------------------------------
   pander(nn)

## ----exampleGRSGraph,warning=FALSE,message=FALSE------------------------------
EndoBasicGraph(nn, "Endoscopist", "Adenocarcinoma")

