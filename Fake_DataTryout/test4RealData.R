library(EndoMineR)
library(stringr)
library(randomNames)
library(generator)
library(dplyr)
library(readxl)
library(lattice)




######Full report Endofaker Upper GI test ##################################### 



Mypath<-pathRep2()
Myendo<-EndoRaw2()
v<-Endomerge2(Mypath,Myendo)
names(v)<-c("Endo_ResultText","Endo_ResultPerformed","HospNumId","Histo_ResultText","Histo_ResultPerformed","HospNumId2","Days")

#CleanUp Functions###

##Endoscopy tests

EndoscTree<-list("Hospital Number:","Patient Name:","General Practitioner:","Date of procedure:","Endoscopist:","Endoscopist:","Medications","Instrument","Extent of Exam:","Indications:","Procedure Performed:","Findings:","Endoscopic Diagnosis:")

v<-ChopperNewLines(v,"Endo_ResultText")
for(i in 1:(length(EndoscTree)-1)) {
  v<-Extractor(v,"Endo_ResultText",as.character(EndoscTree[i]),as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
}

v$Endo_ResultPerformed<-as.Date(v$Endo_ResultPerformed,format="%d/%m/%Y")
v<-EndoscChopperEndoscopist(v,"Endoscopist")
v$Endoscopist<-gsub("Second","",v$Endoscopist)
v<-EndoscChopperMeds(v,"Medications")
v<-EndoscChopperInstrument(v,"Instrument")
v<-EndoscChopperIndications(v,"Indications")
v<-EndoscChopperProcPerformed(v,"ProcedurePerformed")
v<-EndoscChopperFindings(v,"Findings")


HistolTree<-list("Hospital Number","Patient Name","DOB:","General Practitioner:","Date received:","Clinical Details:","Macroscopic description:","Histology:","Diagnosis:","")
v<-ChopperNewLines(v,"Histo_ResultText")
for(i in 1:(length(HistolTree)-1)) {
  v<-Extractor(v,"Histo_ResultText",as.character(HistolTree[i]),as.character(HistolTree[i+1]),as.character(HistolTree[i]))
}

#Post extraction clean up
v<-HistolChopperAccessionNumber(v,"Histo_ResultText","SP-\\d{2}-\\d{7}")
v<-HistolChopperDx(v,"Diagnosis")
v<-HistolChopperExtrapolDx(v,"Diagnosis")
v<-HistolChopperNumbOfBx(v,"Macroscopicdescription","specimen")
v<-HistolChopperBxSize(v,"Macroscopicdescription")

##EndoMineR functions
em<-SurveillanceTimeByRow(v,"HospitalNumber","Endo_ResultPerformed")
em<-SurveillanceLastToNow(v,"HospitalNumber","Endo_ResultPerformed")
em<-SurveillanceLastTest(v,"HospitalNumber","Endo_ResultPerformed")
em<-SurveillanceFirstTest(v,"HospitalNumber","Endo_ResultPerformed")
em<-SurveillanceTimeByRow(v,"HospitalNumber","Endo_ResultPerformed")
em<-SurveillanceCapacity(v)

HowManyTests(v,"Indications","Endo_ResultPerformed","Surv")

myNotableWords<-c("arrett","oeliac")
tt<-ListLookup(v,"Findings",myNotableWords)
tt


Fent<-MetricByEndoscopist(v,"Endoscopist","Fent")
Midaz<-MetricByEndoscopist(v,"Endoscopist","Midaz")

OverallNumBx<-MetricByEndoscopist(v,"Endoscopist","NumbOfBx")
OverallSizeBx<-MetricByEndoscopist(v, "Endoscopist","BxSize")

####Trying out the Barrett's functions:
####Barrett's specific cleanup

b<-BarrettsDataAccord_Prague(v,"Endo_ResultText")
b<-BarrettsDataAccord_PathStage(b,"Histo_ResultText")
b<-BarrettsDataAccord_Event(b,"Histo_ResultText","ProcedurePerformed","Diagnosis","Findings")
b<-BarrettsDataAccord_FUGroup(b,"Findings")

####

Enroll<-BarrettsPatientTracking_Enrollment_Surveillance(b,"HospitalNumber","Endo_ResultPerformed","Indications")
Rule<-BarrettsPatientTracking_UniqueHospNum(b,"Rule1","HospNumId")
BarrettsQuality_AnalysisDocumentation(b,"Findings")
BarrettsSurveillance_PathDetection(b,"Myplot")

BarrettsQuality_AnalysisBiopsyNumber(b,"Endoscopist","Endo_ResultPerformed","Endoscopist")
BarrettsSurveillanceDDR(b,"Endoscopist","IMorNoIM")




######Full report Endofaker Lower GI test ############################### 
# Now we try out the colonoscopy specific functions:
# For colonoscopy:Create test data set first

MyColonpath<-ColonpathRep()
MyColonendo<-ColonEndoRaw()
v<-Endomerge(MyColonpath,MyColonendo)
names(v)<-c("Endo_ResultText","Endo_ResultPerformed","HospNumId","Histo_ResultText","Histo_ResultPerformed","HospNumId2","Days")

EndoscTree<-list("Hospital Number","Patient Name","General Practitioner","Endoscopist","nd Endoscopist","Medications","Instrument","Extent of Exam","Indications","Procedure Performed","Findings","Endoscopic Diagnosis","")
v<-ChopperNewLines(v,"Endo_ResultText")
for(i in 1:(length(EndoscTree)-1)) {
  v<-Extractor(v,"Endo_ResultText",as.character(EndoscTree[i]),as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
}

#Post extraction clean up
v$Endo_ResultPerformed<-as.Date(v$Endo_ResultPerformed,format="%d/%m/%Y")
v<-EndoscChopperEndoscopist(v,"Endoscopist")
v$Endoscopist<-gsub("Second","",v$Endoscopist)
v<-EndoscChopperMeds(v,"Medications")
v<-EndoscChopperInstrument(v,"Instrument")
v<-EndoscChopperIndications(v,"Indications")
v<-EndoscChopperProcPerformed(v,"ProcedurePerformed")
v<-EndoscChopperFindings(v,"Findings")


Histoltree<-list("Clinical Details","Nature of specimen","Histology","Diagnosis","")
v<-ChopperNewLines(v,"Histo_ResultText")
for(i in 1:(length(Histoltree)-1)) {
  v<-Extractor(v,"Histo_ResultText",as.character(Histoltree[i]),as.character(Histoltree[i+1]),gsub(" ","",as.character(Histoltree[i])))
}

#Post extraction clean up
v<-HistolChopperAccessionNumber(v,"Histo_ResultText","SP-\\d{2}-\\d{7}")
v<-HistolChopperDx(v,"Diagnosis")
v<-HistolChopperExtrapolDx(v,"Diagnosis")
v<-HistolChopperNumbOfBx(v,"Natureofspecimen","specimen")
v<-HistolChopperBxSize(v,"Natureofspecimen")


GRSTable<-GRS_Type_Assess_By_Unit(v,"ProcedurePerformed", "Endoscopist","Diagnosis","Histology")




