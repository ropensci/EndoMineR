############## Endoscopy Clean-up functions##############


#' Clean endoscopist column
#'
#' If an endoscopist column is part of the dataset once the extractor
#' function has been used this cleans the endoscopist column from the report.
#' It gets rid of titles
#' It gets rid of common entries that are not needed.
#' It should be used after the textPrep function
#'
#' @param EndoscopistColumn The endoscopy text column
#' @keywords Endoscopist extraction
#' @export
#' @return This returns a character vector
#' @importFrom stringr str_replace
#' @family Endoscopy specific cleaning functions
#' @examples
#' Myendo$Endoscopist <- EndoscEndoscopist(Myendo$Endoscopist)
EndoscEndoscopist <- function(EndoscopistColumn) {
  # Extraction of the Endoscopist
  EndoscopistColumn <- str_replace_all(EndoscopistColumn, "mr|professor|prof| dr|2nd.*|[^[:alnum:],]", " ")
  # Put gaps between names
  EndoscopistColumn <- str_replace(EndoscopistColumn, "([a-z])([A-Z])", "\\1 \\2")
  # Trim the whitespace
  EndoscopistColumn <- trimws(EndoscopistColumn, which = c("both"))
  return(EndoscopistColumn)
}




#' Clean instrument column
#'
#' This cleans the Instument column from the report assuming such a column exists
#' (where instrument usually refers to the endoscope number being used.)
#' It gets rid of common entries that are not needed.
#' It should be used after the textPrep function.
#' Note this is possibly going to be deprecated in the next version 
#' as the endoscope coding used here is not widely used.
#' @param EndoInstrument column of interest
#' @keywords Instrument
#' @return This returns a character vector
#' @importFrom stringr str_replace
#' @export
#' @family Endoscopy specific cleaning functions
#' @examples
#' Myendo$Instrument <- EndoscInstrument(Myendo$Instrument)
EndoscInstrument <- function(EndoInstrument) {
  # Extraction of the Instrument used:
  EndoInstrument <- trimws(toupper(str_replace_all(
    EndoInstrument,
    "X.*[Ll][Oo][Aa][Nn] [Ss][Cc][Oo][Pp][Ee] \\(|
                                                  Loan Scope \\(specify serial no:|
                                                  Loan Scope \\(specify\\s*serial no|\\)|-.*|,.*|
                                                  :|FC |[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee] |^,|
                                                  [Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee]\\(specify serial no\\)\\s*|
                                                  [Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee]\\(specify serial no:\\)\\s*", ""
  )))
  EndoInstrument <- str_replace(EndoInstrument, "FC ", "FC")
  EndoInstrument <- str_replace(EndoInstrument, "^\\s*([1-9])", "A\\1")
  return(EndoInstrument)
}

############################# Extrapolating Endoscopy ################################


#' Clean medication column
#'
#' This cleans medication column from the report assuming such a column exists.
#' It gets rid of common entries that are not needed. It also splits the
#' medication into fentanyl and midazolam numeric doses for use. 
#' It should be used after the textPrep function.
#' @param MedColumn column of interest as a string vector
#' @keywords Endoscopy medications
#' @return This returns a dataframe
#' @importFrom stringr str_extract str_replace
#' @export
#' @family Endoscopy specific cleaning functions
#' @examples
#' MyendoNew <- cbind(EndoscMeds(Myendo$Medications), Myendo)
EndoscMeds <- function(MedColumn) {
  # Extraction of the Medications: Extract the fentanyl if present

  dataframe <- data.frame(MedColumn, stringsAsFactors = FALSE)

  dataframe$Fent <-
    str_extract(dataframe$MedColumn, "[Ff]entanyl\\s*(\\d*(\\.\\d+)?)\\s*mcg")
  dataframe$Fent <- as.numeric(str_replace_all(dataframe$Fent, "[Ff]entanyl|mcg", ""))

  # Extract the midazolam if present

  dataframe$Midaz <-
    str_extract(dataframe$MedColumn, "[Mm]idazolam\\s*(\\d*(\\.\\d+)?)\\s*mg")
  dataframe$Midaz <- as.numeric(str_replace_all(dataframe$Midaz, "[Mm]idazolam|mg", ""))


  # Extract the pethidine if present
  dataframe$Peth <-
    str_extract(dataframe$MedColumn, "[Pp]ethidine\\s*(\\d*(\\.\\d+)?)\\s*mcg")
  dataframe$Peth <- as.numeric(str_replace_all(dataframe$Peth, "[Pp]ethidine|mcg", ""))


  # Extract the propofol if present
  dataframe$Prop <-
    str_extract(dataframe$MedColumn, "[Pp]ropofol\\s*(\\d*(\\.\\d+)?)\\s*mcg")
  dataframe$Prop <- as.numeric(str_replace_all(dataframe$Prop, "[Pp]ropofol|mcg ", ""))

  # Drop the first column to avoid repetition

  return(dataframe)
}


#' Extract the endoscopic event.
#'
#' This extracts the endoscopic event. It looks for the event 
#' term and then looks in the event sentence as well as the one above to see if
#' the location is listed. It only looks within the endoscopy fields. If tissue is taken
#' then this will be extracted with the HistolTypeAndSite function rather than being 
#' listed as a result as this is cleaner and more robust.
#' @keywords Find and replace
#' @param dataframe datafrane of interest
#' @param EventColumn1 The relevant endoscopt free text column describing the findings
#' @param Procedure Column saying which procedure was performed
#' @param Macroscopic Column describing all the macroscopic specimens
#' @param Histology Column with free text histology (usually microscopic histology)
#' @return This returns a character vector
#' @export
#' @family Endoscopy specific cleaning functions
#' @examples
#' # Myendo$EndoscopyEvent<-EndoscopyEvent(Myendo,"Findings",
#' # "ProcedurePerformed","MACROSCOPICALDESCRIPTION","HISTOLOGY")
EndoscopyEvent <- function(dataframe, EventColumn1, Procedure, Macroscopic, Histology) {
  dataframe <- data.frame(dataframe, stringsAsFactors = FALSE)

  # Extract the events from the
  output <- EntityPairs_TwoSentence(dataframe[, EventColumn1], EventList(), LocationList())

  #MyHistolEvents <- HistolTypeAndSite(dataframe[, Histology], dataframe[, Macroscopic], dataframe[, Procedure])
  output <- unlist(lapply(output, function(x) paste(x, collapse = ";")))
  #MyHistolEventsdf<-data.frame(MyHistolEvents[1],MyHistolEvents[2],stringsAsFactors = FALSE)
  
  #hiya %>%   
  #  mutate(MyEvent = case_when(
  #    grepl("emr", MyHistolEvents[1], ignore.case = TRUE) ~ "hi",
  #    TRUE ~ "SomethingElse"
  #  ))

  # Add emr only if this is seen in the histopath & remove EMR from events if not seen in histopath

  # If emr is in the histology and in the event then leave it
  # output <- ifelse(grepl("emr", MyHistolEvents, ignore.case = TRUE) & grepl("(oesophagus|goj|stomach|gastric|antrum|cardia|pylorus):emr", output, ignore.case = TRUE), output,
  #   # If emr is in the histology but not in the event then dont add it (sometimes EMR written in the request as past therapy
  #   # but  it hasn't actually been done)
  #   ifelse(grepl("emr", MyHistolEvents, ignore.case = TRUE) & !grepl("emr", output, ignore.case = TRUE), gsub("[A-Za-z]+:emr", "", output),
  #     # If emr is not in the histology but is in the event then remove from the event, otherwise leave as output
  #     ifelse(!grepl("emr|nodul", MyHistolEvents, ignore.case = TRUE) & grepl("emr", output, ignore.case = TRUE), gsub("[A-Za-z]+:emr", "", output), output)
  #   )
  # )



  d <- lapply(output, function(x) strsplit(x, ";"))
  t <- lapply(d, function(x) unlist(x))
  out <- lapply(t, function(x) unique(x))
  output <- unlist(lapply(out, function(x) paste(x, collapse = ";")))
  # output<-unlist(output)
  # Need to know if emr done here so can add it
  return(output)
}



############################# Extrapolating Histology ################################


#' Extract the number of biopsies taken from the histology report
#'
#' This extracts the number of biopsies taken from the pathology report.
#' This is usually from the Macroscopic description column.
#' It collects everything from the regex [0-9]{1,2}.{0,3}
#' to whatever the string boundary is (z).
#'
#' @param inputString The input text to process
#' @param regString The keyword to remove and to stop at in the regex
#' @importFrom stringr str_match_all str_replace_all
#' @keywords Biopsy number
#' @export
#' @family Histology specific cleaning functions
#' @examples
#' qq <- HistolNumbOfBx(Mypath$Macroscopicdescription, "specimen")
HistolNumbOfBx <- function(inputString, regString) {
  inputString <- DictionaryInPlaceReplace(inputString, WordsToNumbers())
  mylist <-
    # I need to collapse the unlist
    stringr::str_match_all(
      inputString,
      paste(unlist(lapply(
        strsplit(regString, "\\|", fixed = FALSE),
        function(x) {
          paste("[0-9]{1,2}.{0,3}", x, sep = "")
        }
      )), collapse = "|")
    )
  NumbOfBx <-
    vapply(mylist, function(p)
      sum(as.numeric(stringr::str_replace_all(p, regString, ""))), numeric(1))
  return(NumbOfBx)
}


#' Determine the largest biopsy size from the histology report
#'
#' This extracts the biopsy size from the report. If there are multiple
#' biopsies it will extract the overall size of each one (size is calculated
#' usually in cubic mm from the three dimensions provided). This will result
#' in row duplication.
#'
#' This is usually from the Macroscopic description column.
#' @param MacroColumn Macdescrip
#' @importFrom stringr  str_match str_replace
#' @keywords biopsy size
#' @export
#' @family Histology specific cleaning functions
#' @examples
#' rr <- HistolBxSize(Mypath$Macroscopicdescription)
HistolBxSize <- function(MacroColumn) {
  # What's the average biopsy size this month?
  BxSize <- str_extract(MacroColumn, "the largest.*?mm")
  BxSize <- str_replace(BxSize, "the largest measuring |mm|less than", "")
  strBxSize <- "([0-9]+).*?([0-9])+.*?([0-9])"
  BxSize <-
    as.numeric(str_match(BxSize, strBxSize)[, 2]) *
      as.numeric(str_match(BxSize, strBxSize)[, 3]) *
      as.numeric(str_match(BxSize, strBxSize)[, 4])
  return(BxSize)
}




#' Extract the site a specimen was removed from as well as the type
#'
#' This needs some blurb to be written. Used in the OPCS4 coding
#' @keywords Find and replace
#' @param inputString1 The first column to look in
#' @param inputString2 The second column to look in
#' @param procedureString The column with the procedure in it
#' @importFrom stringr str_remove_all str_replace_all
#' @export
#' @family Histology specific cleaning functions
#' @return a list with two columns, one is the type and site and the other
#' is the index to be used for OPCS4 coding later if needed.
#' @examples
#'Myendo2<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',
#'Mypath,'Dateofprocedure','HospitalNumber')
#'PathSiteAndType <- HistolTypeAndSite(Myendo2$PathReportWhole,
#'Myendo2$Macroscopicdescription, Myendo2$ProcedurePerformed)



HistolTypeAndSite<-function(inputString1,inputString2,procedureString){
  
  output<-ifelse(EntityPairs_OneSentence(inputString1,HistolType(),LocationList())=="NA:NA", 
                 EntityPairs_OneSentence(inputString2,HistolType(),LocationList()),
                 EntityPairs_OneSentence(inputString1,HistolType(),LocationList()))
  
  #If there is only a colon (punctuation) and then empty then assume it is a biopsy
  output<-str_replace_all(output, ":,|:$", ":biopsy,")
  
  #Make sure only unique values represented:
  output<-lapply(output, function(x) paste(unlist(unique(unlist(strsplit(x,",")))),collapse=","))
  
  output<-ifelse(grepl("Gastroscopy",procedureString,ignore.case = TRUE),
                 str_remove_all(output, paste0('(',tolower( paste0(unlist(LocationListLower(),use.names=F),collapse="|")),')',':biopsy')),
                 ifelse(grepl("Colonoscopy|Flexi",procedureString,ignore.case = TRUE),
                        str_remove_all(output, paste0('(',tolower(paste0(unlist(LocationListUpper(),use.names=F),collapse="|")),')',':biopsy')),output))
  
  output<-unlist(output)
  
  biopsyIndexresults<-ExtrapolatefromDictionary(output,BiopsyIndex())
  
  output<-list(HistolTypeAndSite=gsub("NA:NA","",output),BiopsyIndex=biopsyIndexresults)
  
  return(output)
}

#' OPCS-4 Coding 
#'
#' This function extracts the OPCS-4 codes for all Barrett's procedures
#' It should take the OPCS-4 from the EVENT and perhaps also using extent
#' depending on how the coding is done. The EVENT column will need to 
#' extract multiple findings
#' The hope is that the OPCS-4 column will then map from the EVENT column. This returns a nested list 
#' column with the procedure, furthest path site and event performed 
#' 
#'
#' @param dataframe the dataframe
#' @param Event the EVENT column
#' @param Procedure The Procedure column
#' @param PathSite The column containing the Pathology site
#' @keywords OPCS-4 codes extraction
#' @importFrom dplyr mutate case_when
#' @importFrom rlang sym
#' @importFrom stringr str_detect
#' @export
#' @examples # Need to run the HistolTypeSite and EndoscopyEvent functions first here
#' # SelfOGD_Dunn$OPCS4w<-ExtrapolateOPCS4Prep(SelfOGD_Dunn,"PROCEDUREPERFORMED",
#' # "PathSite","EndoscopyEvent")

# #####################################################   Sandbox    ##################################################################################################################
library(readxl) 
library(stringr)
library(dplyr)
SelfOGD_Dunn<-read_excel("/home/rstudio/GenDev/DevFiles/EndoMineRFunctionDev/SelfOGD_Dunn.xlsx")
mywords<-c("PatientID","Patient Name","Date of Birth","General Practicioner","Hospital Number","Date of Procedure","Endoscopist","Second endoscopist","Trainee","Referring Physician","Nurses","Medications","Instrument","Extent of Exam","Complications","Co-morbidity","INDICATIONS FOR EXAMINATION","PROCEDURE PERFORMED","FINDINGS","ENDOSCOPIC DIAGNOSIS","RECOMMENDATIONS","COMMENTS","FOLLOW UP","OPCS4 Code","NATURE OF SPECIMEN","CLINICAL DETAILS","MACROSCOPICAL DESCRIPTION","HISTOLOGY","DIAGNOSIS")
SelfOGD_Dunn2<-data.frame(paste(SelfOGD_Dunn$PatientID,",",SelfOGD_Dunn$Endo_ResultText,",",SelfOGD_Dunn$Histo_ResultText),stringsAsFactors = FALSE)
SelfOGD_Dunn2<-EndoMineR::textPrep(SelfOGD_Dunn2[,1],mywords)
PathSite<-HistolTypeAndSite(SelfOGD_Dunn2$histology,SelfOGD_Dunn2$macroscopicaldescription,SelfOGD_Dunn2$procedureperformed)
SelfOGD_Dunn2$PathSite<-unlist(PathSite[1])
SelfOGD_Dunn2$PathSiteIndex<-unlist(PathSite[2])
SelfOGD_Dunn2$EndoscopyEvent<-EndoscopyEvent(SelfOGD_Dunn2,"findings","procedureperformed","macroscopicaldescription","histology")

SelfOGD_Dunn3<-dev_ExtrapolateOPCS4Prep(SelfOGD_Dunn2,"procedureperformed","PathSiteIndex","EndoscopyEvent","extentofexam")




#Checking against actual coding data
 ManualOPCS_4<-read_excel("/home/rstudio/GenDev/DevFiles/EndoMineRFunctionDev/TB_ALLPATID_Dunn_2013ToPresent.xls")
 library(janitor)
 selectedClean<-ManualOPCS_4%>%select("Prim Proc Code & Description","2nd Proc Code","3rd Proc Code","4th Proc Code","Trust ID",
                                      "Consultant","Admission Date","Prim Diag Code & Description",
                                      "2nd Diagnosis Code",
                                      "3rd Diagnosis Code",
                                      "5th Diagnosis Code",
                                      "6th Diagnosis Code",
                                      "7th Diagnosis Code",
                                      "8th Diagnosis Code",
                                      "9th Diagnosis Code",
                                      "10th Diagnosis Code")
 selectedClean<-clean_names(selectedClean,"snake")
 names(selectedClean) <- gsub('admission_date', 'Endo_ResultEntered', names(selectedClean))
 names(selectedClean) <- gsub('trust_id', 'PatientID', names(selectedClean))

 #convert the date and hospital number column names so can do a merge
 SelfOGD_Dunn3$Endo_ResultEntered<-as.Date(str_extract(SelfOGD_Dunn2$dateofprocedure,"\\d+.*\\d+"),"%d/%m/%Y")
 selectedClean$Endo_ResultEntered<-as.Date(selectedClean$Endo_ResultEntered)
 SelfOGD_Dunn3$PatientID<-str_extract(SelfOGD_Dunn3$Start,"[a-z]*\\d+[a-z]*")
 selectedClean$PatientID<-tolower(selectedClean$PatientID)
 mergedData <- merge(selectedClean,SelfOGD_Dunn3,by=c("Endo_ResultEntered","PatientID"))
 
 ForRules<-mergedData%>%filter(grepl("gastroscopy",procedureperformed))%>%select(PatientID,extentofexam,
                                                                                 macroscopicaldescription,
                                                                                 histology,procedureperformed,
                                                                                 findings,EndoscopyEvent,
                                                                                 SecondaryCodes,prim_proc_code_description,
                                                                                 x2nd_proc_code,x3rd_proc_code,x4th_proc_code)%>%sample_n(100)
 View(ForRules)
 write_xlsx(ForRules, "/home/rstudio/EndoscopyEventToValidate.xlsx")
 
 
 
######################################################################################################################################################################################################
#For each event site:
dev_ExtrapolateOPCS4Prep <- function(dataframe, Procedure,PathSite,Event,extent) {  
  dataframe<-data.frame(dataframe,stringsAsFactors=FALSE)
  
  #Clean up the PathSite:
  dataframe$PathSite<-str_replace_all(dataframe$PathSite,",+",";")
  dataframe$PathSite<-str_replace_all(dataframe$PathSite,"NA:NA","")
  dataframe$EndoscopyEvent<-paste0(dataframe$PathSite,";",dataframe$EndoscopyEvent)
  
  #For later processing for the 2nd and 3rd procedure codes:
  SelfOGD_Dunn2$EndoscopyEventRaw<-SelfOGD_Dunn2$EndoscopyEvent

  #For the primary codes:
  dataframe$EndoscopyEvent<-gsub("(Oesophagus|GOJ|cardia|stomach|duodenum|pylorus|antrum|NA):(apc|grasp)",";G432  -  Fibreoptic endoscopic laser destruction of lesion of upper gastrointestinal tract",dataframe$EndoscopyEvent,ignore.case = TRUE)
  dataframe$EndoscopyEvent<-gsub("(Oesophagus|GOJ|cardia|stomach|duodenum|pylorus|antrum|NA):polypectomy",";G431  -  Fibreoptic endoscopic snare resection of lesion of upper gastrointestinal tract",dataframe$EndoscopyEvent,ignore.case = TRUE) 
  dataframe$EndoscopyEvent<-gsub("(Oesophagus|GOJ|cardia|stomach|duodenum|pylorus|antrum|NA):rfa",";G435  -  Fibreoptic endoscopic destruction of lesion of upper gastrointestinal tract NEC",dataframe$EndoscopyEvent,ignore.case = TRUE)
  dataframe$EndoscopyEvent<-gsub("(Oesophagus|GOJ|cardia|stomach|duodenum|pylorus|antrum|NA):(emr|esd|resection)",";G421  -  Fibreoptic endoscopic submucosal resection of lesion of upper gastrointestinal tract",dataframe$EndoscopyEvent,ignore.case = TRUE)
  dataframe$EndoscopyEvent<-gsub("(Oesophagus|GOJ|cardia|stomach|duodenum|pylorus|antrum|NA):dilat", ";G443  -  Fibreoptic endoscopic dilation of upper gastrointestinal tract NEC",dataframe$EndoscopyEvent,ignore.case = TRUE)

  dataframe$EndoscopyEvent<-gsub("APC",";G432  -  Fibreoptic endoscopic laser destruction of lesion of upper gastrointestinal tract",dataframe$EndoscopyEvent,ignore.case = TRUE) 
  dataframe$EndoscopyEvent<-gsub("Polypectomy",";G431  -  Fibreoptic endoscopic snare resection of lesion of upper gastrointestinal tract",dataframe$EndoscopyEvent,ignore.case= TRUE)
  dataframe$EndoscopyEvent<-gsub("RFA",";G435  -  Fibreoptic endoscopic destruction of lesion of upper gastrointestinal tract NEC",dataframe$EndoscopyEvent,ignore.case = TRUE)
  dataframe$EndoscopyEvent<-gsub("EMR|ESD",";G421  -  Fibreoptic endoscopic submucosal resection of lesion of upper gastrointestinal tract",dataframe$EndoscopyEvent,ignore.case = TRUE)
  dataframe$EndoscopyEvent<-gsub("Dilatation",";G443  -  Fibreoptic endoscopic dilation of upper gastrointestinal tract NEC",dataframe$EndoscopyEvent,ignore.case = TRUE)
  
  dataframe$EndoscopyEvent<-stringr::str_replace_all(dataframe$EndoscopyEvent,regex("(Oesophagus|GOJ|cardia|stomach|duodenum|pylorus|antrum|NA):(biopsy|nac|acetic|iodine)", ignore_case = TRUE),
                                                     ";G451  -  Fibreoptic endoscopic exam of upper gastrointestinal tract and biopsy of lesion of upper GI tract")
  
  #Cleanup so that empty strings dont have stray punctuation:
  dataframe$EndoscopyEvent<-str_replace_all(trimws(dataframe$EndoscopyEvent),"^;+","")
  
  dataframe$PathSite<-str_replace_all(dataframe$PathSite,"NA:NA","")
  
  
  dataframe$EndoscopyEvent<-ifelse((grepl("OGD", dataframe$procedureperformed,ignore.case = TRUE)&trimws(dataframe$EndoscopyEvent)==""&(dataframe$PathSite=="")),
                                   ";G459  -  Unspecified diagnostic fibreoptic endoscopic examination of upper gastrointestinal tract",
                                   dataframe$EndoscopyEvent)
  
  #Clean up extra semicolons and commas from the processing above
  x2 <- str_replace_all(trimws(dataframe$EndoscopyEvent),"^;+","")
  x2 <- str_replace_all(trimws(x2),"NA:NA;","")
  x2 <- str_replace_all(trimws(x2),"(;+)|(:;)",";")
  x2 <- str_replace_all(trimws(x2),",+",";")
  #Create nested lists:
  x2 <- strsplit(x2, ";")
  x2<-lapply(x2, function(y) unique(y))
  x2<-lapply(x2, function(y) paste(y, collapse = ";"))
  
  dataframe$EndoscopyEvent<-unlist(x2)
  
  #For the secondary codes:
  dataframe$MAXOFPATHSITE<-stringr::str_extract_all(dataframe$PathSiteIndex,"\\d")
  dataframe$MAXOFPATHSITE<-lapply(dataframe$MAXOFPATHSITE,function(x) max(as.numeric(x)))
  
  dataframe<-dataframe %>%   
    mutate(OPCS4ZCode = case_when( 
      (dataframe$PathSiteIndex==""|is.na(dataframe$PathSiteIndex)|dataframe$PathSiteIndex=="NA")~ case_when(
        #1. if no biopsy and no Event (covers oesophageal and non-oesophageal), then give the extent reached
          grepl("duodenum",tolower(extentofexam))~  "Z274",
          grepl("pylorus",tolower(extentofexam))~  "Z273",
          grepl("stomach|antrum|angularis",tolower(extentofexam))~  "Z272",
          grepl("oesophagus",tolower(extentofexam))~  "Z271",
        ),
       
      
      #2.If event (oesophageal) and biopsy 
      (dataframe$PathSite!=""|is.na(dataframe$PathSite)) ~ case_when(
        dataframe$MAXOFPATHSITE== 5 ~  "Z274",
        dataframe$MAXOFPATHSITE== 4 ~  "Z273",
        dataframe$MAXOFPATHSITE== 3 ~  "Z272",
        dataframe$MAXOFPATHSITE== 1|2 ~  "Z271",
      ),
      TRUE ~ ""
    )
    ) 
  
  miner<-dataframe %>% 
    mutate(row = row_number()) %>%
    tidyr::separate_rows(EndoscopyEventRaw, sep = ";") %>%
    mutate(x2ndProcedureCodes = case_when(grepl("dilat",EndoscopyEventRaw)~  "Y403",
                                   grepl("apc",EndoscopyEventRaw) ~  "Y171",
                                   grepl("rfa",EndoscopyEventRaw) ~  "Y134",
                                   grepl("grasp",EndoscopyEventRaw) ~  "Y134",
                                   grepl("clip",EndoscopyEventRaw) ~  "Y072",
                                   TRUE ~ "")) %>%
    group_by(row) %>%
    summarise(x2ndProcedureCodes = toString(x2ndProcedureCodes)) %>%
    select(-row)
  
  
  
  miner<-data.frame(miner)
  dataframe$x2ndProcedureCodes<-miner$x2ndProcedureCodes
  
  dataframe$SecondaryCodes<-paste(dataframe$x2ndProcedureCodes,dataframe$OPCS4ZCode)
  dataframe$OPCS4ZCode<-NULL
  dataframe$x2ndProcedureCodes<-NULL
  
  return(dataframe)
}
