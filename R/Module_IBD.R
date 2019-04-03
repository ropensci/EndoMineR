#' #IBD functions
#' 
#' #' IBD scores
#' #'
#' #' This extracts any score that is attributed to Mayo or IBD. FOr the moment it just
#' #' gives what is written on the report
#' #'
#' # @param x the dataframe
#' # @param y the column to extract from
#' # @param stra the start of the boundary to extract
#' # @param strb the end of the boundary to extract
#' # @param t the column name to create
#' # @importFrom stringr str_extract
#' # @keywords Extraction
#' # @export
#' # @examples v<-TheOGDReportFinal
#' #' Myendo<-TheOGDReportFinal
#' #' Myendo$OGDReportWhole<-gsub('2nd Endoscopist:','Second endoscopist:',
#' #' Myendo$OGDReportWhole)
#' #' EndoscTree<-list('Hospital Number:','Patient Name:','General Practitioner:',
#' #' 'Date of procedure:','Endoscopist:','Second Endoscopist:','Medications',
#' #' 'Instrument','Extent of Exam:','Indications:','Procedure Performed:',
#' #' 'Findings:','Endoscopic Diagnosis:')
#' #' for(i in 1:(length(EndoscTree)-1)) {
#' #'  Myendo<-Extractor2(Myendo,'OGDReportWhole',as.character(EndoscTree[i]),
#' #'  as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
#' #' }
#' #' res<-Myendo
#' 
#' 
#' IBD_mayoAndUCEIS <- function(x, y, stra, strb, t) {
#'   x <- data.frame(x)
#'   t <- gsub("[^[:alnum:],]", " ", t)
#'   
#'   t <- gsub(" ", "", t, fixed = TRUE)
#'   
#'   
#'   x[, t] <- stringr::str_extract(x[, y], stringr::regex(paste(stra,
#'                                                               "(.*)", strb, sep = ""), dotall = TRUE))
#'   x[, t] <- gsub("\\\\.*", "", x[, t])
#'   
#'   names(x[, t]) <- gsub(".", "", names(x[, t]), fixed = TRUE)
#'   x[, t] <- gsub("       ", "", x[, t])
#'   x[, t] <- gsub(stra, "", x[, t], fixed = TRUE)
#'   if (strb != "") {
#'     x[, t] <- gsub(strb, "", x[, t], fixed = TRUE)
#'   }
#'   x[, t] <- gsub("       ", "", x[, t])
#'   x[, t]<- ColumnCleanUp(x[, t])
#'   
#'   
#'   return(x)
#' }
#' 
#' 
#' ################################### Sandbox ############################################################################################################################################ 
#' 
#' #Sandbox:
#' 
#' 
#' library(readr)
#' #library(EndoMineR)
#' AnonKCHendo6m <- read_csv("~/GenDev/DevFiles/IBD/data/AnonKCHendo6m.csv")
#' 
#' #Data prep
#' AnonKCHendo6m<-data.frame(AnonKCHendo6m)
#' 
#' AnonKCHendo6m<-lapply(AnonKCHendo6m, function(x) textPrep(x))
#' AnonKCHendo6m$ER_PROCEDUREPERFORMED<-textPrep(AnonKCHendo6m,"ER_PROCEDUREPERFORMED")
#' AnonKCHendo6m$PROCNAME<-textPrep(AnonKCHendo6m,"PROCNAME")
#' AnonKCHendo6m$INDICATIONS<-textPrep(AnonKCHendo6m,"INDICATIONS")
#' AnonKCHendo6m$ER_EXTENTOFEXAM<-textPrep(AnonKCHendo6m,"ER_EXTENTOFEXAM")
#' AnonKCHendo6m$ER_FINDINGS_STR<-textPrep(AnonKCHendo6m,"ER_FINDINGS_STR")
#' AnonKCHendo6m$ER_DIAGNOSIS_STR<-textPrep(AnonKCHendo6m,"ER_DIAGNOSIS_STR")
#' AnonKCHendo6m$ER_RECOMMENDATIONS<-textPrep(AnonKCHendo6m,"ER_RECOMMENDATIONS")
#' AnonKCHendo6m$ER_COMMENT<-textPrep(AnonKCHendo6m,"ER_COMMENT")
#' 
#' #Get the Mayo score where available:
#' 
#' #If present in a sentence, then extract it:
#' #To do - look in two fields:
#' AnonKCHendo6m$Mayo <-   
#'   #If the CStage is present then extract it
#'   ifelse(grepl("([Mm]ayo(\\s|=)*\\d+)",AnonKCHendo6m$ER_FINDINGS_STR),
#'          stringr::str_replace(stringr::str_extract(AnonKCHendo6m$ER_FINDINGS_STR,'([Mm]ayo(\\s|=)*\\d+)'),"Mmayo", ""),
#'          "No Mayo score")
#' 
#' #To do -extract the UCEIS if V B and U present:
#' AnonKCHendo6m$UCEIS <-   
#'   #If the CStage is present then extract it
#'   ifelse(grepl("UCEIS.*?[A-Za-z]{2,14}",AnonKCHendo6m$ER_FINDINGS_STR),
#'          stringr::str_replace(stringr::str_extract(AnonKCHendo6m$ER_FINDINGS_STR,'UCEIS.*?[A-Za-z]{2,14}'),"UCEIS", ""),
#'          "No UCEIS score")
#' 
#' # 1. Level of training of senior endoscopist: consultant/junior/nurse endoscopist YES
#' # 3.    ·         Specific indication for procedure YES
#' # 7.    ·         Adequate description of degree of endoscopic disease activity in UC using Mayo endoscopic score YES
#' # 8.    ·         Adequate description of degree of endoscopic disease activity in UC using UCEIS endoscopy score YES
#' # 
#' # 
#' # EXAMPLE DATASET NEEDED
#' # 2.    ·         General description of previous disease extent (anywhere on the report) – NEED EXAMPLE DATASET
#' # 10.    ·         Overall impression/summary of findings is provided NEED EXAMPLE DATASET
#' # 4.    ·         Specific IBD therapy at the time of procedure (medication and route (unless obvious) listed but no need for dosage/interval) NEED EXAMPLE DATASET
#' # 5.    ·         Description of UC related symptoms at the time of current evaluation included in the report (consider ‘yes’ if any of the following documented: diarrhoea, nocturnal bowel opening, urgency, abdominal pain, PR bleeding, fatigue, extraintestinal features of IBD, or a statement describing their absence e.g. ‘asymptomatic’) NEED EXAMPLE DATASET
#' # 
#' # 
#' # BITERM ANALYSIS
#' # 6.    ·         Description of maximal extent of endoscopically visible disease BITERM ANALYSIS 
#' # 9.    ·         Description of segmental location of biopsies obtained BITERM ANALYSIS
#' # 
#' # TEMPORAL POS TAGGING
#' # 11.    ·         Any statement describing recommendations for treatment alteration/continuation TEMPORAL POS TAGGING
#' # 12.    ·         Instructions on when and how to follow-up  ?FROM FOLLOW-UP ONLY?
#' #   
#' #   MISSING
#' # 13.    ·         Speciality of senior endoscopist: Physician/Surgeon NO DETAILS FROM THIS REPORT
#' # 
#' # 
#' # 
#' # 
#' # 
#' # ############################################################################################################################################################################## 
#' # 
#' # 
#' # 
#' # 
#' # 
#' # 