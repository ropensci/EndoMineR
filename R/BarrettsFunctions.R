if (getRversion() >= "2.15.1")
  utils::globalVariables(
    c(
      "PatientID",
      ".SD",
      "CStage",
      "NumbOfBx",
      "Years",
      "Difference",
      "barplot",
      "head",
      "read.table",
      "eHospitalNum",
      "pHospitalNum",
      ".",
      "n",
      "EVENT",
      "MonthYear",
      "freq",
      "Endoscopist",
      "avg",
      "v",
      "destination",
      "dcast",
      "complete.cases",
      "g",
      "gvisSankey",
      "head",
      "pHospitalNum",
      "par",
      "plot",
      "r",
      "read.table",
      "region",
      "rgb",
      "setDT",
      "ind"
    )
  )
###### Barretts Surveillance and Therapeutic Functions ######

#' Prague score extraction
#'
#' The aim is to extract a C and M stage (Prague score) for Barrett's samples.
#' This is done using a regex where C and M stages are explicitly mentioned in
#' the free text
#' Specfically it extracts the Prague score
#' @param dataframe dataframe with column of interest
#' @param EndoReportColumn column of interest
#' @importFrom stringr str_extract str_replace
#' @keywords  Prague score
#' @export
#' @examples #The example takes the endoscopy demo dataset and searches the
#' # Findings column (which contains endoscopy free text about the
#' # procedure itself). It then extracts the Prague score if relevant. I
#' # find it easiest to use this on a Barrett's subset of data rather than
#' # a dump of all endoscopies but of course this is a permissible dataset
#' # too
#'
#'
#' aa<-Barretts_PragueScore(Myendo,'Findings')

#Change to BarrPrague nee Barretts_PragueScore
Barretts_PragueScore <- function(dataframe, EndoReportColumn) {
  dataframe <- data.frame(dataframe)
  #Do C stage as an ifelse- note that if there is no C stage, then the function
  #will look to see if the total length is reported which is assumed to 
  #be the total C stage but may not be accurate 
  #To prevent the length of something else other than Barrett's being described
  #and misinterpreted as a C stage, the sentence with a length must include Barrett's
  #or columnar lined mucosa. Even this wont be fool proof so act with caution
  dataframe$CStage <-   
    ifelse(grepl("(C(\\s|=)*\\d+)",dataframe[,EndoReportColumn]),str_replace(str_extract(dataframe[,EndoReportColumn],'(C(\\s|=)*\\d+)'),"C", ""),
           ifelse(grepl("\\d{2}\\s*[cm]*\\s*(to|-|and)\\s*\\d{2}\\s*[cm]*\\s*",dataframe[,EndoReportColumn]),
                  as.numeric(sapply(str_extract_all(str_extract(dataframe[,EndoReportColumn],"\\d{2}\\s*[cm]*\\s*(to|-|and)\\s*\\d{2}\\s*[cm]*\\s*"),"\\d{2}"), function(x) abs(diff(as.numeric(x))))),"No"))
  
  #Need to include in the C stage values like 35-40cm and35cm to 40 ## \d+.*?(-|to).*?\d+
  
 #5 cm in length/long etc 
  #dataframe$CStage <- as.numeric(str_replace(dataframe$CStage,"C", ""))
  #dataframe$CStage <- stringr::str_extract_all(dataframe$CStage,"\\d{2}")
  #dataframe$CStage<-as.numeric(sapply(dataframe$CStage, function(x) abs(diff(as.numeric(x)))))

  
  dataframe$MStage <- str_extract(dataframe[, EndoReportColumn],
                                  "(M(\\s|=)*\\d+)")
  dataframe$MStage <- as.numeric(str_replace(dataframe$MStage,"M", ""))
  return(dataframe)
}


#' Worst pathological stage Barrett's
#'
#' This extracts the pathological stage from the histopathology specimen. It is
#' done using 'degradation' so that it will look for the worst overall grade
#' in the histology specimen and if not found it will look for the next worst
#' and so on. It looks per report not per biopsy (it is more common
#' for histopathology reports to contain the worst overall grade
#' rather than individual biopsy grades).
#' Specfically it extracts the histopathology worst grade within the specimen
#' FOr the sake of accuracy this should alwats be used after the HistolDx function
#' and this removes negative sentences such as 'there is no dysplasia'. 
#' This current function should be used on the column derived from HistolDx
#' which is called Dx_Simplified
#' @param dataframe dataframe with column of interest
#' @param PathColumn column of interest
#' @keywords Pathology extraction
#' @export
#' @examples # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data 
#' # cleaning
#' # as part of the package.
#'
#' v<-HistolAll(Mypath)
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' # The function then takes the Histology column from the merged data set (v).
#' # It extracts the worst histological grade for a specimen
#' b<-Barretts_PathStage(v,'Dx_Simplified')
#' rm(v)

Barretts_PathStage <- function(dataframe, PathColumn) {
  # Get the worst pathology for that sample inc SM stages
  dataframe <- data.frame(dataframe)
  dataframe$IMorNoIM <-
    ifelse(grepl("[Ss][Mm]2", dataframe[, PathColumn], perl = TRUE),
           "SM2",
           ifelse(
             grepl("[Ss][Mm]1", dataframe[, PathColumn], perl = TRUE),
             "SM1",
             ifelse(
               grepl("T1b", dataframe[, PathColumn], perl = TRUE),
               "T1b_Unspec",
               ifelse(
                 grepl("T1a|ntramucosal",
                       dataframe[, PathColumn], perl = TRUE),
                 "T1a",
                 ifelse(
                  grepl("[Hh]igh [Gg]rade ", dataframe[, PathColumn], perl = TRUE),
                   "HGD",
                   ifelse(
                    grepl("[Ll]ow [Gg]rade", dataframe[, PathColumn], perl = TRUE),
                     "LGD",
                     ifelse(
                       grepl("[Ii]ndef", dataframe[, PathColumn], perl = TRUE),
                       "IGD",
                       ifelse(
                         grepl(
                           "[Ii]ntestinal [Mm]etaplasia",
                           dataframe[, PathColumn],
                           perl = TRUE
                       ),
                       "IM",
                       ifelse(
                         grepl(
                           "",
                           dataframe[, PathColumn],
                           perl = TRUE
                       ),
                       "No_IM",
                  ifelse(is.na(dataframe[, PathColumn]), "No tissue", "No_IM")
                       )
                       )
                       )
                     )
                 )
               )
             )
               ))
  return(dataframe)
}


#' Therapeutic subtypes
#'
#' This function extracts the Event- usually a therapeutic event, from the text
#' eg endoscopic mucosal resection, radiofrequency ablation etc.
#' It does not currently include stricture
#' dilatation.Specfically it extracts the event
#'
#' @param dataframe the dataframe
#' @param HistolReportColumn The histology main text
#' @param ProcPerformedColumn The Procedure Performed column
#' @param EndoReportColumn The endoscopic diagnosis column
#' @param EndoFindingsColumn The endoscopic findings column if different 
#' to the Diagnosis column
#' @keywords Event extraction
#' @export
#' @examples # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#'
#' v<-HistolAccessionNumber(Mypath,'Histology',
#' 'SP-\\d{2}-\\d{7}')
#' v<-HistolDx(v,'Diagnosis')
#' v<-HistolExtrapolDx(v,'Diagnosis',"")
#' v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
#' v<-HistolBxSize(v,'Macroscopicdescription')
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' # The function then looks within the Histology and the
#' # Procedure performed, free text endoscopic Findings and the original
#' # whole endoscopy report columns from the merged data set (v) for
#' # EMR/APC/RFA/HALO so that the event for the procedure, is recorded.
#' bb<-Barretts_EventType(v,'Histology',
#' 'ProcedurePerformed','Indications','Findings')
#' rm(v)
#'
Barretts_EventType <- function(dataframe, HistolReportColumn, 
                  ProcPerformedColumn, EndoReportColumn, EndoFindingsColumn) {
  dataframe <- data.frame(dataframe)
  # Get all the EVENTS in:
  dataframe$EVENT <-
    ifelse(grepl("[Ee][Mm][Rr]", dataframe[, HistolReportColumn], perl = TRUE),
           "EMR",
           ifelse(
             grepl("[Ee]ndoscopic [Mm]ucosal [Rr]esection",
                   dataframe[, HistolReportColumn], perl = TRUE),
             "EMR",
             ifelse(
            grepl("ndomucosal", dataframe[, HistolReportColumn], perl = TRUE),
               "EMR",
               ifelse(
              grepl("HALO|RFA", dataframe[, ProcPerformedColumn], perl = TRUE),
                 "RFA",
                 ifelse(
                  grepl("APC", dataframe[, ProcPerformedColumn], perl = TRUE),
                   "APC",
                   ifelse(
                grepl("HALO|RFA", dataframe[, EndoReportColumn], perl = TRUE),
                     "RFA",
                     ifelse(
                       grepl("APC", dataframe[, EndoReportColumn], perl = TRUE),
                       "RFA",
                       ifelse(
                grepl("HALO|RFA", dataframe[, EndoFindingsColumn], perl = TRUE),
                         "RFA",
      ifelse(grepl("APC", dataframe[, EndoFindingsColumn], perl = TRUE), "APC",
                                "nothing")
                       )
                     )
                   )
                 )
               )
             )
           ))
  return(dataframe)
}

#' Follow up group determination
#'
#' This determines the follow up rule a patient should fit in to (according to
#' the British Society for Gastroenterology guidance on Barrett's oesophagus)
#' Specfically it combines the presence of intestinal metaplasia with
#' Prague score so the follow-up group can be determined. It relies on the
#' presence of a Prague score. It should be run after
#' Barretts_PathStage which looks for the worst stage of a
#' specimen and which will determine the presence or absence of intestinal
#' metaplasia if the sample is non-dysplastic.
#' Being the procedure done at the time and the follow-up timings
#' @param dataframe the dataframe(which has to have been processed by the
#' Barretts_PathStage function first to get IMorNoIM),
#' @param EndoReportColumn The field to search (endoscopic findings)
#' @keywords Follow-Up
#' @importFrom stringr str_extract str_replace
#' @export
#' @examples # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' v<-HistolDx(Mypath,'Diagnosis')
#' v<-HistolExtrapolDx(v,'Diagnosis',"")
#' v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
#' v<-HistolBxSize(v,'Macroscopicdescription')
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' #The function relies on the other Barrett's functions being run as well:
#' b<-Barretts_PathStage(v,'Histology')
#' b2<-Barretts_EventType(b,'Histology',
#' 'ProcedurePerformed','Indications','Findings')
#' #The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' cc<-Barretts_FUType(b2,'Findings')
#' rm(v)

Barretts_FUType <- function(dataframe, EndoReportColumn) {
  dataframe <- data.frame(dataframe)
  # Do the follow-up groupings
  try(dataframe$MStage <-
        ifelse(
          grepl("(C(\\s|=)*\\d+)", dataframe[, EndoReportColumn], perl = TRUE),
          str_extract(dataframe[, EndoReportColumn], "(M(\\s|=)*\\d+)"),
          ifelse(
grepl("[Ss]hort|[Tt]iny|[Tt]ongue|[Ss]mall", 
      dataframe[, EndoReportColumn], perl = TRUE),
            "0",
            ifelse(
              grepl("[1-3]{1}(\\s)*?[cC][mM]",
                    dataframe[, EndoReportColumn], perl = TRUE),
              str_extract(dataframe[, EndoReportColumn], 
                          "[1-3]{1}(\\s)*?[cC][mM]"),
              NA
            )
          )
        ))
  
  dataframe$MStage <- str_replace(dataframe$MStage,"M|cm|=", "")
  
  dataframe$MStage <- as.integer(dataframe$MStage)
  dataframe$FU_Group <-
    ifelse(
      dataframe$IMorNoIM == "No_IM" &
        dataframe$MStage < 3,
      "Rule1",
      ifelse(
        dataframe$IMorNoIM == "IM" &
          dataframe$MStage < 3,
        "Rule2",
        ifelse(dataframe$MStage >= 3, "Rule3", "NoRules")
      )
    )
  return(dataframe)
  
}



#' Follow up group determination
#'
#' This function collects all the Barrett's functions togather as a 
#' parent function. The function checks the column names and performs
#' the necessary subfunction as long as the column is named correctly. 
#' The function follows the Histology function. You can use the same column
#' names as suggested with the parent function HistolAll
#' @param dataframe the dataframe(merged endoscopy and histology).
#' @keywords Barretts
#' @importFrom stringr str_extract str_replace
#' @export
#' @examples  
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',Mypath,'Dateofprocedure',
#' 'HospitalNumber')
#' #The function relies on the other Barrett's functions being run as well:
#' dd<-BarrettsAll(v)
#' rm(v)



BarrettsAll <- function(dataframe) {
  if("Histology" %in% colnames(dataframe)){
    dataframe<-Barretts_PathStage(dataframe,'Histology')
  }
  
  
  if("Findings" %in% colnames(dataframe)){
    dataframe<-Barretts_PragueScore(dataframe,'Findings')
    dataframe<-Barretts_FUType(dataframe,'Findings')
  }
  
  if("Histology" %in% colnames(dataframe)){
    if("ProcedurePerformed" %in% colnames(dataframe)){
      if("Indications" %in% colnames(dataframe)){
        if("Findings" %in% colnames(dataframe)){
          dataframe<-Barretts_EventType(dataframe,'Histology',
                       'ProcedurePerformed','Indications','Findings')
        }
      }
    }
  }
  dataframe<-data.frame(dataframe)
  
  return(dataframe)
  
  }

############## Surveillance functions ########


#' How many Barrett's patients had surveillance
#'
#' This function graphs the patients who were not on surveillance programmes and
#' sees how many then had an endoscopy.This allows us to determine how many
#' index Barrett's detections went on to undergo surveillance.
#' This should be run after the Barretts_PragueScore and
#' Barretts_PathStage.
#' @param dataframe dataframe
#' @param PatientID column of interest with unique hospital number in it
#' @param Endo_ResultPerformed column of interest with date endiscopy performed
#' in it
#' @param IndicationsFroExamination column of interest with indications in it
#' (usually 'Surveillance' or similar)
#' @keywords Patient Tracking
#' @importFrom dplyr group_by slice mutate filter
#' @importFrom magrittr '%>%'
#' @importFrom rlang sym
#' @export
#' @examples #This takes the Myendo demo dataset
#' # and then groups the Barrett's endoscopies by patient (as defined by their
#' # unique hospital identifier and then orders by the date of procedure. It
#' # should look in the Indications column for Barrett's related indication
#' ee<-BarrettsSurveil(Myendo,
#' 'HospitalNumber',
#' 'Dateofprocedure','Indications')

BarrettsSurveil <- function(dataframe,
                                              PatientID,
                                              Endo_ResultPerformed,
                                              IndicationsFroExamination) {
  dataframe <- data.frame(dataframe)
  PatientIDa <- sym(PatientID)
  Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
  IndicationsFroExaminationa <-
    sym(IndicationsFroExamination)
  # So you want all those whose last endoscopy was non surveillance but who
  # have
  # a difftime between now and the last of > 3years So get the last endoscopy
  # for each patient Filter out the endoscopies that were surveillance Get the
  # difftime between now and the last endoscopy
  # Filter for those who have been waiting >3 years post non surveillance
  # endoscopy
  
  t <-
    dataframe %>% arrange(as.Date(!!Endo_ResultPerformeda)) %>%
    group_by(!!PatientIDa) %>%
    slice(n()) %>%
    filter(!grepl("Surv|Barr", !!IndicationsFroExamination)) %>%
    mutate(Years = difftime(
      as.Date(Sys.Date()),
      as.Date(!!Endo_ResultPerformeda),
      units = "weeks"
    ) / 52) %>%
    filter(Years > 3)
  
  t<-data.frame(t)
  
  return(t)
  
}


#' Unique Hospital Numbers of Barrett's patients
#'
#' This function gets the unique patient ID's for each patient,
#' for each rule. It lists the unique PatientIDs assocaited with a rule
#' ('Rule1','Rule2','Rule3','NoRules'). This allows us to determine how many
#' patients will need follow up at specific time intervals.
#' This should be run after the Barretts_PragueScore and
#' Barretts_PathStage.
#' @param dataframe dataframe with column of interest
#' @param rule Rule of interest
#' @param PatientID Column containing patient numbers
#' @keywords Rule
#' @export
#' @examples # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' v<-HistolAccessionNumber(Mypath,'Histology',
#' 'SP-\\d{2}-\\d{7}')
#' v<-HistolDx(v,'Diagnosis')
#' v<-HistolExtrapolDx(v,'Diagnosis',"")
#' v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
#' v<-HistolBxSize(v,'Macroscopicdescription')
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' # The function relies on the other Barrett's functions being run as well:
#' b1<-Barretts_PragueScore(v,'Findings')
#' b2<-Barretts_PathStage(b1,'Histology')
#' b3<-Barretts_EventType(b2,'Histology',
#' 'ProcedurePerformed','Indications','Findings')
#' # The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' b4<-Barretts_FUType(b3,'Findings')
#' colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#' #Finally the unique hospital numbers are obtained according to the follow-up
#' # rule you are looking for
#' ff<-BarrettsSurveil_HospNum(b4,'Rule1','HospitalNumber')
#' rm(v)

BarrettsSurveil_HospNum <- function(dataframe, rule, PatientID) {
  dataframe <- data.frame(dataframe)
  dataframe <- subset(dataframe, dataframe$FU_Group == rule)
  dataframe <- data.frame(unique(dataframe[, PatientID]))
  names(dataframe) <- c("x")
  return(dataframe)
}

######## Endoscopic Performance Quality-#####


#' Analysis of Barrett's Documentation Quality
#'
#' This function assesses the Barrett's documentation. This notes how many
#' reports contain the mandatory report fields as specified in the BSG standards
#'  on Barrett's endoscopic reporting. This should be run after the
#' Barretts_PragueScore.
#' @param dataframe dataframe
#' @param Findings column of interest- usually the main body of the
#' endoscopic report
#' @importFrom ggplot2 geom_bar xlab labs coord_flip theme 
#'element_blank element_line coord_flip
#' @keywords Documentation
#' @export
#' @examples # Firstly relevant columns are extrapolated from the
#' b1<-Barretts_PragueScore(Myendo,'Findings')
#' # The documentation is really from the endoscopic Findings column
#' gg<-BarrettsDocumentQual(b1,'Findings')




BarrettsDocumentQual <- function(dataframe, Findings) {
  dataframe <- data.frame(dataframe)
  # Is Surveillance documentation done properly?
  PragueSubsetx <- subset(dataframe, !is.na(dataframe$MStage))
  
  # Presence of islands
  IslandSubsetx <- dataframe[grepl("[Ii]sland", dataframe[, Findings]), ]
  
  # Hiatus hernia (top of gastric folds)
  HerniaSubsetx <-
    dataframe[grep("[Hh]iat|astric fold|[Pp]inch", dataframe[, Findings]), ]
  
  
  # Visible lesions- should also describe the absence of visible lesions
  #explicitly
  LesionSubsetx <- dataframe[grep("esion|odule|lcer", dataframe[, Findings]), ]
  
  # Classification of lesions On surveillance vs not on surveillance.
  # This one is done as part of the Therapeutic survey so a different dataset.
  
  # Biopsies (location and samples taken) Decided not to do this as no point as
  # all biopsies are labelled at time of pathology so don't see why they should
  # be on the form. On surveillance vs not on surveillance
  
  Proportion <- c(
    as.numeric(nrow(PragueSubsetx) / nrow(dataframe)),
    as.numeric(nrow(IslandSubsetx) / nrow(dataframe)),
    as.numeric(nrow(HerniaSubsetx) / nrow(dataframe)),
    as.numeric(nrow(LesionSubsetx)) / nrow(dataframe)
  )
  b <- c("Prague",
         "Island",
         "Hernia",
         "Lesion")
  EndoMinDataSet <- data.frame(b, Proportion)
  
  
  t <-ggplot(EndoMinDataSet, aes(x=b, y=Proportion)) + 
    geom_bar(stat="identity",fill="blue")+
    xlab("Documentation")+
    labs(title="Proportion of Reports Containing Terms")+
    coord_flip()+ 
    theme(panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))
  return(t)
}

############## Pathology Quality #############


#' Barrett's number of biopsies
#'
#' This function gets the biopsies taken per endoscopy and compares to the
#' Prague score for that endoscopy.
#' @param dataframe dataframe
#' @param Endo_ResultPerformed Date of the Endocscopy
#' @param PatientID Patient's unique identifier
#' @param Endoscopist name of the column with the Endoscopist names
#' @importFrom dplyr summarise group_by filter
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot geom_point labs theme xlab ylab unit element_text
#' @keywords Does something with data
#' @export
#' @examples # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' v<-HistolDx(Mypath,'Diagnosis')
#' v<-HistolExtrapolDx(v,'Diagnosis',"")
#' v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
#' v<-HistolBxSize(v,'Macroscopicdescription')
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' # The function relies on the other Barrett's functions being run as well:
#' b1<-Barretts_PragueScore(v,'Findings')
#' b2<-Barretts_PathStage(b1,'Histology')
#' b3<-Barretts_EventType(b2,'Histology','ProcedurePerformed',
#' 'Indications','Findings')
#' # The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' b4<-Barretts_FUType(b3,'Findings')
#' colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#' # The number of average number of biopsies is then calculated and
#' # compared to the average Prague C score so that those who are taking
#' # too few biopsies can be determined
#' hh<-BarrettsBxQual(b4,'Date.x','HospitalNumber',
#'                                      'Endoscopist')
#' rm(v)

BarrettsBxQual <- function(dataframe,
                                                 Endo_ResultPerformed,
                                                 PatientID,
                                                 Endoscopist) {
  dataframe <- data.frame(dataframe)
  PatientIDa <- rlang::sym(PatientID)
  Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)
  Endoscopista <- rlang::sym(Endoscopist)
  
  GroupedByEndoscopy <-
    dataframe %>% filter(!is.na(CStage), !is.na(NumbOfBx)) %>%
    group_by(as.Date(!!Endo_ResultPerformeda),!!PatientID,
             !!Endoscopista) %>%
    summarise(Sum = sum(NumbOfBx), AvgC = mean(CStage))
  
  GroupedByEndoscopy$ExpectedNumber <-
    (GroupedByEndoscopy$AvgC + 1) * 2
  GroupedByEndoscopy$Difference <-
    GroupedByEndoscopy$Sum - GroupedByEndoscopy$ExpectedNumber
  
  # Now group the difference by endoscopist
  BxShortfallPre <-
    GroupedByEndoscopy %>% group_by(!!Endoscopista) %>%
    summarise(MeanDiff = mean(Difference))
  
  BxShortfallPre<-data.frame(BxShortfallPre)
  
  # e) Then show shortfall of number of biopsies on a graph
  t <-
    ggplot() +
    geom_point(aes(BxShortfallPre$Endoscopist, BxShortfallPre$MeanDiff),
               size = 9) + geom_point(cex = 2) +
    labs(title = "Shortfall number of biopsies on Barrett's Surveillance list",
         x = "", y = "") +
    theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    theme(legend.position = "top") +
    xlab("Endoscopist") +
    ylab("Shortfall(Obs-\nexpec number Bx)") +
    theme(axis.text.x = element_text(angle = -90, size = 10)) +
    theme(axis.text.y = element_text(angle = -90, size = 10)) +
    theme(axis.title = element_text(size = 10)) +
    theme(title = element_text(size = 10)) +
    theme(legend.position = "top")
  
  functionResults <- list(BxShortfallPre = BxShortfallPre, t = t)
  return(functionResults)
  
}


############## Diagnostic yield functions #######

#' Dysplasia detection on surveillance
#'
#' This function assesses pathology of specimens taken at surveillance per year.
#' It outputs a plot which determines the the overall number of pathologies (
#' low/high grade dysplasia and cancer) for patients on surveillance
#'
#' @param dataframe the dataframe
#' @param titlePlot The plot title
#' @keywords dysplasia and cancer detection
#' @importFrom ggplot2 ggplot geom_bar ylab labs theme element_text xlab
#' @importFrom magrittr '%>%'
#' @export
#' @examples # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' v<-HistolDx(Mypath,'Diagnosis')
#' v<-HistolExtrapolDx(v,'Diagnosis',"")
#' v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
#' v<-HistolBxSize(v,'Macroscopicdescription')
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' # The function relies on the other Barrett's functions being run as well:
#' b1<-Barretts_PragueScore(v,'Findings')
#' b2<-Barretts_PathStage(b1,'Histology')
#' b3<-Barretts_EventType(b2,'Histology',
#' 'ProcedurePerformed','Indications','Findings')
#' # The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' b4<-Barretts_FUType(b3,'Findings')
#' colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#' # The function simply the the histopathological grades overall for
#' # your dataset and then creates a frequency plot of them
#' ii<-BarrettsPathDetectQual(b4,'Myplot')
#' rm(v)


BarrettsPathDetectQual <- function(dataframe, titlePlot) {
  dataframe <- data.frame(dataframe)
  
  LGD <- dataframe[grepl("LGD", dataframe$IMorNoIM), ]
  HGD <- dataframe[grepl("HGD", dataframe$IMorNoIM), ]
  OAC <- dataframe[grepl("SM1|SM2|T1b|T1a", dataframe$IMorNoIM), ]
  
  n <- c(nrow(LGD), nrow(HGD), nrow(OAC))
  b <- c("LGD", "HGD", "OAC")
  EndoMinDataSet <- data.frame(b, n)
  
  ggplot(EndoMinDataSet, aes(x = b, y = n)) +
    geom_bar(stat = "identity") + 
    xlab("Pathology Grade") +
    ylab("Total Number") + 
    theme(axis.text.x = element_text(angle = -90)) +
    labs(title = titlePlot) +
    theme(legend.position = "top")
}




#' Dysplasia detection rate on surveillance
#'
#' This function assesses the dysplasia detection rate per endoscopist.
#' @param dataframe dataframe,
#' @param EndoscopistReportColumn column with the Endoscopist names
#' @param IMorNoIM extracted with the function Barretts_PathStage()
#' @keywords dysplasia detection rate
#' @export
#' @examples # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' v<-HistolDx(Mypath,'Diagnosis')
#' v<-HistolExtrapolDx(v,'Diagnosis',"")
#' v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
#' v<-HistolBxSize(v,'Macroscopicdescription')
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' #The function relies on the other Barrett's functions being run as well:
#' b1<-Barretts_PragueScore(v,'Findings')
#' b2<-Barretts_PathStage(b1,'Histology')
#' b3<-Barretts_EventType(b2,'Histology',
#' 'ProcedurePerformed','Indications','Findings')
#' # The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' b4<-Barretts_FUType(b3,'Findings')
#' colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#' # The function takes the column with the extracted worst grade of
#' # histopathology and returns the proportion of each finding (ie
#' # proportion with low grade dysplasia, high grade etc.) for each
#' # endoscopist
#' jj<-BarrettsDDRQual(b4,'Endoscopist','IMorNoIM')
#' rm(v)



BarrettsDDRQual <- function(dataframe, EndoscopistReportColumn, IMorNoIM) {
  dataframe <- data.frame(dataframe)
  
  # Need to include indefinite for dysplasia
  myDDR <- prop.table(table(dataframe[, EndoscopistReportColumn],
                            dataframe[, IMorNoIM]))
  return(myDDR)
}

#' Number of Barrett's EMRs by grade
#'
#' Plots all the pathological grades of the EMRs.This should only be
#' run after all the BarrettsDataAccord functions.
#' @param EndoSubsetEMR The dataset.
#' @keywords EMR chart
#' @export
#' @examples # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' v<-HistolDx(Mypath,'Diagnosis')
#' v<-HistolExtrapolDx(v,'Diagnosis',"")
#' v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
#' v<-HistolBxSize(v,'Macroscopicdescription')
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' #The function relies on the other Barrett's functions being run as well:
#' b1<-Barretts_PragueScore(v,'Findings')
#' b2<-Barretts_PathStage(b1,'Histology')
#' b3<-Barretts_EventType(b2,'Histology',
#' 'ProcedurePerformed','Indications','Findings')
#' # The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' b4<-Barretts_FUType(b3,'Findings')
#' colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#' # The function extracts only those rows for patients who have undergone
#' # EMR and then extracts the EMR grade from the extracted worst histopath
#' # column (called IMorNoIM). This is then plotted out.
#' kk<-BarrettsEMRGrades(b4)
#' rm(v)

BarrettsEMRGrades <- function(EndoSubsetEMR) {
  EndoSubsetEMR <- EndoSubsetEMR[EndoSubsetEMR$EVENT == "EMR", ]
  AllEMRs <- nrow(EndoSubsetEMR)
  SM2 <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "SM2", ])
  SM1 <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "SM1", ])
  T1b <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "T1b", ])
  T1a <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "T1a", ])
  HGD <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "HGD", ])
  LGD <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "LGD", ])
  IM <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "IM", ])
  NoIM <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "No_IM", ])
  
  n <- c(
    as.numeric(AllEMRs),
    as.numeric(SM2),
    as.numeric(SM1),
    as.numeric(T1b),
    as.numeric(T1a),
    as.numeric(HGD),
    as.numeric(LGD),
    as.numeric(IM),
    as.numeric(NoIM)
  )
  s <- c("AllEMRs",
         "SM2",
         "SM1",
         "T1b_Unspecified",
         "T1a",
         "HGD",
         "LGD",
         "IM",
         "No IM")
  EMRResult <- data.frame(s, n)
  # axis(1, at=mids)
  barplot(
    EMRResult$n,
    names.arg = c(
      "AllEMRs",
      "SM2",
      "SM1",
      "T1b_Unspec",
      "T1a",
      "HGD",
      "LGD",
      "IM",
      "No IM"
    ),
    xlab = "Tissue grade",
    ylab = "Number of EMRs",
    cex.lab = 2.0,
    cex.axis = 1.5,
    cex.main = 1.5,
    cex.names = 1.5,
    main = "EMR Tissue pathology results"
  )
}




#' Barretts Basic Numbers
#'
#' This looks at the basic numbers of all the therapeutic endoscopies over time.
#' This should only be run after all the BarrettsDataAccord functions.
#' @param dataframe the dataframe
#' @param Endo_ResultPerformed the date the endoscopy was performed
#' @keywords Number of therapies
#' @importFrom dplyr filter group_by mutate summarise
#' @importFrom magrittr '%>%'
#' @importFrom rlang sym
#' @importFrom lubridate year
#' @importFrom ggplot2 ggplot aes geom_line labs
#' @export
#' @examples # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' rm(list=ls(all=TRUE))
#' v<-HistolDx(Mypath,'Diagnosis')
#' v<-HistolExtrapolDx(v,'Diagnosis',"")
#' v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
#' v<-HistolBxSize(v,'Macroscopicdescription')
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,
#' "Dateofprocedure","HospitalNumber")
#' # The function relies on the other Barrett's functions being run as well:
#' b1<-Barretts_PragueScore(v,'Findings')
#' b2<-Barretts_PathStage(b1,'Histology')
#' b3<-Barretts_EventType(b2,'Histology',
#'  'ProcedurePerformed','Indications','Findings')
#'  
#' # The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' b4<-Barretts_FUType(b3,'Findings')
#' colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#' # The function groups the overall number of surveillance cases over time
#' # The endoscopic episodes should be selected,according to surveillance
#' # being the indication prior to using this function
#' jj<-BarrettsBasicNumbers(b4,"Date.x")
#' rm(v)

BarrettsBasicNumbers <- function(dataframe, Endo_ResultPerformed) {
  dataframe <- data.frame(dataframe)
  Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)
  xNum <-
    dataframe %>% filter(EVENT != "nothing") %>%
    mutate(year = year(as.Date(!!Endo_ResultPerformeda))) %>%
    group_by(EVENT, year) %>% summarise(n = n())
  xNumPlot <-
    ggplot(xNum, aes(
      x = year,
      y = n,
      group = EVENT,
      colour = EVENT
    )) + 
    geom_line() + geom_smooth()+
    labs(title="Number of procedures by type")
    
    
  functionResults <-
    list(ProcNumbers = xNum, ProcNumbersPlot = xNumPlot)
  return(functionResults)
}

#' RFA catheter use
#'
#' This looks at the basic numbers of RFA by catheter type used.
#' This should only be run after all the BarrettsDataAccord functions.
#' @param EndoSubsetRFA The dataframe
#' @param Column report field of interest
#' @param Column2 Another endoscopy report field of interest
#' @keywords RFA, Radiofrequency ablation
#' @export
#' @examples # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' v<-HistolDx(Mypath,'Diagnosis')
#' v<-HistolExtrapolDx(v,'Diagnosis',"")
#' v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
#' v<-HistolBxSize(v,'Macroscopicdescription')
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date #and Hospital number
#' v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,
#' "Dateofprocedure","HospitalNumber")
#' # The function relies on the other Barrett's functions being run as well:
#' b1<-Barretts_PragueScore(v,'Findings')
#' b2<-Barretts_PathStage(b1,'Histology')
#' b3<-Barretts_EventType(b2,'Histology',
#' 'ProcedurePerformed','Indications','Findings')
#' # The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' b4<-Barretts_FUType(b3,'Findings')
#' colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#' # The function takes the RFA cases by looking in any free text column where
#' # endoscopy fingings are described and then summarising by RFA subtype
#' ll<-BarrettssRFACath(b4,"ProcedurePerformed","Findings")
#' rm(v)

BarrettssRFACath <- function(EndoSubsetRFA, Column, Column2) {
  EndoSubsetRFA <- EndoSubsetRFA[EndoSubsetRFA$EVENT == "RFA", ]
  HALO90a <-
    EndoSubsetRFA[grepl("90", EndoSubsetRFA[, Column], perl = TRUE), ]
  HALO90b <-
    EndoSubsetRFA[grepl("90", EndoSubsetRFA[, Column2], perl = TRUE), ]
  HALO90c <- rbind(HALO90a, HALO90b)
  
  HALO360a <-
    EndoSubsetRFA[grepl("360", EndoSubsetRFA[, Column], perl = TRUE), ]
  HALO360b <-
    EndoSubsetRFA[grepl("360", EndoSubsetRFA[, Column2], perl = TRUE), ]
  HALO360c <- rbind(HALO360a, HALO360b)
  
  HALO60a <-
    EndoSubsetRFA[grepl("HALO60| 60",
                        EndoSubsetRFA[, Column], perl = TRUE), ]
  HALO60b <-
    EndoSubsetRFA[grepl("HALO60| 60", 
                        EndoSubsetRFA[, Column2], perl = TRUE), ]
  HALO60c <- rbind(HALO60a, HALO60b)
  
  HALOTTSa <-
    EndoSubsetRFA[grepl("TTS|[Cc]hannel", 
                        EndoSubsetRFA[, Column], perl = TRUE), ]
  HALOTTSb <-
    EndoSubsetRFA[grepl("TTS|[Cc]hannel", 
                        EndoSubsetRFA[, Column2], perl = TRUE), ]
  HALOTTSc <- rbind(HALOTTSa, HALOTTSb)
  
  HALOAPCa <-
    EndoSubsetRFA[grepl("APC", EndoSubsetRFA[, Column], perl = TRUE), ]
  HALOAPCb <-
    EndoSubsetRFA[grepl("APC", EndoSubsetRFA[, Column2], perl = TRUE), ]
  HALOAPCc <- rbind(HALOAPCa, HALOAPCb)
  
  
  n <- c(nrow(HALO90c),
         nrow(HALO360c),
         nrow(HALO60c),
         nrow(HALOTTSc),
         nrow(HALOAPCc))
  s <- c("HALO 90", "HALO 360", "HALO 60", "HALO TTS", "APC")
  EMRResult <- data.frame(s, n)
  # axis(1, at=mids, labels=EMRResult%s) axis(1, at=mids)
  barplot(
    EMRResult$n,
    names.arg = c("HALO 90", "HALO 360", "HALO 60", "HALO TTS", "APC"),
    xlab = "Catheter type",
    ylab = "Number of RFA's",
    cex.lab = 1.5,
    cex.axis = 1.5,
    cex.main = 1.5,
    cex.names = 1.5,
    main = "RFA Catheter type usage"
  )
}




#' Paris vs histopath Barrett's
#'
#' This looks at the Paris grades of each EMR and then creates a heatmap
#' of pathological grade vs
#' endoscopic Paris grade.This should only be run after all the
#' BarrettsDataAccord functions.
#' @param EndoSubsetEMR The dataframe
#' @param Column Endoscopy report field of interest
#' @param Column2 Another endoscopy report field of interest
#' @keywords Does something with data
#' @export
#' @examples # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' v<-HistolAccessionNumber(Mypath,'Histology',
#' 'SP-\\d{2}-\\d{7}')
#' v<-HistolDx(v,'Diagnosis')
#' v<-HistolExtrapolDx(v,'Diagnosis',"")
#' v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
#' v<-HistolBxSize(v,'Macroscopicdescription')
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' #The function relies on the other Barrett's functions being run as well:
#' b1<-Barretts_PragueScore(v,'Findings')
#' b2<-Barretts_PathStage(b1,'Histology')
#' b3<-Barretts_EventType(b2,'Histology',
#' 'ProcedurePerformed','Indications','Findings')
#' # The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' b4<-Barretts_FUType(b3,'Findings')
#' colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#' # The function compares the Paris score 
#' # from the endoscopy report free text to
#' # the histopathology scores for the same endoscopies so you can see what the
#' # lesion recognition is like overall
#' mm<-BarrettsParisEMR(b4,"ProcedurePerformed","Findings")
#' rm(v)

BarrettsParisEMR <- function(EndoSubsetEMR, Column, Column2) {
  EndoSubsetEMR <- EndoSubsetEMR[EndoSubsetEMR$EVENT == "EMR", ]
  EndoSubsetEMR$ParisClass <-
    ifelse(
      grepl("11a_c|2a_c|[Ii][Ii]a_c", EndoSubsetEMR[, Column], perl = TRUE) |
        grepl("11a_c|2a_c|[Ii][Ii]a_c", EndoSubsetEMR[, Column2], perl = TRUE),
      "2a_c",
      ifelse(
        grepl("[Ii][Ii]a|2a|11a",
              EndoSubsetEMR[, Column], perl = TRUE) |
          grepl("[Ii][Ii]a|2a|11a", EndoSubsetEMR[, Column2], perl = TRUE),
        "2a",
        ifelse(
          grepl("[Ii][Ii]b|2b|11b", EndoSubsetEMR[, Column], perl = TRUE) |
            grepl("[Ii][Ii]b|2b|11b",
                  EndoSubsetEMR[, Column2], perl = TRUE),
          "2b",
          ifelse(
            grepl("[Ii][Ii]c|2c|11c", EndoSubsetEMR[, Column], perl = TRUE) |
              grepl("[Ii][Ii]c|2c|11c", 
                    EndoSubsetEMR[, Column2], perl = TRUE),
            "2c",
            ifelse(
              grepl("[Ii][Ii][Ii]|III",
                    EndoSubsetEMR[, Column], perl = TRUE) |
                grepl("[Ii][Ii][Ii]|III", 
                      EndoSubsetEMR[, Column2], perl = TRUE),
              "3",
              ifelse(
                grepl("Paris [Tt]ype [Ii]s|1s "
                      , EndoSubsetEMR[, Column], perl = TRUE) |
                  grepl("Paris [Tt]ype [Ii]s|1s",
                        EndoSubsetEMR[, Column2], perl = TRUE),
                "1s",
                ifelse(
                  grepl("[Ii]p|1p", EndoSubsetEMR[, Column], perl = TRUE) |
                    grepl("[Ii]p|1p", EndoSubsetEMR[, Column2], perl = TRUE),
                  "1p",
                  "No_Paris"
                )
              )
            )
          )
        )
      )
    )
  
  # Create the matrix
  df3 <-
    data.frame(EndoSubsetEMR$ParisClass, EndoSubsetEMR$IMorNoIM)
  # Reorganise the column names and rows Get rid of no Paris EMR's
  dfy <- df3[!df3$EndoSubsetEMR.ParisClass == "No_Paris", ]
  # Get the histology proportions by the Paris grade
  tr4 <- as.data.frame.matrix(prop.table(table(dfy), 1))
  
  tr5 <- as.matrix(tr4)
  tr5 <- head(tr5, -1)
  # Create the heatmap par(oma = c(4, 0, 0, 4))
  
  tr5 <- tr5[!!rowSums(!is.na(tr5)), ]
  tr5 <- t(tr5)
  tr5 <- tr5[!!rowSums(!is.na(tr5)), ]
  tr5 <- t(tr5)
  if (nrow(tr5) > 2 & ncol(tr5) > 2) {
    colors <- c(seq(-1, 0.2, length = 100),
                seq(0.21, 0.8, length = 100),
                seq(0.81, 1, length = 100))
    
    gplots::heatmap.2(
      tr5,
      trace = "none",
      breaks = colors,
      density.info = "none",
      dendrogram = "none",
      Rowv = FALSE,
      Colv = FALSE,
      cexRow = 3.5,
      cexCol = 1.5
    )
  }
  
}

#' CRIM status Barrett's
#'
#' This collects the patients in whom it is assumed that complete clearance of
#' intestinal metasplasia has occurred (CRIM) after ablation therapy.
#' This is done by collecting those endoscopies where the last EVENT was
#' equal to 'nothing' when the patient had undergone radiofrequency ablation at
#' some point
#' @param dataframe The dataframe
#' @param HospNum The Hospital Number column
#' @param EVENT The column  called EVENT that determines what procedure the
#' patient had at that endoscopy
#' @keywords CRIM
#' @importFrom dplyr group_by slice mutate lead
#' @importFrom rlang sym
#' @export
#' @examples # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' v<-HistolAccessionNumber(Mypath,'Histology',
#' 'SP-\\d{2}-\\d{7}')
#' v<-HistolDx(v,'Diagnosis')
#' v<-HistolExtrapolDx(v,'Diagnosis',"")
#' v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
#' v<-HistolBxSize(v,'Macroscopicdescription')
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' # The function relies on the other Barrett's functions being run as well:
#' b1<-Barretts_PragueScore(v,'Findings')
#' b2<-Barretts_PathStage(b1,'Histology')
#' b3<-Barretts_EventType(b2,'Histology',
#' 'ProcedurePerformed','Indications','Findings')
#' colnames(b3)[colnames(b3) == 'pHospitalNum'] <- 'HospitalNumber'
#' # The function groups the procedures by patient and then looks at those which
#' # have 'nothing' in the event column (which means biopsies only) which was
#' # preceded by radiofrequency ablation (RFA) so that these patients are
#' # labelled as having clearance of intestinal metaplasia. The result is a true
#'# or false column.
#' nn<-Barretts_CRIM(b3,'HospitalNumber',"EVENT")
#' rm(v)

Barretts_CRIM <- function(dataframe, HospNum, EVENT) {
  dataframe <- data.frame(dataframe)
  HospNuma <- sym(HospNum)
  EVENTa <- sym(EVENT)
  
  CRIM <-
    dataframe %>% group_by(!!HospNuma) %>%
    mutate(ind = (!!EVENTa) == "RFA" &
             lead(!!EVENTa) == "nothing") %>%
    slice(sort(c(which(ind), which(ind) + 1)))
  CRIM<-data.frame(CRIM)
  return(CRIM)
}
