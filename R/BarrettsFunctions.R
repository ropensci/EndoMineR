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
###### Barrett's specific extrapolation Functions ######

#' Prague score extraction
#'
#' The aim is to extract a C and M stage (Prague score) for Barrett's samples.
#' This is done using a regex where C and M stages are explicitly mentioned in
#' the free text
#' Specfically it extracts the Prague score
#' @param dataframe dataframe with column of interest
#' @param EndoReportColumn column of interest
#' @param EndoReportColumn2 second column of interest
#' @importFrom stringr str_extract str_replace str_extract_all
#' @importFrom purrr map
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
#' aa<-Barretts_PragueScore(Myendo,'Findings','OGDReportWhole')

#Change to BarrPrague nee Barretts_PragueScore
Barretts_PragueScore <- function(dataframe, EndoReportColumn,EndoReportColumn2) {
  dataframe <- data.frame(dataframe)

  
  dataframe$CStage <-   
    #If the CStage is present then extract it
    ifelse(grepl("([Cc](\\s|=)*\\d+)",dataframe[,EndoReportColumn]),
           stringr::str_replace(stringr::str_extract(dataframe[,EndoReportColumn],'([Cc](\\s|=)*\\d+)'),"[Cc]", ""),
           ifelse(grepl("([Cc](\\s|=)*\\d+)",dataframe[,EndoReportColumn2]),
                  stringr::str_replace(stringr::str_extract(dataframe[,EndoReportColumn2],'([Cc](\\s|=)*\\d+)'),"[Cc]", ""),
                  ifelse(grepl("([Cc](\\s|=)*O(\\s)*[Mm](\\s)*\\d+)",dataframe[,EndoReportColumn]),
                         stringr::str_replace(stringr::str_extract(dataframe[,EndoReportColumn],'([Cc](\\s|=)*O(\\s)*[Mm](\\s)*\\d+)'),"[Cc]", ""),
                  "Insufficient")))
  
dataframe$CStage<-trimws(unlist(dataframe$CStage))
dataframe$CStage<-gsub("O","0",dataframe$CStage)
dataframe$CStage<-gsub("M.*","",dataframe$CStage)

dataframe$mytext<-stri_split_boundaries(dataframe[,EndoReportColumn], type="sentence")
dataframe$mytext<-lapply(dataframe$mytext,function(x) trimws(x))


dataframe<-dataframe %>%
  mutate(
    MStage = map(
      mytext, ~ case_when(
        grepl("( [Mm](\\s|=)*\\d+)",.x) ~ stringr::str_replace(stringr::str_extract(.x,"( [Mm](\\s|=)*\\d+)"),"M", ""),
        #dataframe[,CStage]!="Insufficient" ~ dataframe[,CStage],
        grepl("(?=[^\\.]*Barr)[^\\.]*\\s+\\d{2}\\s*[cm]*\\s*(to |-| and)\\s*\\d{2}\\s*[cm]*\\s*",.x,ignore.case = TRUE,perl=TRUE)  ~ as.character(as.numeric(sapply(stringr::str_extract_all(stringr::str_extract(.x,"\\d{2}\\s*[cm]*\\s*(to|-|and)\\s*\\d{2}\\s*[cm]*\\s*"),"\\d{2}"), function(y) abs(diff(as.numeric(y)))))),
        grepl("(?=[^\\.]*cm)(?=[^\\.]*Barr)(?=[^\\.]*(of |length))[^\\.]*", .x, perl=TRUE)  ~  stringr::str_extract(paste0(stringr::str_match(.x,"(?=[^\\.]*cm)(?=[^\\.]*[Bb]arr)(?=[^\\.]*(of |length))[^\\.]*"),collapse=""),"\\d+"),
        grepl("(\\.|^|\n)(?=[^\\.]*(small|tiny|tongue|finger))(?=[^\\.]*Barr)[^\\.]*(\\.|\n|$)", .x, perl=TRUE) ~  stringr::str_replace(.x, ".*","1"),
                TRUE ~ "Insufficient")
    )
  )


dataframe$MStage<-lapply(dataframe$MStage, function(x) gsub("Insufficient","",x))
dataframe$MStage<-lapply(dataframe$MStage, function(x) gsub("m","",x))

dataframe$MStage<-suppressWarnings(unlist(lapply(dataframe$MStage, function(x) max(as.numeric(x),na.rm=TRUE))))
#If there are more than two numbers pick the highest one

dataframe$MStage<-ifelse(is.infinite(dataframe$MStage),ifelse(dataframe$CStage!="Insufficient",dataframe$CStage,"Insufficient"),dataframe$MStage)

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
#' @importFrom rlang sym
#' @examples # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data 
#' # cleaning
#' # as part of the package.
#' v<-HistolDx(Mypath,'Diagnosis')
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' # The function then takes the Histology column from the merged data set (v).
#' # It extracts the worst histological grade for a specimen
#' nn<-HistolDx(Mypath,'Diagnosis')
#' b<-Barretts_PathStage(nn,'Dx_Simplified')
#' rm(v)

Barretts_PathStage <- function(dataframe, PathColumn) {
  # Get the worst pathology for that sample inc SM stages
  dataframe <- data.frame(dataframe)
  PathColumna <- rlang::sym(PathColumn)
  
  df<-dataframe %>%
    mutate(
      IMorNoIM = case_when(
          grepl("sm2", !!PathColumna,ignore.case=TRUE) ~ "SM2",
          grepl("sm1", !!PathColumna,ignore.case=TRUE) ~ "SM1",
          grepl("T1b", !!PathColumna,ignore.case=TRUE) ~  "T1b",
          grepl("T1a|ntramucosal", !!PathColumna,ignore.case=TRUE) ~  "T1a",
          grepl("[Hh]igh [Gg]rade ", !!PathColumna,ignore.case=TRUE,perl=TRUE) ~  "HGD",
          grepl("[Ll]ow [Gg]rade", !!PathColumna,ignore.case=TRUE,perl=TRUE) ~  "LGD",
          grepl("[Ii]ndef", !!PathColumna,ignore.case=TRUE,perl=TRUE) ~  "IGD",
          grepl("[Ii]ntestinal|[^-][Ss]pecialised", !!PathColumna,ignore.case=TRUE,perl=TRUE) ~  "IM",
          grepl("[Mm]etaplasia|[Cc]olumnar|Glandular", !!PathColumna,ignore.case=TRUE,perl=TRUE) ~  "No_IM",
          is.na(!!PathColumna) ~  "Insufficient",
          TRUE ~ "Insufficient")
      )
    
  return(df)
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
#' metaplasia if the sample is non-dysplastic. Because reports often do not record
#' a Prague score a more pragmatic approach as been to assess the M stage and if 
#' this is not present then to use the C stage extrapolated using the 
#' Barretts_Prague function
#' @param dataframe the dataframe(which has to have been processed by the
#' Barretts_PathStage function first to get IMorNoIM and the Barretts_PragueScore
#' to get the C and M stage if available),
#' @keywords Follow-Up
#' @importFrom stringr str_extract str_replace
#' @export
#' @examples # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' v<-HistolDx(Mypath,'Diagnosis')
#' v$NumBx<-HistolNumbOfBx(v$Macroscopicdescription,'specimen')
#' v$BxSize<-HistolBxSize(v$Macroscopicdescription)
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' #The function relies on the other Barrett's functions being run as well:
#' b<-Barretts_PathStage(v,'Histology')
#' b2<-Barretts_PragueScore(b,'Findings')
#' #The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' cc<-Barretts_FUType(b2,"CStage","MStage","IMorNoIM")
#' rm(v)

Barretts_FUType <- function(dataframe,CStage,MStage,IMorNoIM) {
  dataframe <- data.frame(dataframe)
  
  CStagea<-rlang::sym(CStage)
  MStagea<-rlang::sym(MStage)
  IMorNoIMa<-rlang::sym(IMorNoIM)

  df<-dataframe %>%
    mutate(
      ParisClass = case_when(
        grepl("SM2|SM1|T1b_Unspec|T1a|LGD|HGD|IGD", !!IMorNoIMa,ignore.case=TRUE) ~ "Therapy",
        !!CStagea == "Insufficient" & !!MStagea == "Insufficient" ~ "NoRules",
        !!IMorNoIMa == "No_IM" & !is.na(!!MStagea) & as.numeric(!!MStagea) < 3 ~ "Rule1",
        !!IMorNoIMa == "IM" & !is.na(!!MStagea) & as.numeric(!!MStagea) < 3 ~ "Rule2",
        (!is.na(!!MStagea) & as.numeric(!!MStagea)) >= 3 ~ "Rule3",
        !!IMorNoIMa == "No_IM" & !is.na(!!CStagea) & as.numeric(!!CStagea) < 3 ~"Rule1",
        !!IMorNoIMa == "IM" & !is.na(!!CStagea) & as.numeric(!!CStagea) < 3 ~"Rule2",
        (!is.na(!!CStagea) & as.numeric(!!CStagea)) >= 3 ~ "Rule3",
        TRUE ~ "NoRules")
    )
  
  return(df)

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
#' v<-Mypath
#' v<-HistolDx(v,'Diagnosis')
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' #The function relies on the other Barrett's functions being run as well:
#' b1<-Barretts_PragueScore(v,'Findings')
#' b2<-Barretts_PathStage(b1,'Histology')
#' # The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' b4<-Barretts_FUType(b2,"CStage","MStage","IMorNoIM")
#' colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#' # The function compares the Paris score 
#' # from the endoscopy report free text to
#' # the histopathology scores for the same endoscopies so you can see what the
#' # lesion recognition is like overall
#' mm<-BarrettsParisEMR(b4,"ProcedurePerformed","Findings")
#' rm(v)

BarrettsParisEMR <- function(dataframe, Column, Column2) {
  
  #NewCol<-paste0(Column, Column2)
  NewCol<-paste0(dataframe[,Column], dataframe[,Column2])
  NewCol <- data.frame(NewCol,stringsAsFactors = FALSE)
  
  # Get the worst pathology for that sample inc SM stages
  df<-NewCol %>%
    mutate(
      ParisClass = case_when(
        grepl("11a_c|2a_c|[Ii][Ii]a_c", NewCol,ignore.case=TRUE) ~ "2a_c",
        grepl("[Ii][Ii]a|2a|11a", NewCol,ignore.case=TRUE) ~ "2a",
        grepl("[Ii][Ii]b|2b|11b", NewCol,ignore.case=TRUE) ~  "2b",
        grepl("[Ii][Ii][Ii]|III", NewCol,ignore.case=TRUE) ~  "3",
        grepl("Paris [Tt]ype [Ii]s|1s ", NewCol,ignore.case=TRUE,perl=TRUE) ~  "!s",
        grepl("[Ii]p|1p", NewCol,ignore.case=TRUE,perl=TRUE) ~  "1p",
        TRUE ~ "No_Paris")
    )
  
  return(df)
  

  
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
#' #ee<-BarrettsSurveil(Myendo,'HospitalNumber','Dateofprocedure','Indications')

BarrettsSurveil <- function(dataframe,PatientID,Endo_ResultPerformed,
                                              IndicationsFroExamination) {
  dataframe <- data.frame(dataframe)
  PatientIDa <- rlang::sym(PatientID)
  Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)
  IndicationsFroExaminationa <-
    rlang::sym(IndicationsFroExamination)
  # So you want all those whose last endoscopy was non surveillance but who
  # have
  # a difftime between now and the last of > 3years So get the last endoscopy
  # for each patient Filter out the endoscopies that were surveillance Get the
  # difftime between now and the last endoscopy
  # Filter for those who have been waiting >3 years post non surveillance
  # endoscopy.Yes
  
  t <-
    dataframe %>% arrange(as.Date(!!Endo_ResultPerformeda)) %>%
    group_by(!!PatientIDa) %>%
    slice(dplyr::n()) %>%
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
#' v<-Mypath
#' v<-HistolDx(v,'Diagnosis')
#' v$NumBx<-HistolNumbOfBx(v$Macroscopicdescription,'specimen')
#' v$BxSize<-HistolBxSize(v$Macroscopicdescription)
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' # The function relies on the other Barrett's functions being run as well:
#' b1<-Barretts_PragueScore(v,'Findings')
#' b2<-Barretts_PathStage(b1,'Histology')

#' # The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' b4<-Barretts_FUType(b2,"CStage","MStage","IMorNoIM")
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
#' v$NumBx<-HistolNumbOfBx(v$Macroscopicdescription,'specimen')
#' v$BxSize<-HistolBxSize(v$Macroscopicdescription)
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
#' 'HospitalNumber')
#' # The function relies on the other Barrett's functions being run as well:
#' b1<-Barretts_PragueScore(v,'Findings')
#' b2<-Barretts_PathStage(b1,'Histology')

#' # The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' b4<-Barretts_FUType(b2,"CStage","MStage","IMorNoIM")
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
    suppressWarnings(dataframe %>% filter(!is.na(CStage), !is.na(NumbOfBx)) %>%
    group_by(as.Date(!!Endo_ResultPerformeda),!!PatientID,
             !!Endoscopista) %>%
    summarise(Sum = sum(NumbOfBx), AvgC = mean(CStage)))
  
  GroupedByEndoscopy$ExpectedNumber <-
    (GroupedByEndoscopy$AvgC + 1) * 2
  GroupedByEndoscopy$Difference <-
    GroupedByEndoscopy$Sum - GroupedByEndoscopy$ExpectedNumber
  
  # Now group the difference by endoscopist
  BxShortfallPre <-
    GroupedByEndoscopy %>% group_by(!!Endoscopista) %>%
    summarise(MeanDiff = mean(Difference))
  
  BxShortfallPre<-data.frame(BxShortfallPre)
  return(BxShortfallPre)
  
}





