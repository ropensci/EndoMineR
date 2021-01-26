if (getRversion() >= "2.15.1") {
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
      "ind",
      "mytext",
      "NumBx"
    )
  )
}
###### Barrett's specific extrapolation Functions ######

#' Extract the Prague score
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
#' @importFrom dplyr case_when
#' @keywords  Prague score
#' @export
#' @family Disease Specific Analysis - Barretts Data
#' @examples
#' # The example takes the endoscopy demo dataset and searches the
#' # Findings column (which contains endoscopy free text about the
#' # procedure itself). It then extracts the Prague score if relevant. I
#' # find it easiest to use this on a Barrett's subset of data rather than
#' # a dump of all endoscopies but of course this is a permissible dataset
#' # too
#' 
#' 
#' aa <- Barretts_PragueScore(Myendo, "Findings", "OGDReportWhole")
Barretts_PragueScore <- function(dataframe, EndoReportColumn, EndoReportColumn2) {
  dataframe <- data.frame(dataframe)


  dataframe$CStage <-
    # If the CStage is present then extract it
    ifelse(grepl("([Cc](\\s|=)*\\d+)", dataframe[, EndoReportColumn]),
      stringr::str_replace(stringr::str_extract(dataframe[, EndoReportColumn], "([Cc](\\s|=)*\\d+)"), "[Cc]", ""),
      ifelse(grepl("([Cc](\\s|=)*\\d+)", dataframe[, EndoReportColumn2]),
        stringr::str_replace(stringr::str_extract(dataframe[, EndoReportColumn2], "([Cc](\\s|=)*\\d+)"), "[Cc]", ""),
        ifelse(grepl("([Cc](\\s|=)*O(\\s)*[Mm](\\s)*\\d+)", dataframe[, EndoReportColumn]),
          stringr::str_replace(stringr::str_extract(dataframe[, EndoReportColumn], "([Cc](\\s|=)*O(\\s)*[Mm](\\s)*\\d+)"), "[Cc]", ""),
          "Insufficient"
        )
      )
    )

  dataframe$CStage <- trimws(unlist(dataframe$CStage))
  dataframe$CStage <- gsub("O", "0", dataframe$CStage)
  dataframe$CStage <- gsub("M.*", "", dataframe$CStage)

  dataframe$mytext <- stri_split_boundaries(dataframe[, EndoReportColumn], type = "sentence")
  dataframe$mytext <- lapply(dataframe$mytext, function(x) trimws(x))
  
  

  dataframe <- dataframe %>%
    mutate(
      MStage = map(
        mytext, ~ case_when(
          grepl("((?<![Cc])[Mm](\\s|=)*\\d+)", .x,perl = TRUE) ~ stringr::str_replace(stringr::str_extract(.x, "((?<![Cc])[Mm](\\s|=)*\\d+)"), "M", ""),
          grepl("(?=[^\\.]*Barr)[^\\.]*\\s+\\d{2}\\s*[cm]*\\s*(to |-| and)\\s*\\d{2}\\s*[cm]*\\s*", .x, ignore.case = TRUE, perl = TRUE) ~ as.character(as.numeric(sapply(stringr::str_extract_all(stringr::str_extract(.x, "\\d{2}\\s*[cm]*\\s*(to|-|and)\\s*\\d{2}\\s*[cm]*\\s*"), "\\d{2}"), function(y) abs(diff(as.numeric(y)))))),
          grepl("(?=[^\\.]*cm)(?=[^\\.]*Barr)(?=[^\\.]*(of |length))[^\\.]*", .x, perl = TRUE) ~ stringr::str_extract(paste0(stringr::str_match(.x, "(?=[^\\.]*cm)(?=[^\\.]*[Bb]arr)(?=[^\\.]*(of |length))[^\\.]*"), collapse = ""), "\\d+"),
          grepl("(\\.|^|\n)(?=[^\\.]*(small|tiny|tongue|finger))(?=[^\\.]*Barr)[^\\.]*(\\.|\n|$)", .x, perl = TRUE) ~ stringr::str_replace(.x, ".*", "1"),
          TRUE ~ "Insufficient"
        )
      )
    )


  dataframe$MStage <- lapply(dataframe$MStage, function(x) gsub("Insufficient", "", x))
  dataframe$MStage <- lapply(dataframe$MStage, function(x) gsub("m", "", x))
  dataframe$MStage <- suppressWarnings(unlist(lapply(dataframe$MStage, function(x) max(as.numeric(x), na.rm = TRUE))))
  # If there are more than two numbers pick the highest one

  dataframe$MStage <- ifelse(is.infinite(dataframe$MStage), ifelse(dataframe$CStage != "Insufficient", dataframe$CStage, "Insufficient"), dataframe$MStage)

  return(dataframe)
}


#' Get the worst pathological stage for Barrett's
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
#' @importFrom dplyr case_when
#' @family Disease Specific Analysis - Barretts Data
#' @examples
#' # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' # The function then takes the Histology column from the merged data set (v).
#' # It extracts the worst histological grade for a specimen
#' b <- Barretts_PathStage(Mypath, "Histology")
#' rm(v)
Barretts_PathStage <- function(dataframe, PathColumn) {
  # Get the worst pathology for that sample inc SM stages
  dataframe <- data.frame(dataframe)
  PathColumna <- rlang::sym(PathColumn)

  df <- dataframe %>%
    mutate(
      IMorNoIM = case_when(
        grepl("sm2", !!PathColumna, ignore.case = TRUE) ~ "SM2",
        grepl("sm1", !!PathColumna, ignore.case = TRUE) ~ "SM1",
        grepl("T1b", !!PathColumna, ignore.case = TRUE) ~ "T1b",
        grepl("T1a|ntramucosal", !!PathColumna, ignore.case = TRUE) ~ "T1a",
        grepl("denocarcino", !!PathColumna, ignore.case = TRUE) ~ "Cancer unstaged",
        grepl("[Hh]igh [Gg]rade ", !!PathColumna, ignore.case = TRUE, perl = TRUE) ~ "HGD",
        grepl("[Ll]ow [Gg]rade", !!PathColumna, ignore.case = TRUE, perl = TRUE) ~ "LGD",
        grepl("[Ii]ndef", !!PathColumna, ignore.case = TRUE, perl = TRUE) ~ "IGD",
        grepl("[Ii]ntestinal|[^-][Ss]pecialised", !!PathColumna, ignore.case = TRUE, perl = TRUE) ~ "IM",
        grepl("[Mm]etaplasia|[Cc]olumnar|Glandular", !!PathColumna, ignore.case = TRUE, perl = TRUE) ~ "No_IM",
        is.na(!!PathColumna) ~ "Insufficient",
        TRUE ~ "Insufficient"
      )
    )

  return(df$IMorNoIM)
}






#' Determine the Follow up group
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
#' @param CStage CStage column
#' @param MStage MStage column
#' @param IMorNoIM IMorNoIM column
#' @keywords Follow-Up
#' @importFrom stringr str_extract str_replace
#' @importFrom dplyr case_when
#' @export
#' @family Disease Specific Analysis - Barretts Data
#' @examples
#' # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' v <- Mypath
#' v$NumBx <- HistolNumbOfBx(v$Macroscopicdescription, "specimen")
#' v$BxSize <- HistolBxSize(v$Macroscopicdescription)
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v <- Endomerge2(
#'   Myendo, "Dateofprocedure", "HospitalNumber", v, "Dateofprocedure",
#'   "HospitalNumber"
#' )
#' # The function relies on the other Barrett's functions being run as well:
#' v$IMorNoIM <- Barretts_PathStage(v, "Histology")
#' v <- Barretts_PragueScore(v, "Findings")
#' 
#' # The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' v$FU_Type <- Barretts_FUType(v, "CStage", "MStage", "IMorNoIM")
#' rm(v)
Barretts_FUType <- function(dataframe, CStage, MStage, IMorNoIM) {
  dataframe <- data.frame(dataframe)

  CStagea <- rlang::sym(CStage)
  MStagea <- rlang::sym(MStage)
  IMorNoIMa <- rlang::sym(IMorNoIM)

  df <- dataframe %>%
    mutate(
      FU_Type = case_when(
        grepl("SM2|SM1|T1b_Unspec|T1a|LGD|HGD|IGD", !!IMorNoIMa, ignore.case = TRUE) ~ "Therapy",
        !!CStagea == "Insufficient" & !!MStagea == "Insufficient" ~ "NoRules",
        !!IMorNoIMa == "No_IM" & !is.na(!!MStagea) & as.numeric(!!MStagea) < 3 ~ "Rule1",
        !!IMorNoIMa == "IM" & !is.na(!!MStagea) & as.numeric(!!MStagea) < 3 ~ "Rule2",
        (!is.na(!!MStagea) & as.numeric(!!MStagea)) >= 3 ~ "Rule3",
        !!IMorNoIMa == "No_IM" & !is.na(!!CStagea) & as.numeric(!!CStagea) < 3 ~ "Rule1",
        !!IMorNoIMa == "IM" & !is.na(!!CStagea) & as.numeric(!!CStagea) < 3 ~ "Rule2",
        (!is.na(!!CStagea) & as.numeric(!!CStagea)) >= 3 ~ "Rule3",
        TRUE ~ "NoRules"
      )
    )

  return(df$FU_Type)
}



#' Run all the basic Barrett's functions
#'
#' Function to encapsulate all the Barrett's functions together. This includes the Prague
#' score and the worst pathological grade and then feeds both of these things into
#' the follow up function. The output is a dataframe with all the original data as
#' well as the new columns that have been created.
#' @param Endodataframe endoscopy dataframe of interest
#' @param EndoReportColumn Endoscopy report field of interest as a string vector
#' @param EndoReportColumn2 Second endoscopy report field of interest as a string vector
#' @param Pathdataframe pathology dataframe of interest
#' @param PathColumn Pathology report field of interest as a string vector
#' @keywords Does something with data
#' @importFrom dplyr case_when
#' @export
#' @return Newdf
#' @family Disease Specific Analysis - Barretts Data
#' @examples
#' Barretts_df <- BarrettsAll(Myendo, "Findings", "OGDReportWhole", Mypath, "Histology")
BarrettsAll <- function(Endodataframe, EndoReportColumn, EndoReportColumn2, Pathdataframe, PathColumn) {
  Newdf <- Barretts_PragueScore(Endodataframe, EndoReportColumn, EndoReportColumn2)
  Newdf$IMorNoIM <- Barretts_PathStage(Pathdataframe, PathColumn)
  # The named columns here are derived from the previous functions outputs
  Newdf$FU_Type <- Barretts_FUType(Newdf, "CStage", "MStage", "IMorNoIM")
  return(Newdf)
}


#' Run the Paris classification versus worst histopath grade for Barrett's
#'
#' This creates a column of Paris grade for all samples where this is mentioned.
#' @param Column Endoscopy report field of interest as a string vector
#' @param Column2 Another endoscopy report field of interest as a string vector
#' @keywords Does something with data
#' @importFrom dplyr case_when
#' @export
#' @return a string vector
#' @family Disease Specific Analysis - Barretts Data
#' @examples # 
#' Myendo$EMR<-BarrettsParisEMR(Myendo$ProcedurePerformed,Myendo$Findings)


BarrettsParisEMR <- function(Column, Column2) {
  NewCol <- paste0(Column, Column2)
  NewCol <- data.frame(NewCol, stringsAsFactors = FALSE)

  # Get the worst pathology for that sample inc SM stages
  df <- NewCol %>%
    mutate(
      ParisClass = case_when(
        grepl("11a_c|2a_c|[Ii][Ii]a_c", NewCol, ignore.case = TRUE) ~ "2a_c",
        grepl("[Ii][Ii]a|2a|11a", NewCol, ignore.case = TRUE) ~ "2a",
        grepl("[Ii][Ii]b|2b|11b", NewCol, ignore.case = TRUE) ~ "2b",
        grepl("[Ii][Ii][Ii]|III", NewCol, ignore.case = TRUE) ~ "3",
        grepl("Paris [Tt]ype [Ii]s|1s ", NewCol, ignore.case = TRUE, perl = TRUE) ~ "1s",
        grepl(" [Ii]p |1p", NewCol, ignore.case = TRUE, perl = TRUE) ~ "1p",
        TRUE ~ "No_Paris"
      )
    )

  return(df$ParisClass)
}



############## Pathology Quality #############


#' Get the number of Barrett's biopsies taken
#'
#' This function gets the number of biopsies taken per 
#' endoscopy and compares it to the
#' Prague score for that endoscopy.Endoscopists should be taking a certain
#' number of biopsies given the length of a Barrett's segment so it
#' should be straightforward to detect a shortfall in the number
#' of biopsies being taken. The output is the shortfall per endoscopist
#' @param dataframe dataframe
#' @param Endo_ResultPerformed Date of the Endoscopy
#' @param PatientID Patient's unique identifier
#' @param Endoscopist name of the column with the Endoscopist names
#' @importFrom dplyr summarise group_by filter
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot geom_point labs theme xlab ylab unit element_text
#' @keywords Does something with data
#' @export
#' @family Disease Specific Analysis - Barretts Data
#' @examples
#' # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' Mypath$NumBx <- HistolNumbOfBx(Mypath$Macroscopicdescription, "specimen")
#' Mypath$BxSize <- HistolBxSize(Mypath$Macroscopicdescription)
#' 
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v <- Endomerge2(
#'   Myendo, "Dateofprocedure", "HospitalNumber", Mypath, "Dateofprocedure",
#'   "HospitalNumber"
#' )
#' 
#' # The function relies on the other Barrett's functions being run as well:
#' b1 <- Barretts_PragueScore(v, "Findings")
#' b1$PathStage <- Barretts_PathStage(b1, "Histology")
#' 
#' # The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' b1$FU_Type <- Barretts_FUType(b1, "CStage", "MStage", "PathStage")
#' 
#' 
#' colnames(b1)[colnames(b1) == "pHospitalNum"] <- "HospitalNumber"
#' # The number of average number of biopsies is then calculated and
#' # compared to the average Prague C score so that those who are taking
#' # too few biopsies can be determined
#' hh <- BarrettsBxQual(
#'   b1, "Date.x", "HospitalNumber",
#'   "Endoscopist"
#' )
#' rm(v)
BarrettsBxQual <- function(dataframe,
                           Endo_ResultPerformed,
                           PatientID,
                           Endoscopist) {
  dataframe <- data.frame(dataframe)
  PatientIDa <- rlang::sym(PatientID)
  Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)
  Endoscopista <- rlang::sym(Endoscopist)

  # Make sure the C and M stage is mueric (wil be character from the PragueScore function to
  # incorporate "Insufficient " as an outcome)
  dataframe$CStage <- as.numeric(dataframe$CStage)
  dataframe$MStage <- as.numeric(dataframe$MStage)

  GroupedByEndoscopy <-
    suppressWarnings(dataframe %>%
      filter(!is.na(CStage), !is.na(NumBx)) %>%
      group_by(
        as.Date(!!Endo_ResultPerformeda), !!PatientID,
        !!Endoscopista
      ) %>%
      summarise(Sum = sum(NumBx), AvgC = mean(CStage)))

  GroupedByEndoscopy$ExpectedNumber <-
    (GroupedByEndoscopy$AvgC + 1) * 2
  GroupedByEndoscopy$Difference <-
    GroupedByEndoscopy$Sum - GroupedByEndoscopy$ExpectedNumber

  # Now group the difference by endoscopist
  BxShortfallPre <-
    GroupedByEndoscopy %>%
    group_by(!!Endoscopista) %>%
    summarise(MeanDiff = round(mean(Difference),2))

  BxShortfallPre <- data.frame(BxShortfallPre)
  return(BxShortfallPre)
}
