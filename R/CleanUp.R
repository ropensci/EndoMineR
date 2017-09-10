if (getRversion() >= "2.15.1") utils::globalVariables(c("PatientID", ".SD", "CStage", "NumbOfBx", 
    "Years", "Difference", "barplot", "head", "read.table", "eHospitalNum", "pHospitalNum", ".", 
    "EVENT", "MonthYear", "freq", "Endoscopist", "avg", "v", "destination", "dcast", "complete.cases", 
    "g", "gvisSankey", "head", "pHospitalNum", "par", "plot", "r", "read.table", "region", "rgb", 
    "setDT"))


############## Endoscopy Clean-up functions##############

# Title:CleanUp Aim: This script comprises the functions that I use to clean up endoscopic
# reports and pathology reports.  At the moment the script assumes that I am provided with
# the following column headings: EndoResultPerformed (the date of the endoscopy). I usually
# get another date called EndoResultEntered but this can be ignored. This is also true of
# histology The date of the histology and endoscopy is likely to be generic for all reports.
# I also get the Report text which has all of the other information in it.  The scripts
# assume the endoscopy and histopathology data set is merged already but it can also be used
# of course with the unmerged datasets The functions for both endoscopy and histology really
# rely heavily on regular expressions. At the moment I don't know whether these are specific
# to GSTT or whether they can be used for other Trusts as well. I need to get example reports
# from other Trusts to find this out.

# There are two stages to improvements in this script to make it more generic:

# 1. Discover the structure of raw information from other trusts so I can determine
# commonalities in the electronically stored data 2. Improve the regex to make it less
# repetitive




# ENDOSCOPY CLEAN UP FUNCTION
#' EndoscChopperNewLines
#'
#' Cleans the long lines to separate them into new lines if necessary
#' @param x dataframe
#' @param y The endoscopy report text column
#' @keywords Endoscopy Newlines
#' @export
#' @examples v<-TheOGDReportFinal
#' EndoscTree<-list('Hospital Number:','Patient Name:','General Practitioner:',
#' 'Date of procedure:','Endoscopist:','Endoscopist:','Medications','Instrument',
#' 'Extent of Exam:','Indications:','Procedure Performed:','Findings:','Endoscopic Diagnosis:')
#' v<-ChopperNewLines(v,'OGDReportWhole')


ChopperNewLines <- function(x, y) {
    x <- data.frame(x)
    # Separate long lines with empty space into new lines
    x[, y] <- gsub("    ", "\n", x[, y])
    x[, y] <- gsub("(\n|\r){2,8}", "\\.", x[, y])
    x[, y] <- gsub("(\n|\r)", "\\.", x[, y])
    return(x)
}



#' Extractor
#'
#' This is the main extractor for the Endoscopy and Histology report
#' @param x the dataframe 
#' @param y the column to extract from
#' @param stra the start of the boundary to extract
#' @param strb the end of the boundary to extract
#' @param t the column name to create
#' @import stringr
#' @keywords Extraction
#' @export
#' @examples v<-TheOGDReportFinal
#' Myendo<-TheOGDReportFinal
#' Myendo$OGDReportWhole<-gsub("2nd Endoscopist:","Second endoscopist:",Myendo$OGDReportWhole)
#' EndoscTree<-list("Hospital Number:","Patient Name:","General Practitioner:",
#' "Date of procedure:","Endoscopist:","Second Endoscopist:","Medications",
#' "Instrument","Extent of Exam:","Indications:","Procedure Performed:","Findings:",
#' "Endoscopic Diagnosis:")
#' for(i in 1:(length(EndoscTree)-1)) {
#'  Myendo<-Extractor(Myendo,"OGDReportWhole",as.character(EndoscTree[i]),
#'  as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
#' }
#' Myendo


Extractor <- function(x, y, stra, strb, t) {
    x <- data.frame(x)
    t <- gsub("[^[:alnum:],]", " ", t)
    t <- gsub(" ", "", t, fixed = T)
    x[, t] <- stringr::str_extract(x[, y], stringr::regex(paste(stra, "(.*)", strb, sep = ""), 
        dotall = TRUE))
    
    
    x[, t] <- gsub("\\\\.*", "", x[, t])
    names(x[, t]) <- gsub(".", "", names(x[, t]), fixed = T)
    x[, t] <- gsub("       ", "", x[, t])
    x[, t] <- gsub(stra, "", x[, t], fixed = TRUE)
    if (strb != "") {
        x[, t] <- gsub(strb, "", x[, t], fixed = TRUE)
    }
    x[, t] <- gsub("       ", "", x[, t])
    x[, t] <- ColumnCleanUp(x, t)
    x[, t] <- trimws(x[, t])
    
    
    return(x)
}



#' EndoscChopperEndoscopist
#'
#' This extracts the endoscopist from the report
#' @param x dataframe 
#' @param y The endoscopy text column
#' @keywords Endoscopist extraction
#' @export 
#' @examples v<-TheOGDReportFinal
#' Myendo<-TheOGDReportFinal
#' Myendo$OGDReportWhole<-gsub("2nd Endoscopist:","Second endoscopist:",Myendo$OGDReportWhole)
#' EndoscTree<-list("Hospital Number:","Patient Name:","General Practitioner:",
#' "Date of procedure:","Endoscopist:","Second Endoscopist:","Medications",
#' "Instrument","Extent of Exam:","Indications:","Procedure Performed:","Findings:",
#' "Endoscopic Diagnosis:")
#' for(i in 1:(length(EndoscTree)-1)) {
#'  Myendo<-Extractor(Myendo,"OGDReportWhole",as.character(EndoscTree[i]),
#'  as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
#' }
#' v$Endo_ResultPerformed<-as.Date(v$Endo_ResultPerformed,format='%d/%m/%Y')
#' v<-EndoscChopperEndoscopist(v,'Endoscopist')

EndoscChopperEndoscopist <- function(x, y) {
    # Extraction of the Endoscopist
    x <- data.frame(x)
    x[, y] <- gsub("Dr", "", x[, y], fixed = TRUE)
    x[, y] <- gsub("Mr", "", x[, y], fixed = TRUE)
    x[, y] <- gsub("[^[:alnum:],]", "", x[, y])
    # Put gaps between names
    x[, y] <- gsub("([a-z])([A-Z])", "\\1 \\2", x[, y])
    x[, y] <- gsub("2nd.*", "", x[, y])
    x[, y] <- trimws(x[, y], which = c("both"))
    
    return(x)
}

#' EndoscChopperMeds
#'
#' This cleans the medications from the report
#' @param x dataframe with column of interest 
#' @param y column of interest
#' @keywords Endoscopy medications
#' @import stringr
#' @export
#' @examples v<-TheOGDReportFinal
#' Myendo<-TheOGDReportFinal
#' Myendo$OGDReportWhole<-gsub("2nd Endoscopist:","Second endoscopist:",Myendo$OGDReportWhole)
#' EndoscTree<-list("Hospital Number:","Patient Name:","General Practitioner:",
#' "Date of procedure:","Endoscopist:","Second Endoscopist:","Medications",
#' "Instrument","Extent of Exam:","Indications:","Procedure Performed:","Findings:",
#' "Endoscopic Diagnosis:")
#' for(i in 1:(length(EndoscTree)-1)) {
#'  Myendo<-Extractor(Myendo,"OGDReportWhole",as.character(EndoscTree[i]),
#'  as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
#' }
#'v<-EndoscChopperMeds(v,'Medications')

EndoscChopperMeds <- function(x, y) {
    # Extraction of the Medications: Extract the fentanyl:
    x$Fent <- stringr::str_extract(x[, y], "Fentanyl\\s*(\\d+)\\s*mcg")
    x$Fent <- gsub("Fentanyl", "", x$Fent)
    x$Fent <- gsub("mcg", "", x$Fent)
    x$Fent <- as.numeric(x$Fent)
    
    # Extract the midazolam
    x$Midaz <- stringr::str_extract(x$Medications, "Midazolam\\s*(\\d+)\\s*mg")
    x$Midaz <- gsub("Midazolam ", "", x$Midaz)
    x$Midaz <- gsub("mg", "", x$Midaz)
    x$Midaz <- as.numeric(x$Midaz)
    return(x)
}


#' EndoscChopperInstrument
#'
#' This cleans the Instrument from the report
#' @param x dataframe with column of interest
#' @param y column of interest
#' @keywords Instrument
#' @export
#' @examples v<-TheOGDReportFinal
#' Myendo<-TheOGDReportFinal
#' Myendo$OGDReportWhole<-gsub("2nd Endoscopist:","Second endoscopist:",Myendo$OGDReportWhole)
#' EndoscTree<-list("Hospital Number:","Patient Name:","General Practitioner:",
#' "Date of procedure:","Endoscopist:","Second Endoscopist:","Medications",
#' "Instrument","Extent of Exam:","Indications:","Procedure Performed:","Findings:",
#' "Endoscopic Diagnosis:")
#' for(i in 1:(length(EndoscTree)-1)) {
#'  Myendo<-Extractor(Myendo,"OGDReportWhole",as.character(EndoscTree[i]),
#'  as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
#' }
#'v<-EndoscChopperInstrument(v,'Instrument')

EndoscChopperInstrument <- function(x, y) {
    # Extraction of the Instrument used:
    
    x[, y] <- gsub("-.*", "", x[, y])
    x[, y] <- gsub("X.*[Ll][Oo[Aa][Nn] [Ss][Cc][Oo][Pp][Ee] \\(|Loan Scope \\(specify serial no:|Loan Scope \\(specify\\s*serial no|\\)|-.*", 
        "", x[, y])
    x[, y] <- gsub(",.*|:|FC |[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee] |^,", "", x[, y])
    x[, y] <- gsub("FC ", "FC", x[, y])
    x[, y] <- gsub("^\\s*([1-9])", "A\\1", x[, y])
    x[, y] <- gsub("[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee] \\(specify serial no\\)\\s*", "", 
        x[, y])
    x[, y] <- gsub("[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee] \\(specify serial no:\\)\\s*", "", 
        x[, y])
    x[, y] <- toupper(x[, y])
    x[, y] <- trimws(x[, y])
    return(x)
}

#' EndoscChopperIndications
#'
#' This cleans the Indication from the report
#' @param x dataframe with column of interest 
#' @param y column of interest
#' @keywords Indications
#' @export
#' @examples v<-TheOGDReportFinal
#' Myendo<-TheOGDReportFinal
#' Myendo$OGDReportWhole<-gsub("2nd Endoscopist:","Second endoscopist:",Myendo$OGDReportWhole)
#' EndoscTree<-list("Hospital Number:","Patient Name:","General Practitioner:",
#' "Date of procedure:","Endoscopist:","Second Endoscopist:","Medications",
#' "Instrument","Extent of Exam:","Indications:","Procedure Performed:","Findings:",
#' "Endoscopic Diagnosis:")
#' for(i in 1:(length(EndoscTree)-1)) {
#'  Myendo<-Extractor(Myendo,"OGDReportWhole",as.character(EndoscTree[i]),
#'  as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
#' }
#'v<-EndoscChopperIndications(v,'Indications')

EndoscChopperIndications <- function(x, y) {
    # Extraction of the Indications for examination eg chest pain/ dysphagia etc.
    x[, y] <- gsub("\r\n", "\n", x[, y])
    x[, y] <- gsub("\\.\n\\.\n|\\.\r\\.\r", "\\.", x[, y])
    return(x)
    
}


#' EndoscChopperProcPerformed
#'
#' This cleans the procedure performed from the report
#' @param x dataframe with column of interest
#' @param y column of interest
#' @keywords Procedure 
#' @export
#' @examples v<-TheOGDReportFinal
#' Myendo<-TheOGDReportFinal
#' Myendo$OGDReportWhole<-gsub("2nd Endoscopist:","Second endoscopist:",Myendo$OGDReportWhole)
#' EndoscTree<-list("Hospital Number:","Patient Name:","General Practitioner:",
#' "Date of procedure:","Endoscopist:","Second Endoscopist:","Medications",
#' "Instrument","Extent of Exam:","Indications:","Procedure Performed:","Findings:",
#' "Endoscopic Diagnosis:")
#' for(i in 1:(length(EndoscTree)-1)) {
#'  Myendo<-Extractor(Myendo,"OGDReportWhole",as.character(EndoscTree[i]),
#'  as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
#' }
#'v<-EndoscChopperProcPerformed(v,'ProcedurePerformed')

EndoscChopperProcPerformed <- function(x, y) {
    # Extraction of the eg Colonoscopy or gastroscopy etc:
    x[, y] <- gsub("Withdrawal.*", "", x[, y])
    x[, y] <- gsub("Quality.*", "", x[, y])
    x[, y] <- gsub("Adequate.*|Good.*|Poor.*|None.*", "", x[, y])
    x[, y] <- gsub("FINDINGS", "", x[, y])
    x[, y] <- gsub("-\\s*$|-$|-\\s+$", "", x[, y])
    x[, y] <- gsub("([A-Z])-", "\\1 -", x[, y])
    x[, y] <- gsub("\\.", "", x[, y])
    x[, y] <- gsub("-([A-Z])", "-\\1", x[, y])
    x[, y] <- gsub(")-", ") -", x[, y])
    return(x)
}


#' EndoscChopperFindings
#'
#' This cleans the Findings performed from the report
#' @param x dataframe with column of interest
#' @param y column of interest
#' @keywords Procedure 
#' @export
#' @examples v<-TheOGDReportFinal
#' Myendo<-TheOGDReportFinal
#' Myendo$OGDReportWhole<-gsub("2nd Endoscopist:","Second endoscopist:",Myendo$OGDReportWhole)
#' EndoscTree<-list("Hospital Number:","Patient Name:","General Practitioner:",
#' "Date of procedure:","Endoscopist:","Second Endoscopist:","Medications",
#' "Instrument","Extent of Exam:","Indications:","Procedure Performed:","Findings:",
#' "Endoscopic Diagnosis:")
#' for(i in 1:(length(EndoscTree)-1)) {
#'  Myendo<-Extractor(Myendo,"OGDReportWhole",as.character(EndoscTree[i]),
#'  as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
#' }
#'v<-EndoscChopperFindings(v,'Findings')

EndoscChopperFindings <- function(x, y) {
    # Extraction of the FINDINGS
    x[, y] <- gsub("cm\\s+[A-Z]|cm.+\\)", "cm\n", x[, y])
    return(x)
}

####### General Clean-Up functions #####


# Extraction of the Negative sentences so that normal findings can be removed and not counted
# when searching for true diseases. eg remove No evidence of candidal infection so it doesn't
# get included if looking for candidal infections.

#' NegativeRemove
#'
#' This extracts removes negative diagnoses from the report
#' @param x dataframe with column of interest
#' @param y column of interest
#' @keywords Negative Sentences
#' @export
#' @examples x[, y] <- NegativeRemove(x[, y])

NegativeRemove <- function(x, y) {
    
    x <- (data.frame(x))
    x[, y] <- gsub("No .*?(\\.|:)", "", x[, y])
    x[, y] <- gsub("[Nn]o .*\n", "", x[, y], perl = T)
    x[, y] <- gsub("[Nn]o [Dd]ysplasia.*?\\.", "", x[, y], perl = T)
    x[, y] <- gsub(".*[Nn]ormal.*?\\.|:", "", x[, y])
    x[, y] <- gsub(".*[Nn]ormal.*\n", "", x[, y], perl = T)
    x[, y] <- gsub("^[Nn]ormal.*\n", "", x[, y], perl = T)
    x[, y] <- gsub(".*[Nn]either .*?(\\.|:)", "", x[, y])
    x[, y] <- gsub(".*[Tt]here is no .*?(\\.|:)", "", x[, y])
    x[, y] <- gsub(".*[Tt]here are no .*?(\\.|:)", "", x[, y])
    x[, y] <- gsub(".*[Nn]egative.*?(\\.|:)", "", x[, y])
    x[, y] <- gsub("[Nn]egative for.*\n", "", x[, y], perl = T)
    x[, y] <- gsub("[Nn]egative for [Dd]ysplasia", "", x[, y], perl = T)
    x[, y] <- gsub("[Nn]egative.*\n", "", x[, y], perl = T)
    x[, y] <- gsub("[Nn]egative for.*?\\.", "", x[, y], perl = T)
    x[, y] <- gsub(".*[Ww]ithin normal .*?(\\.|:)", "", x[, y])
    x[, y] <- gsub("[Ww]ithin normal histol.*\n", "", x[, y], perl = T)
    x[, y] <- gsub("[Ww]ithin normal .*\n", "", x[, y], perl = T)
    x[, y] <- gsub(".*with no.*?(\\.|:)", "", x[, y])
    x[, y] <- gsub(".*[Nn]o significant.*?(\\.|:)", "", x[, y])
    x[, y] <- gsub("Neither dysplasia.*?\\.", "", x[, y], perl = T)
    x[, y] <- gsub("Neither dysplasia nor malignancy is seen", "", x[, y], perl = T)
}


#' ColumnCleanUp
#'
#' This does a general clean up of whitespace,semi-colons,full stops at the start
#' of lines and converts end sentence full stops to new lines
#' @param x dataframe with column of interest
#' @param y column of interest
#' @keywords Cleaner
#' @export
#' @examples  x[, y] <- ColumnCleanUp(x, y)

ColumnCleanUp <- function(x, y) {
    x <- (data.frame(x))
    x[, y] <- gsub("^\\.\n", "", x[, y])
    x[, y] <- gsub("^:", "", x[, y])
    x[, y] <- gsub(".", "\n", x[, y], fixed = TRUE)
    x[, y] <- gsub("\\s{5}", "", x[, y])
    x[, y] <- gsub("^\\.", "", x[, y])
    x[, y] <- gsub("$\\.", "", x[, y])
}

####### Histology Clean Up functions #######

#' HistolChopperMacDescripCleanup
#'
#' This extracts Macroscopic description data from the report
#' @param x dataframe with column of interest
#' @param y column of interest
#' @keywords Macroscopic
#' @export
#' @examples x <- HistolChopperMacDescripCleanup(x, y)


HistolChopperMacDescripCleanup <- function(x, y) {
    x <- data.frame(x)
    # Column specific cleanup
    x[, y] <- gsub("Dictated by.*", "", x[, y])
    return(x)
}


#' HistolChopperHistol
#'
#' This extracts Histology details data from the report
#' @param x dataframe with column of interest
#' @param y column of interest
#' @keywords Histology
#' @export
#' @examples HistolChopperHistol(x,'Histo_ResultText')

HistolChopperHistol <- function(x, y) {
    # HISTOLOGY
    x[, y] <- gsub("\n|\r", " ", x[, y])
    x[, y] <- NegativeRemove(x[, y])
    x$Histol_Simplified <- x[, y]
    # Negative extraction- may merge this with the function NegativeRemove() above and some of
    # the phrases below could undoubetdly be simplified with more intelligent regex
    x$Histol_Simplified <- gsub("- ", "\n", x$Histol_Simplified, fixed = TRUE)
    x$Histol_Simplified <- gsub("-[A-Z]", "\n", x$Histol_Simplified, fixed = TRUE)
    x$Histol_Simplified <- gsub(".*biopsies.*\n", "", x$Histol_Simplified, perl = T)
    x$Histol_Simplified <- gsub(".*biopsy.*\n", "", x$Histol_Simplified, perl = T)
    x$Histol_Simplified <- gsub(":", "", x$Histol_Simplified, perl = T)
    x$Histol_Simplified <- gsub("[Nn]egative.*\n", "", x$Histol_Simplified, perl = T)
    x$Histol_Simplified <- gsub("[Ww]ithin normal .*\n", "", x$Histol_Simplified, perl = T)
    x$Histol_Simplified <- gsub("[Nn]o .*\n", "", x$Histol_Simplified, perl = T)
    x$Histol_Simplified <- gsub(".*[Nn]ormal.*\n", "", x$Histol_Simplified, perl = T)
    x$Histol_Simplified <- gsub("^[Nn]ormal.*\n", "", x$Histol_Simplified, perl = T)
    x$Histol_Simplified <- gsub("[Ww]ithin normal histol.*\n", "", x$Histol_Simplified, perl = T)
    x$Histol_Simplified <- gsub("[Nn]egative for.*\n", "", x$Histol_Simplified, perl = T)
    x$Histol_Simplified <- gsub("[Nn]egative for [Dd]ysplasia", "", x$Histol_Simplified, perl = T)
    x$Histol_Simplified <- gsub("[Nn]o [Dd]ysplasia.*?\\.", "", x$Histol_Simplified, perl = T)
    x$Histol_Simplified <- gsub("[Nn]egative for.*?\\.", "", x$Histol_Simplified, perl = T)
    x$Histol_Simplified <- gsub("Neither dysplasia.*?\\.", "", x$Histol_Simplified, perl = T)
    x$Histol_Simplified <- gsub("Neither dysplasia nor malignancy is seen", "", x$Histol_Simplified, 
        perl = T)
    return(x)
}



#' HistolChopperAccessionNumber
#'
#' This extracts Accession Number data data from the report
#' @param x the dataframe name and 
#' @param  y the column name as a string. 
#' @param  stra regular expression needed as a string
#' @import stringr
#' @keywords Sample Accession number
#' @export
#' @examples v<-PathDataFrameFinalColon
#' Histoltree<-list("Hospital Number:","Patient Name:","General Practitioner:",
#' "Date of procedure:","Clinical Details","Nature of specimen",
#' "Histology","Diagnosis","")
#' for(i in 1:(length(Histoltree)-1)) {
#'   Mypath<-Extractor(Mypath,"PathReportWhole",as.character(Histoltree[i]),
#'   as.character(Histoltree[i+1]),gsub(" ","",as.character(Histoltree[i])))
#' }
#' v<-HistolChopperAccessionNumber(v,"Histo_ResultText","SP-\\d{2}-\\d{7}")

HistolChopperAccessionNumber <- function(x, y, stra) {
    x <- data.frame(x)
    # Accession number samples- not really necessary to extract:
    x$AccessionNumber <- stringr::str_extract(x[, y], stra)
    return(x)
}

#' HistolChopperDx
#'
#' This extracts Diagnosis data from the report
#' @param x the dataframe
#' @param y column containing the Hisopathology report
#' @keywords Histology Diagnosis
#' @export
#' @examples v<-PathDataFrameFinalColon
#' Histoltree<-list("Hospital Number:","Patient Name:","General Practitioner:",
#' "Date of procedure:","Clinical Details","Nature of specimen",
#' "Histology","Diagnosis","")
#' for(i in 1:(length(Histoltree)-1)) {
#'   Mypath<-Extractor(Mypath,"PathReportWhole",as.character(Histoltree[i]),
#'   as.character(Histoltree[i+1]),gsub(" ","",as.character(Histoltree[i])))
#' }
#' v<-HistolChopperDx(v,'Diagnosis')

HistolChopperDx <- function(x, y) {
    # Without the negative extractor which needs some improvement. Only Capital D included (not
    # lower case d) to make sure picks up subtitle header as opposed to mentioning 'diagnosis' as
    # part of a sentence.  Column specific cleanup
    x[, y] <- gsub("Dr.*", "", x[, y], perl = T)
    x[, y] <- gsub("[Rr]eported.*", "", x[, y])
    # Column-generic cleanup
    x[, y] <- ColumnCleanUp(x, y)
    x[, y] <- NegativeRemove(x, y)
    x$Dx_Simplified <- x[, y]
    x$Dx_Simplified <- gsub("- ", "\n", x$Dx_Simplified, fixed = TRUE)
    x$Dx_Simplified <- gsub("-[A-Z]", "\n", x$Dx_Simplified, fixed = TRUE)
    x$Dx_Simplified <- gsub(".*biopsies.*\n", "", x$Dx_Simplified, perl = T)
    x$Dx_Simplified <- gsub(".*biopsy.*\n", "", x$Dx_Simplified, perl = T)
    return(x)
    
}

#' HistolChopperExtrapolDx
#'
#' This extracts other specific diagnoses from the report
#' @param x the dataframe containing histology results, 
#' @param y the column to extract dysplasia, cancer, and GIST from- often the Histology diagnosis column
#' @import stringr
#' @keywords Histology diagnosis
#' @export
#' @examples v<-PathDataFrameFinalColon
#' Histoltree<-list("Hospital Number:","Patient Name:","General Practitioner:",
#' "Date of procedure:","Clinical Details","Nature of specimen",
#' "Histology","Diagnosis","")
#' for(i in 1:(length(Histoltree)-1)) {
#'   Mypath<-Extractor(Mypath,"PathReportWhole",as.character(Histoltree[i]),
#'   as.character(Histoltree[i+1]),gsub(" ","",as.character(Histoltree[i])))
#' }
#' v<-HistolChopperExtrapolDx(v,'Diagnosis')

HistolChopperExtrapolDx <- function(x, y) {
    # Some further extraction to get commonly searched for data
    x$Cancer <- stringr::str_extract(x[, y], "[Cc]arcin|[Cc]ance|[Ll]ymphoma|[Tt]umour")
    x$Dysplasia <- stringr::str_extract(x[, y], "[Dd]yspla")
    x$GIST <- stringr::str_extract(x[, y], "G[Ii][Ss][Tt]|[Ss]tromal|[Ll]eio")
    return(x)
}




#' HistolChopperMacDescrip
#' 
#' This extracts numbers from written (spelt) numbers
#' 
#' @param x dataframe
#' @param y column to extract the numbers from. Usually the column
#' with the Nature of the specimen or the Macroscopic description in it
#' @keywords Macroscopic
#' @export
#' @examples #' @examples v<-PathDataFrameFinalColon
#' Histoltree<-list("Hospital Number:","Patient Name:","General Practitioner:",
#' "Date of procedure:","Clinical Details","Nature of specimen",
#' "Histology","Diagnosis","")
#' for(i in 1:(length(Histoltree)-1)) {
#'   Mypath<-Extractor(Mypath,"PathReportWhole",as.character(Histoltree[i]),
#'   as.character(Histoltree[i+1]),gsub(" ","",as.character(Histoltree[i])))
#' }
#' HistolChopperMacDescrip(Mypath, "Natureofspecimen")

HistolChopperMacDescrip <- function(x, y) {
    x <- data.frame(x)
    # Conversion of text numbers to allow number of biopsies to be extracted
    x[, y] <- gsub("[Oo]ne", "1", x[, y])
    x[, y] <- gsub("[Ss]ingle", "1", x[, y])
    x[, y] <- gsub("[Tt]wo", "2", x[, y])
    x[, y] <- gsub("[Tt]hree", "3", x[, y])
    x[, y] <- gsub("[Ff]our", "4", x[, y])
    x[, y] <- gsub("[Ff]ive", "5", x[, y])
    x[, y] <- gsub("[Ss]ix", "6", x[, y])
    x[, y] <- gsub("[Ss]even", "7", x[, y])
    x[, y] <- gsub("[Ee]ight", "8", x[, y])
    return(x)
}

#' HistolChopperNumbOfBx
#'
#' This extracts the number of biopsies taken from the pathology report. This is usually from the Macroscopic description column.
#' It collects everything from the regex [0-9]{1,2}.{0,3} to whatever the string boundary is (z)
#' @param x the dataframe
#' @param y Column containing the Macroscopic description text
#' @param z The keyword to remove and to stop at in the regex
#' @import stringr
#' @keywords Biopsy number
#' @export
#' @examples v<-PathDataFrameFinalColon
#' Histoltree<-list("Hospital Number:","Patient Name:","General Practitioner:",
#' "Date of procedure:","Clinical Details","Nature of specimen",
#' "Histology","Diagnosis","")
#' for(i in 1:(length(Histoltree)-1)) {
#'   Mypath<-Extractor(Mypath,"PathReportWhole",as.character(Histoltree[i]),
#'   as.character(Histoltree[i+1]),gsub(" ","",as.character(Histoltree[i])))
#' }
#' v<-HistolChopperNumbOfBx(v,'Natureofspecimen','specimen')

HistolChopperNumbOfBx <- function(x, y, z) {
    x <- data.frame(x)
    x <- HistolChopperMacDescrip(x, y)
    mylist <- str_match_all(x[, y], paste("[0-9]{1,2}.{0,3}", z, sep = ""))
    x$NumbOfBx <- sapply(mylist, function(p) sum(as.numeric(gsub(z, "", p))))
    return(x)
}

#' HistolChopperBxSize
#'
#' This extracts the biopsy size from the report.This is usually from the Macroscopic description column.
#' @param x the dataframe
#' @param y Macdescrip
#' @import stringr
#' @keywords biopsy size
#' @export
#' @examples v<-PathDataFrameFinalColon
#' Histoltree<-list("Hospital Number:","Patient Name:","General Practitioner:",
#' "Date of procedure:","Clinical Details","Nature of specimen",
#' "Histology","Diagnosis","")
#' for(i in 1:(length(Histoltree)-1)) {
#'   Mypath<-Extractor(Mypath,"PathReportWhole",as.character(Histoltree[i]),
#'   as.character(Histoltree[i+1]),gsub(" ","",as.character(Histoltree[i])))
#' }
#' v<-HistolChopperBxSize(v,'Natureofspecimen')

HistolChopperBxSize <- function(x, y) {
    # What's the average biopsy size this month?
    x$BxSize <- stringr::str_extract(x[, y], "the largest.*?mm")
    x$BxSize <- gsub("the largest measuring ", "", x$BxSize)
    x$BxSize <- gsub("mm", "", x$BxSize)
    x$BxSize <- gsub("less than", "", x$BxSize)
    strBxSize <- "([0-9]+).*?([0-9])+.*?([0-9])"
    x$BxSize <- as.numeric(stringr::str_match(x$BxSize, strBxSize)[, 2]) * as.numeric(stringr::str_match(x$BxSize, 
        strBxSize)[, 3]) * as.numeric(stringr::str_match(x$BxSize, strBxSize)[, 4])
    return(x)
}

