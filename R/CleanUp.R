if (getRversion() >= "2.15.1") utils::globalVariables(c("PatientID", ".SD", 
        "CStage", "NumbOfBx", "Years", "Difference", "barplot", "head", 
        "read.table", "eHospitalNum", "pHospitalNum", ".", "EVENT", 
    "MonthYear", "freq", "Endoscopist", "avg", "v", "destination", 
    "dcast", "complete.cases", "g", "gvisSankey", "head", 
    "pHospitalNum", "par", "plot", "r", "read.table", 
    "region", "rgb", "setDT"))


############## Endoscopy Clean-up functions##############

#' EndoscChopperNewLines
#'
#' Cleans the long lines to separate them into new lines if necessary. 
#' This helps further down the line in the tokenisation of the text. 
#' It is optional depending on the format of your text. It should be used 
#' after the Extractor has separated out the different
#' parts of the text according to the user's requirements
#' @param x dataframe
#' @param y The endoscopy report text column
#' @keywords Endoscopy Newlines
#' @export
#' @examples v<-ChopperNewLines(Myendo,'OGDReportWhole')


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
#' This is the main extractor for the Endoscopy and Histology report.
#' This depends on the user creating a list of words or characters that 
#' act as the words that should be split against. The list is then fed to the 
#' Extractor in a loop so that it acts as the beginning and the end of the 
#' regex used to split the text. Whatever has been specified in the list 
#' is used as a column header. Column headers don't tolerate special characters 
#' like : or ? and / and don't allow numbers as the start character so these 
#' have to be dealt with in the text before processing
#' 
#' @param x the dataframe 
#' @param y the column to extract from
#' @param stra the start of the boundary to extract
#' @param strb the end of the boundary to extract
#' @param t the column name to create
#' @importFrom stringr str_extract
#' @keywords Extraction
#' @export
#' @examples v<-TheOGDReportFinal
#' Myendo<-TheOGDReportFinal
#' Myendo$OGDReportWhole<-gsub('2nd Endoscopist:','Second endoscopist:',
#' Myendo$OGDReportWhole)
#' EndoscTree<-list('Hospital Number:','Patient Name:','General Practitioner:',
#' 'Date of procedure:','Endoscopist:','Second Endoscopist:','Medications',
#' 'Instrument','Extent of Exam:','Indications:','Procedure Performed:',
#' 'Findings:','Endoscopic Diagnosis:')
#' for(i in 1:(length(EndoscTree)-1)) {
#'  Myendo<-Extractor(Myendo,'OGDReportWhole',as.character(EndoscTree[i]),
#'  as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
#' }
#' res<-Myendo


Extractor <- function(x, y, stra, strb, t) {
    x <- data.frame(x)
    t <- gsub("[^[:alnum:],]", " ", t)
    t <- gsub(" ", "", t, fixed = TRUE)
    x[, t] <- stringr::str_extract(x[, y], stringr::regex(paste(stra, 
             "(.*)", strb, sep = ""), dotall = TRUE))
    x[, t] <- gsub("\\\\.*", "", x[, t])
    
    names(x[, t]) <- gsub(".", "", names(x[, t]), fixed = TRUE)
    x[, t] <- gsub("       ", "", x[, t])
    x[, t] <- gsub(stra, "", x[, t], fixed = TRUE)
    if (strb != "") {
        x[, t] <- gsub(strb, "", x[, t], fixed = TRUE)
    }
    x[, t] <- gsub("       ", "", x[, t])
    x[, t] <- ColumnCleanUp(x, t)
    return(x)
}



#' EndoscChopperEndoscopist
#'
#' If an endoscopist column is part of the dataset once the extractor 
#' function has been used this cleans the endoscopist column from the report. 
#' It gets rid of titles
#' It gets rid of common entries that are not needed.
#' It should be used after the Extractor and the optional ChopperNewLines 
#' has been used.
#' 
#' @param x dataframe 
#' @param y The endoscopy text column
#' @keywords Endoscopist extraction
#' @export 
#' @examples v<-EndoscChopperEndoscopist(Myendo,'Endoscopist')

EndoscChopperEndoscopist <- function(x, y) {
    # Extraction of the Endoscopist
    x <- data.frame(x)
    x[, y] <- gsub("Dr", "", x[, y], fixed = TRUE)
    x[, y] <- gsub("Mr", "", x[, y], fixed = TRUE)
    x[, y] <- gsub("Professor|Prof", "", x[, y], fixed = TRUE)
    x[, y] <- gsub("[^[:alnum:],]", "", x[, y])
    # Put gaps between names
    x[, y] <- gsub("([a-z])([A-Z])", "\\1 \\2", x[, y])
    x[, y] <- gsub("2nd.*", "", x[, y])
    x[, y] <- trimws(x[, y], which = c("both"))
    
    return(x)
}

#' EndoscChopperMeds
#'
#' This cleans medication column from the report assuming such a column exists. 
#' It gets rid of common entries that are not needed. It also splits the 
#' medication into fentanyl and midazolam doses for use in the global rating 
#' scale tables later. It should be used after the Extractor and the optional 
#' ChopperNewLines has been used.
#' @param x dataframe with column of interest 
#' @param y column of interest
#' @keywords Endoscopy medications
#' @importFrom stringr str_extract
#' @export
#' @examples v<-EndoscChopperMeds(Myendo,'Medications')

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
#' This cleans Instument column from the report assuming such a column exists
#' (where instrument usually refers to the endoscope number being used. 
#' It gets rid of common entries that are not needed.
#' It should be used after the Extractor and the optional 
#' ChopperNewLines has been used.
#' @param x dataframe with column of interest
#' @param y column of interest
#' @keywords Instrument
#' @export
#' @examples v<-EndoscChopperInstrument(Myendo,'Instrument')

EndoscChopperInstrument <- function(x, y) {
    # Extraction of the Instrument used:
    
    x[, y] <- gsub("-.*", "", x[, y])
    x[, y] <- gsub("X.*[Ll][Oo[Aa][Nn] [Ss][Cc][Oo][Pp][Ee] \\(|
                   Loan Scope \\(specify serial no:|
                   Loan Scope \\(specify\\s*serial no|\\)|-.*", "", x[, y])
    x[, y] <- gsub(",.*|:|FC |[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee] |
                   ^,", "", x[, y])
    x[, y] <- gsub("FC ", "FC", x[, y])
    x[, y] <- gsub("^\\s*([1-9])", "A\\1", x[, y])
    x[, y] <- gsub("[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee]
                   \\(specify serial no\\)\\s*", "", x[, y])
    x[, y] <- gsub("[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee] 
                   \\(specify serial no:\\)\\s*", "", x[, y])
    x[, y] <- toupper(x[, y])
    x[, y] <- trimws(x[, y])
    return(x)
}

#' EndoscChopperIndications
#'
#' This cleans the Indication from the report assuming such a column exists. 
#' It largely gets rid of carriage returns especially if used after the 
#' ChopperNewLinesfunction. There may be multiple indications.
#' It should be used after the Extractor and the optional ChopperNewLines has 
#' been used.
#' @param x dataframe with column of interest 
#' @param y column of interest
#' @keywords Indications
#' @export
#' @examples v<-EndoscChopperIndications(Myendo,'Indications')

EndoscChopperIndications <- function(x, y) {
    # Extraction of the Indications for examination 
  # eg chest pain/ dysphagia etc.
    x[, y] <- gsub("\r\n", "\n", x[, y])
    x[, y] <- gsub("\\.\n\\.\n|\\.\r\\.\r", "\\.", x[, y])
    return(x)
    
}


#' EndoscChopperProcPerformed
#'
#' This cleans the Procedure Performed column from the report assuming 
#' such a column exists. Procedure Performed relates to whether this was a 
#' Gastroscopy or Colonoscopy etc.
#' It gets rid of common entries that are not needed.
#' It should be used after the Extractor and the optional ChopperNewLines 
#' has been used.
#' @param x dataframe with column of interest
#' @param y column of interest
#' @keywords Procedure 
#' @export
#' @examples v<-EndoscChopperProcPerformed(Myendo,'ProcedurePerformed')

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
#' This cleans the Procedure Performed column from the report assuming 
#' such a column exists. Findings relates to what was found at the endoscopy
#' This is usually a separate entry to the overall 'Diagnosis' but any
#' are in which the description of the endoscopic findings, including 
#' overall diagnosis or not, can be used.
#' It should be used after the Extractor and the optional ChopperNewLines 
#' has been used.
#' @param x dataframe with column of interest
#' @param y column of interest
#' @keywords Procedure 
#' @export
#' @examples v<-EndoscChopperFindings(Myendo,'Findings')

EndoscChopperFindings <- function(x, y) {
    # Extraction of the FINDINGS
    x[, y] <- gsub("cm\\s+[A-Z]|cm.+\\)", "cm\n", x[, y])
    return(x)
}

####### General Clean-Up functions #####

#' NegativeRemove
#'
#' Extraction of the Negative sentences so that normal findings can be 
#' removed and not counted when searching for true diseases. eg remove 
#' No evidence of candidal infection so it doesn't get included if 
#' looking for candidal infections.
#' It should be used after the Extractor and the optional 
#' ChopperNewLines has been used. It can be used as part of the other functions
#' or as a way of providing a 'positive diagnosis only' type output (see
#' HistolChopperDx)
#' @param x dataframe with column of interest
#' @param y column of interest
#' @keywords Negative Sentences
#' @importFrom stringr str_replace
#' @export
#' @examples anexample<-c("There is no evidence of polyp here",
#' "Although the prep was poor,there was no adenoma found",
#' "The colon was basically inflammed, but no polyp was seen",
#' "The Barrett's segment was not biopsied",
#' "The C0M7 stretch of Barrett's was flat")
#' anexample<-data.frame(anexample)
#' names(anexample)<-"Thecol"
#' NegativeRemove(anexample,"Thecol")

NegativeRemove <- function(x, y) {
    x <- (data.frame(x))
    # Conjunctions
    x[, y] <- gsub("(but|although|however|though|apart|otherwise
                   |unremarkable|\\,)[a-zA-Z0-9_ ]+(no |negative|
unremarkable|-ve|normal).*?(\\.|
                   \\n|:|$)\\R*", "\\.\n", x[, y], 
                   perl = TRUE, ignore.case = TRUE)
    x[, y] <- gsub("(no |negative|unremarkable|-ve| normal) .*?([Bb]ut|
                   [Aa]lthough| [Hh]owever| [Tt]hough| [Aa]part| [Oo]therwise|
                   [Uu]nremarkable)\\R*", "", x[, y], 
                   perl = TRUE, ignore.case = TRUE)
    # Nots
    x[, y] <- gsub(".*(was|were) not.*?(\\.|\n|:|$)\\R*", "", x[, y], 
                   perl = TRUE, ignore.case = TRUE)
    x[, y] <- gsub("not (biop|seen).*?(\\.|\n|:|$)\\R*", "", x[, y], 
                   perl = TRUE, ignore.case = TRUE)
    # Nos
    x[, y] <- gsub(".*(?:((?<!with)|(?<!there is )|(?<!there are ))\\bno\\b
(?![?:A-Za-z])|
                   ([?:]\\s*N?![A-Za-z])).*\\R*", "", x[, y], 
                   perl = TRUE, ignore.case = TRUE)
    x[, y] <- gsub(".*(:|[?])\\s*(\\bno\\b|n)\\s*[^A-Za-z0-9].*?(\\.|\n|:|$)
                   \\R*", "", x[, y], 
                   perl = TRUE, ignore.case = TRUE)
    x[, y] <- gsub(".*(negative|neither).*?(\\.|\n|:|$)\\R*", "", x[, y], 
                   perl = TRUE, ignore.case = TRUE)
    # Keep abnormal in- don't ignore case as it messes it up
    x[, y] <- gsub(".*(?<!b)[Nn]ormal.*?(\\.|\n|:|$)", "", x[, y], 
                   perl = TRUE)
    # Other negatives
    x[, y] <- gsub(".*there (is|are) \\bno\\b .*?(\\.|
                   \n|:|$)\\R*", "", x[, y], perl = TRUE, 
                   ignore.case = TRUE)
    x[, y] <- gsub("(within|with) (normal|\\bno\\b) .*?(\\.|
                   \n|:|$)\\R*", "", x[, y], perl = TRUE, 
                   ignore.case = TRUE)
    # Specific cases
    x[, y] <- gsub(".*duct.*clear.*?(\\.|\n|:|$)\\R*", "", x[, y], 
                   perl = TRUE, ignore.case = TRUE)
    # Unanswered prompt lines
    x[, y] <- gsub(".*:(\\.|\n)\\R*", "", x[, y], 
                   perl = TRUE, ignore.case = TRUE)
    return(x)
}



#' ColumnCleanUp
#'
#' This does a general clean up of whitespace,
#' semi-colons,full stops at the start
#' of lines and converts end sentence full stops to new lines.
#' It should be used after the Extractor and the optional 
#' ChopperNewLines has been used. It can be used as part of the other functions
#' or as a way of providing a 'positive diagnosis only' type output (see
#' HistolChopperDx)
#' @param x dataframe with column of interest
#' @param y column of interest
#' @keywords Cleaner
#' @export
#' @examples

ColumnCleanUp <- function(x, y) {
    x <- (data.frame(x))
    x[, y] <- gsub("^\\.\n", "", x[, y])
    x[, y] <- gsub("^:", "", x[, y])
    x[, y] <- gsub(".", "\n", x[, y], fixed = TRUE)
    x[, y] <- gsub("\\s{5}", "", x[, y])
    x[, y] <- gsub("^\\.", "", x[, y])
    x[, y] <- gsub("$\\.", "", x[, y])
    return(x[, y])
}

####### Histology Clean Up functions #######

#' HistolChopperMacDescripCleanup
#'
#' This extracts Macroscopic description data from the pathology report.
#' Macroscopic description usually relates to the number of specimens
#' retrieved, the size of each specimen and the location it was taken from.
#' The cleanup usually relates to the removal of top and tail characters such
#' as who reported the specimens etc. 
#' @param x dataframe with column of interest
#' @param y column of interest that describes the macroscopic specimen
#' @keywords Macroscopic
#' @export
#' @examples v<-HistolChopperMacDescripCleanup(Mypath,'Macroscopicdescription')


HistolChopperMacDescripCleanup <- function(x, y) {
    x <- data.frame(x)
    # Column specific cleanup
    x[, y] <- gsub("[Dd]ictated by.*", "", x[, y])
    return(x)
}


#' HistolChopperHistol
#'
#' This extracts Histology details data from the report. The Histology details
#' usually relate to the description of the histological report. This implements
#'  the negative remover and also adds further negative removing regexes. This 
#' may be refined in further iterations.
#' @param x dataframe with column of interest
#' @param y column of interest
#' @keywords Histology
#' @export
#' @examples t<-HistolChopperHistol(Mypath,'Histology')


HistolChopperHistol <- function(x, y) {
    # HISTOLOGY
    x[, y] <- gsub("\n|\r", " ", x[, y])
    x[, y] <- NegativeRemove(x[, y])
    x$Histol_Simplified <- x[, y]
    # Negative extraction- may merge this with the function 
    # NegativeRemove() above and some of the 
    #phrases below could undoubetdly be simplified with more intelligent regex
    x$Histol_Simplified <- gsub("- ", "\n", x$Histol_Simplified,
                                fixed = TRUE)
    x$Histol_Simplified <- gsub("-[A-Z]", "\n", x$Histol_Simplified
                                , fixed = TRUE)
    x$Histol_Simplified <- gsub(".*biopsies.*\n", "", x$Histol_Simplified,
                                perl = TRUE)
    x$Histol_Simplified <- gsub(".*biopsy.*\n", "", x$Histol_Simplified,
                                perl = TRUE)
    x$Histol_Simplified <- gsub(":", "", x$Histol_Simplified,
                                perl = TRUE)
    return(x)
}



#' HistolChopperAccessionNumber
#'
#' This extracts Accession Number data data from the report where one is 
#' present. The Accession number relates to the actual specimen number as 
#' ascribed by the pathology service.
#' @param x the dataframe name and 
#' @param  y the column name as a string. 
#' @param  stra regular expression needed as a string
#' @importFrom stringr str_extract
#' @keywords Sample Accession number
#' @export
#' @examples v<-HistolChopperAccessionNumber(Mypath,'Histology',
#' 'SP-\\d{2}-\\d{7}')

HistolChopperAccessionNumber <- function(x, y, stra) {
    x <- data.frame(x)
    # Accession number samples- not really necessary to extract:
    x$AccessionNumber <- stringr::str_extract(x[, y], stra)
    return(x)
}

#' HistolChopperDx
#'
#' This extracts Diagnosis data from the report. The Diagnosis is the overall 
#' impression of the pathologist for that specimen. At the moment, Only Capital 
#' D included (not lower case d) to make sure picks up subtitle header as 
#' opposed to mentioning 'diagnosis' as part of a sentence.  Column specific 
#' cleanup and negative remover have also been implemented here.
#' 
#' @param x the dataframe
#' @param y column containing the Hisopathology report
#' @keywords Histology Diagnosis
#' @export
#' @examples v<-HistolChopperDx(Mypath,'Diagnosis')

HistolChopperDx <- function(x, y) {
    x[, y] <- gsub("Dr.*", "", x[, y], perl = TRUE)
    x[, y] <- gsub("[Rr]eported.*", "", x[, y])
    # Column-generic cleanup
    x[, y] <- ColumnCleanUp(x, y)
    x[, y] <- NegativeRemove(x, y)
    x$Dx_Simplified <- x[, y]
    x$Dx_Simplified <- gsub("- ", "\n", x$Dx_Simplified, fixed = TRUE)
    x$Dx_Simplified <- gsub("-[A-Z]", "\n", x$Dx_Simplified, fixed = TRUE)
    x$Dx_Simplified <- gsub(".*biopsies.*\n", "", x$Dx_Simplified, perl = TRUE)
    x$Dx_Simplified <- gsub(".*biopsy.*\n", "", x$Dx_Simplified, perl = TRUE)
    return(x)
    
}

#' HistolChopperExtrapolDx
#'
#' This extracts other specific diagnoses from the report. These have been hard 
#' coded to look for dysplasia cancer and GIST. Optional use.
#' 
#' @param x the dataframe containing histology results, 
#' @param y the column to extract dysplasia, cancer, and GIST from- often the 
#' Histology diagnosis column
#' @importFrom stringr str_extract
#' @keywords Histology diagnosis
#' @export
#' @examples v<-HistolChopperExtrapolDx(Mypath,'Diagnosis')

HistolChopperExtrapolDx <- function(x, y) {
    # Some further extraction to get commonly searched for data
    x$Cancer <- stringr::str_extract(x[, y], "[Cc]arcin|[Cc]ance|[Ll]ymphoma|
                                     [Tt]umour")
    x$Dysplasia <- stringr::str_extract(x[, y], "[Dd]yspla")
    x$GIST <- stringr::str_extract(x[, y], "G[Ii][Ss][Tt]|[Ss]tromal|[Ll]eio")
    return(x)
}




#' HistolChopperMacDescrip
#' 
#' This extracts numbers from written (spelt) numbers in the Macroscopic 
#' description text. This means the text can then be used to extract the number 
#' and size of biopsies.This is used as part of the 
#' HistolChopperNumOfBx function below and normally not used as a stand alone 
#' function.
#' 
#' @param x dataframe
#' @param y column to extract the numbers from. Usually the column
#' with the Nature of the specimen or the Macroscopic description in it
#' @keywords Macroscopic
#' @export
#' @examples t<-HistolChopperMacDescrip(Mypath, 'Macroscopicdescription')

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
#' This extracts the number of biopsies taken from the pathology report. 
#' This is usually from the Macroscopic description column.
#' It collects everything from the regex [0-9]{1,2}.{0,3} 
#' to whatever the string boundary is (z).
#' 
#' @param x the dataframe
#' @param y Column containing the Macroscopic description text
#' @param z The keyword to remove and to stop at in the regex
#' @importFrom stringr str_match_all
#' @keywords Biopsy number
#' @export
#' @examples v<-HistolChopperNumbOfBx(Mypath,'Macroscopicdescription',
#' 'specimen')

HistolChopperNumbOfBx <- function(x, y, z) {
    x <- data.frame(x)
    x <- HistolChopperMacDescrip(x, y)
    mylist <- str_match_all(x[, y], paste("[0-9]{1,2}.{0,3}", z, sep = ""))
    x$NumbOfBx <- sapply(mylist, function(p) sum(as.numeric(gsub(z, "", p))))
    return(x)
}

#' HistolChopperBxSize
#'
#' This extracts the biopsy size from the report. If there are multiple 
#' biopsies it will extract the overall size of each one (size is calculated 
#' usually in cubic mm from the three dimensions provided). This will result
#' in row duplication.
#' 
#' This is usually from the Macroscopic description column.
#' @param x the dataframe
#' @param y Macdescrip
#' @importFrom stringr str_extract
#' @keywords biopsy size
#' @export
#' @examples v<-HistolChopperBxSize(Mypath,'Macroscopicdescription')

HistolChopperBxSize <- function(x, y) {
    # What's the average biopsy size this month?
    x$BxSize <- stringr::str_extract(x[, y], "the largest.*?mm")
    x$BxSize <- gsub("the largest measuring ", "", x$BxSize)
    x$BxSize <- gsub("mm", "", x$BxSize)
    x$BxSize <- gsub("less than", "", x$BxSize)
    strBxSize <- "([0-9]+).*?([0-9])+.*?([0-9])"
    x$BxSize <- as.numeric(stringr::str_match(x$BxSize, strBxSize)[, 2]) * 
      as.numeric(stringr::str_match(x$BxSize, strBxSize)[, 3]) * 
      as.numeric(stringr::str_match(x$BxSize, strBxSize)[, 4])
    return(x)
}

