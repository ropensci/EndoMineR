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
      "setDT"
    )
  )


############## Endoscopy Clean-up functions##############

#' Cleans endoscopy reports and splits sentences into new lines
#'
#' Cleans the long lines to separate them into new lines if necessary.
#' This helps further down the line in the tokenisation of the text.
#' It is optional depending on the format of your text. It should be used
#' after the Extractor has separated out the different
#' parts of the text according to the user's requirements
#' @param dataframe dataframe
#' @param EndoReportColumn The endoscopy report text column
#' @importFrom stringr str_replace
#' @keywords Endoscopy Newlines
#' @export
#' @examples # The function takes the demo data
#' # Myendo and searches through the raw
#' # endoscopy text so that the text is
#' #divided by sentence into a newline
#' v<-ChopperNewLines(Myendo,'OGDReportWhole')


ChopperNewLines <- function(dataframe, EndoReportColumn) {
  dataframe <- data.frame(dataframe)
  # Separate long lines with empty space into new lines
  dataframe[, EndoReportColumn] <- str_replace(dataframe[, EndoReportColumn],"    ", "\n")
  dataframe[, EndoReportColumn] <- str_replace(dataframe[, EndoReportColumn],"(\n|\r){2,8}", "\\.")
  dataframe[, EndoReportColumn] <- str_replace(dataframe[, EndoReportColumn],"(\n|\r)", "\\." )
  return(dataframe)
}

#' Extracts the columns from the raw report
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
#' @param dataframeIn the dataframe
#' @param Column the column to extract from
#' @param delim the vector of words that will be used as the boundaries to
#' extract against
#' @importFrom stringr str_extract
#' @importFrom tidyr separate
#' @importFrom rlang sym
#' @keywords Extraction
#' @export
#' @examples
#' # As column names cant start with a number, one of the dividing
#' # words has to be converted
#' # A list of dividing words (which will also act as column names)
#' # is then constructed
#' mywords<-c("Hospital Number","Patient Name:","DOB:","General Practitioner:",
#' "Date received:","Clinical Details:","Macroscopic description:",
#' "Histology:","Diagnosis:")
#' Mypath<-Extractor(Mypath,"PathReportWhole",mywords)
#' res<-Mypath
Extractor <- function(dataframeIn, Column, delim) {
  Column <- rlang::sym(Column)
  dataframeIn <- data.frame(dataframeIn)
  dataframeIn<-dataframeIn %>% tidyr::separate(!!Column, into = c("added_name",delim), 
                                          sep = paste(delim, collapse = "|"))
  
  #Devise function that splits each column into sentences using NLP or other 
  #tokenizer non java
  names(dataframeIn) <- gsub(".", "", names(dataframeIn), fixed = TRUE)
  dataframeIn <- apply(dataframeIn, 2, function(x) gsub("\\\\.*", "", x))
  dataframeIn <- apply(dataframeIn, 2, function(x) gsub("       ", "", x))
  #Convert back to a dataframe as has been converted to a matrix
  dataframeIn<-data.frame(dataframeIn)
  #dataframeIn <- apply(dataframeIn, 1, function(x) print(names(x)))
  dataframeIn<-lapply(dataframeIn, ColumnCleanUpAll)
  return(dataframeIn)
}



#' Extracts the columns from the raw report
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
#' @param dataframeIn the dataframe
#' @param Column the column to extract from
#' @param delim the vector of words that will be used as the boundaries to
#' extract against
#' @importFrom stringr str_extract
#' @importFrom tidyr separate
#' @importFrom rlang sym
#' @keywords Extraction
#' @export
#' @examples
#' # As column names cant start with a number, one of the dividing
#' # words has to be converted
#' # A list of dividing words (which will also act as column names)
#' # is then constructed
#' mywords<-c("Hospital Number","Patient Name:","DOB:","General Practitioner:",
#' "Date received:","Clinical Details:","Macroscopic description:",
#' "Histology:","Diagnosis:")
#' Myendo<-EndoscChopperAll(Myendo)
#' res<-Myendo


EndoscChopperAll<- function(dataframe) {

  if("Medications" %in% colnames(dataframe)){
    dataframe<-EndoscChopperMeds(Myendo,'Medications')
    print("Meds")
  }

  if("Instruments" %in% colnames(dataframe)){
    dataframe<-EndoscChopperInstrument(dataframe,'Instruments')
  }

  if("Indications" %in% colnames(dataframe)){
    dataframe<-EndoscChopperIndications(dataframe,'Indications')
  }
  if("Procedure Performed" %in% colnames(dataframe)){
    dataframe<-EndoscChopperProcPerformed(dataframe,'ProcedurePerformed')
  }
  if("Findings" %in% colnames(dataframe)){
    dataframe<-EndoscChopperFindings(dataframe,'Findings')
  }
  return(dataframe)
}





#' Cleans endoscopist column if present
#'
#' If an endoscopist column is part of the dataset once the extractor
#' function has been used this cleans the endoscopist column from the report.
#' It gets rid of titles
#' It gets rid of common entries that are not needed.
#' It should be used after the Extractor and the optional ChopperNewLines
#' has been used.
#'
#' @param dataframe dataframe
#' @param EndoReportColumn The endoscopy text column
#' @keywords Endoscopist extraction
#' @export
#' @importFrom stringr str_replace
#' @examples v<-EndoscChopperEndoscopist(Myendo,'Endoscopist')

EndoscChopperEndoscopist <- function(dataframe, EndoReportColumn) {
  # Extraction of the Endoscopist
  dataframe <- data.frame(dataframe)
  dataframe[, EndoReportColumn] <- gsub("Dr", "", dataframe[, EndoReportColumn], fixed = TRUE)
  dataframe[, EndoReportColumn] <- gsub("Mr", "", dataframe[, EndoReportColumn], fixed = TRUE)
  dataframe[, EndoReportColumn] <- gsub("Professor|Prof", "", dataframe[, EndoReportColumn], fixed = TRUE)
  dataframe[, EndoReportColumn] <- str_replace(dataframe[, EndoReportColumn],"[^[:alnum:],]", "")
  # Put gaps between names
  dataframe[, EndoReportColumn] <- str_replace(dataframe[, EndoReportColumn],"([a-z])([A-Z])", "\\1 \\2")
  dataframe[, EndoReportColumn] <- str_replace(dataframe[, EndoReportColumn],"2nd.*", "")
  dataframe[, EndoReportColumn] <- trimws(dataframe[, EndoReportColumn], which = c("both"))
  return(dataframe)
}

#' Cleans medication column if present
#'
#' This cleans medication column from the report assuming such a column exists.
#' It gets rid of common entries that are not needed. It also splits the
#' medication into fentanyl and midazolam doses for use in the global rating
#' scale tables later. It should be used after the Extractor and the optional
#' ChopperNewLines has been used.
#' @param dataframe dataframe with column of interest
#' @param MedColumn column of interest
#' @keywords Endoscopy medications
#' @importFrom stringr str_extract str_replace
#' @export
#' @examples v<-EndoscChopperMeds(Myendo,'Medications')

EndoscChopperMeds <- function(dataframe, MedColumn) {
  # Extraction of the Medications: Extract the fentanyl:
  dataframe$Fent <-
    str_extract(dataframe[, MedColumn], "Fentanyl\\s*(\\d+)\\s*mcg")
  dataframe$Fent <- str_replace(dataframe$Fent,"Fentanyl", "")
  dataframe$Fent <- str_replace(dataframe$Fent,"mcg", "")
  dataframe$Fent <- as.numeric(dataframe$Fent)
  
  # Extract the midazolam
  dataframe$Midaz <-
    str_extract(dataframe$Medications, "Midazolam\\s*(\\d+)\\s*mg")
  dataframe$Midaz <- str_replace(dataframe$Midaz,"Midazolam ", "")
  dataframe$Midaz <- str_replace(dataframe$Midaz,"mg", "")
  dataframe$Midaz <- as.numeric(dataframe$Midaz)
  return(dataframe)
}


#' Cleans instrument column if present
#'
#' This cleans Instument column from the report assuming such a column exists
#' (where instrument usually refers to the endoscope number being used.
#' It gets rid of common entries that are not needed.
#' It should be used after the Extractor and the optional
#' ChopperNewLines has been used.
#' @param dataframe dataframe with column of interest
#' @param InstrumentColumn column of interest
#' @keywords Instrument
#' @importFrom stringr str_replace
#' @export
#' @examples v<-EndoscChopperInstrument(Myendo,'Instrument')

EndoscChopperInstrument <- function(dataframe, InstrumentColumn) {
  # Extraction of the Instrument used:
  
  dataframe[, InstrumentColumn] <- str_replace(dataframe[, InstrumentColumn],"-.*", "")
  dataframe[, InstrumentColumn] <- gsub("X.*[Ll][Oo[Aa][Nn] [Ss][Cc][Oo][Pp][Ee] \\(|
    Loan Scope \\(specify serial no:|
    Loan Scope \\(specify\\s*serial no|\\)|-.*",
    "",dataframe[, InstrumentColumn]
    )
  dataframe[, InstrumentColumn] <-
    str_replace(dataframe[, InstrumentColumn],",.*|:|FC |[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee] |
         ^,",
         ""
         )
  dataframe[, InstrumentColumn] <- str_replace(dataframe[, InstrumentColumn],"FC ", "FC")
  dataframe[, InstrumentColumn] <- str_replace(dataframe[, InstrumentColumn],"^\\s*([1-9])", "A\\1")
  dataframe[, InstrumentColumn] <- str_replace(dataframe[, InstrumentColumn],"[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee]
                 \\(specify serial no\\)\\s*",
                 "")
  dataframe[, InstrumentColumn] <- str_replace(dataframe[, InstrumentColumn],
                 "[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee]
                 \\(specify serial no:\\)\\s*",
                 "")
  dataframe[, InstrumentColumn] <- toupper(dataframe[, InstrumentColumn])
  dataframe[, InstrumentColumn] <- trimws(dataframe[, InstrumentColumn])
  return(dataframe)
}

#' Cleans indications column if present
#'
#' This cleans the Indication from the report assuming such a column exists.
#' It largely gets rid of carriage returns especially if used after the
#' ChopperNewLinesfunction. There may be multiple indications.
#' It should be used after the Extractor and the optional ChopperNewLines has
#' been used.
#' @param dataframe dataframe with column of interest
#' @param IndicationColumn column of interest
#' @keywords Indications
#' @importFrom stringr str_replace
#' @export
#' @examples v<-EndoscChopperIndications(Myendo,'Indications')

EndoscChopperIndications <- function(dataframe, IndicationColumn) {
  # Extraction of the Indications for examination
  # eg chest pain/ dysphagia etc.
  dataframe[, IndicationColumn] <- str_replace(dataframe[, IndicationColumn],"\r\n", "\n")
  dataframe[, IndicationColumn] <- str_replace(dataframe[, IndicationColumn],"\\.\n\\.\n|\\.\r\\.\r", "\\.")
  return(dataframe)
  
}


#'  Cleans Procedure performed column if present
#'
#' This cleans the Procedure Performed column from the report assuming
#' such a column exists. Procedure Performed relates to whether this was a
#' Gastroscopy or Colonoscopy and or the type of therapy used etc.
#' It gets rid of common entries that are not needed.
#' It should be used after the Extractor and the optional ChopperNewLines
#' has been used.
#' @param dataframe dataframe with column of interest
#' @param ProcPerformed column of interest
#' @keywords Procedure
#' @importFrom stringr str_replace
#' @export
#' @examples v<-EndoscChopperProcPerformed(Myendo,'ProcedurePerformed')

EndoscChopperProcPerformed <- function(dataframe, ProcPerformed) {
  # Extraction of the eg Colonoscopy or gastroscopy etc:
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],"Withdrawal.*", "")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],"Quality.*", "")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],"Adequate.*|Good.*|Poor.*|None.*", "")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],"FINDINGS", "")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],"-\\s*$|-$|-\\s+$", "")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],"([A-Z])-", "\\1 -")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],"\\.", "")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],"-([A-Z])", "-\\1")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],"\\)-", ") -")
  return(dataframe)
}


#' Cleans Procedure performed column if present
#'
#' This cleans the Findings column from the report assuming
#' such a column exists. Findings relates to what was found at the endoscopy
#' This is usually a separate entry to the overall 'Diagnosis' but any
#' are in which the description of the endoscopic findings, including
#' overall diagnosis or not, can be used.
#' It should be used after the Extractor and the optional ChopperNewLines
#' has been used. At present it only cleans cm measurement
#' @param dataframe dataframe with column of interest
#' @param FindingsColumn column of interest
#' @keywords Procedure
#' @export
#' @importFrom stringr str_replace
#' @examples v<-EndoscChopperFindings(Myendo,'Findings')

EndoscChopperFindings <- function(dataframe, FindingsColumn) {
  # Extraction of the FINDINGS
  dataframe[, FindingsColumn] <- str_replace(dataframe[, FindingsColumn],"cm\\s+[A-Z]|cm.+\\)", "cm\n")
  return(dataframe)
}

####### General Clean-Up functions #####

#' Removes negative and normal sentences
#'
#' Extraction of the Negative sentences so that normal findings can be
#' removed and not counted when searching for true diseases. eg remove
#' No evidence of candidal infection so it doesn't get included if
#' looking for candidal infections.
#' It should be used after the Extractor and the optional
#' ChopperNewLines has been used. It can be used as part of the other functions
#' or as a way of providing a 'positive diagnosis only' type output (see
#' HistolChopperDx)
#' @param dataframe dataframe with column of interest
#' @param Column column of interest
#' @keywords Negative Sentences
#' @importFrom stringr str_replace
#' @export
#' @examples # Build a character vector and then
#' # incorporate into a dataframe
#' anexample<-c("There is no evidence of polyp here",
#' "Although the prep was poor,there was no adenoma found",
#' "The colon was basically inflammed, but no polyp was seen",
#' "The Barrett's segment was not biopsied",
#' "The C0M7 stretch of Barrett's was flat")
#' anexample<-data.frame(anexample)
#' names(anexample)<-"Thecol"
#' # Run the function on the dataframe and it should get rid of sentences (and
#' # parts of sentences) with negative parts in them.
#' NegativeRemove(anexample,"Thecol")

NegativeRemove <- function(dataframe, Column) {
  dataframe <- (data.frame(dataframe))
  # Conjunctions
  dataframe[, Column] <- gsub(
    "(but|although|however|though|apart|otherwise
    |unremarkable|\\,)[a-zA-Z0-9_ ]+(no |negative|
    unremarkable|-ve|normal).*?(\\.|
    \\n|:|$)\\R*",
    "\\.\n",
    dataframe[, Column],
    perl = TRUE,
    ignore.case = TRUE
)
  dataframe[, Column] <-
    gsub(
      "(no |negative|unremarkable|-ve| normal) .*?([Bb]ut|
      [Aa]lthough| [Hh]owever| [Tt]hough| [Aa]part| [Oo]therwise|
      [Uu]nremarkable)\\R*",
      "",
      dataframe[, Column],
      perl = TRUE,
      ignore.case = TRUE
  )
  # Nots
  dataframe[, Column] <-
    gsub(
      ".*(was|were) not.*?(\\.|\n|:|$)\\R*",
      "",
      dataframe[, Column],
      perl = TRUE,
      ignore.case = TRUE
    )
  dataframe[, Column] <-
    gsub(
      "not (biop|seen).*?(\\.|\n|:|$)\\R*",
      "",
      dataframe[, Column],
      perl = TRUE,
      ignore.case = TRUE
    )
  # Nos
  dataframe[, Column] <-
    gsub(
      ".*(?:((?<!with)|(?<!there is )|(?<!there are ))\\bno\\b
      (?![?:A-Za-z])|
      ([?:]\\s*N?![A-Za-z])).*\\R*",
      "",
      dataframe[, Column],
      perl = TRUE,
      ignore.case = TRUE
      )
  dataframe[, Column] <-
    gsub(
      ".*(:|[?])\\s*(\\bno\\b|n)\\s*[^A-Za-z0-9].*?(\\.|\n|:|$)
      \\R*",
      "",
      dataframe[, Column],
      perl = TRUE,
      ignore.case = TRUE
    )
  dataframe[, Column] <-
    gsub(
      ".*(negative|neither).*?(\\.|\n|:|$)\\R*",
      "",
      dataframe[, Column],
      perl = TRUE,
      ignore.case = TRUE
    )
  # Keep abnormal in- don't ignore case as it messes it up
  dataframe[, Column] <- str_replace(dataframe[, Column],".*(?<!b)[Nn]ormal.*?(\\.|\n|:|$)", "")
  # Other negatives
  dataframe[, Column] <- gsub(
    ".*there (is|are) \\bno\\b .*?(\\.|
    \n|:|$)\\R*",
    "",
    dataframe[, Column],
    perl = TRUE,
    ignore.case = TRUE
)
  dataframe[, Column] <- gsub(
    "(within|with) (normal|\\bno\\b) .*?(\\.|
    \n|:|$)\\R*",
    "",
    dataframe[, Column],
    perl = TRUE,
    ignore.case = TRUE
)
  # Specific cases
  dataframe[, Column] <- gsub(
    ".*duct.*clear.*?(\\.|\n|:|$)\\R*",
    "",
    dataframe[, Column],
    perl = TRUE,
    ignore.case = TRUE
  )
  # Unanswered prompt lines
  dataframe[, Column] <- gsub(".*:(\\.|\n)\\R*",
                 "",
                 dataframe[, Column],
                 perl = TRUE,
                 ignore.case = TRUE)
  return(dataframe)
}

#' Cleans up the endoscopy columns 
#'
#' This function runs all of the cleaning subfunctions rather than needing to 
#' run them individually
#' @param dataframe dataframe with column of interest
#' @param Column column of interest
#' @keywords Clean
#' @export
#' @importFrom stringr str_replace
#' @examples v<-EndoClean(Myendo,Findings='Findings2')

EndoClean <- function(dataframe, ...) {
  print(dataframe$Findings)
}

#' Tidies up messy columns
#'
#' This does a general clean up of whitespace,
#' semi-colons,full stops at the start
#' of lines and converts end sentence full stops to new lines.
#' It should be used after the Extractor and the optional
#' ChopperNewLines has been used. It can be used as part of the other functions
#' or as a way of providing a 'positive diagnosis only' type output (see
#' HistolChopperDx)
#' @param dataframe dataframe with column of interest
#' @param Column column of interest
#' @keywords Cleaner
#' @export
#' @importFrom stringr str_replace
#' @examples pp<-c("The rain in spain falls mainly",".\n",":What")
#' me<-ColumnCleanUp(pp)

ColumnCleanUp <- function(dataframe, Column) {
 # dataframe <- (data.frame(dataframe))
  dataframe[, "Column"] <- str_replace(dataframe[, Column],"^\\.\n", "")
  dataframe[, Column] <- str_replace(dataframe[, Column],"^:", "")
  dataframe[, Column] <- gsub(".", "\n", dataframe[, Column], fixed = TRUE)
  dataframe[, Column] <- str_replace(dataframe[, Column],"\\s{5}", "")
  dataframe[, Column] <- str_replace(dataframe[, Column],"^\\.", "")
  dataframe[, Column] <- str_replace(dataframe[, Column],"$\\.", "")
  return(dataframe[, Column])
}






#' Tidies up all columns
#'
#' This does a general clean up of whitespace,
#' semi-colons,full stops at the start
#' of lines and converts end sentence full stops to new lines.
#' It it used as part of the Extractor function
#' It can be used as part of the other functions
#' or as a way of providing a 'positive diagnosis only' type output (see
#' HistolChopperDx)
#' @param dataframe dataframe with column of interest
#' @keywords Cleaner
#' @export
#' @importFrom stringr str_replace
#' @examples pp<-c("The rain in spain falls mainly",".\n",":What")
#' me<-ColumnCleanUp(pp)

ColumnCleanUpAll <- function(x) {
  # dataframe <- (data.frame(dataframe))
  x <- str_replace(x,"^\\.\n", "")
  x <- str_replace(x,"^:", "")
  x <- gsub(".", "\n", x, fixed = TRUE)
  x <- str_replace(x,"\\s{5}", "")
  x <- str_replace(x,"^\\.", "")
  
  x<- str_replace(x,"$\\.", "")
  return(x)
}


####### Histology Clean Up All functions #######

#' Clean up histological data. Parent function for all the histological
#' cleaning functions
#'
#' This extracts data from full text histoogy reports and cleans it using all 
#' the histology functions in the package. It is a timesaving function
#' rather than needing to use all the functions one by one.
#' @param dataframe dataframe with column of interest
#' @param MacroColumn column of interest that describes the macroscopic specimen
#' @keywords Macroscopic
#' @export
#' @importFrom stringr str_replace
#' @examples v<-HistolChopperMacDescripCleanup(Mypath,'Macroscopicdescription')


HistolChopperMacDescripCleanup <- function(dataframe, MacroColumn) {
  dataframe <- data.frame(dataframe)
  # Column specific cleanup
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],"[Dd]ictated by.*", "")
  return(dataframe)
}



####### Histology Clean Up functions #######

#' Clean up histological macroscopic description data
#'
#' This extracts Macroscopic description data from the pathology report.
#' Macroscopic description usually relates to the number of specimens
#' retrieved, the size of each specimen and the location it was taken from.
#' The cleanup usually relates to the removal of top and tail characters such
#' as who reported the specimens etc.
#' @param dataframe dataframe with column of interest
#' @param MacroColumn column of interest that describes the macroscopic specimen
#' @keywords Macroscopic
#' @export
#' @importFrom stringr str_replace
#' @examples v<-HistolChopperMacDescripCleanup(Mypath,'Macroscopicdescription')


HistolChopperMacDescripCleanup <- function(dataframe, MacroColumn) {
  dataframe <- data.frame(dataframe)
  # Column specific cleanup
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],"[Dd]ictated by.*", "")
  return(dataframe)
}


#' Extract the histology data from the report by removing negative findings
#'
#' This extracts Histology details data from the report. The Histology details
#' usually relate to the description of the histological report. This implements
#'  the negative remover and also adds further negative removing regexes. This
#' may be refined in further iterations.
#' @param dataframe dataframe with column of interest
#' @param HistolColumn column of interest
#' @keywords Histology
#' @export
#' @importFrom stringr str_replace
#' @examples t<-HistolChopperHistol(Mypath,'Histology')


HistolChopperHistol <- function(dataframe, HistolColumn) {
  # HISTOLOGY
  dataframe[, HistolColumn] <- str_replace(dataframe[, HistolColumn],"\n|\r", " ")
  dataframe[, HistolColumn] <- NegativeRemove(dataframe[, HistolColumn])
  dataframe$Histol_Simplified <- dataframe[, HistolColumn]
  # Negative extraction- may merge this with the function
  # NegativeRemove() above and some of the
  #phrases below could undoubetdly be simplified with more intelligent regex
  dataframe$Histol_Simplified <- gsub("- ", "\n", dataframe$Histol_Simplified,
                              fixed = TRUE)
  dataframe$Histol_Simplified <- gsub("-[A-Z]", "\n", dataframe$Histol_Simplified
                              , fixed = TRUE)
  dataframe$Histol_Simplified <-
    str_replace(dataframe$Histol_Simplified,".*biopsies.*\n", "")
  dataframe$Histol_Simplified <-
    str_replace(dataframe$Histol_Simplified,".*biopsy.*\n", "")
  dataframe$Histol_Simplified <-str_replace(dataframe$Histol_Simplified,":", "")
  return(dataframe)
}



#' Extract histological accession number
#'
#' This extracts Accession Number data data from the report where one is
#' present. The Accession number relates to the actual specimen number as
#' ascribed by the pathology service.
#' @param dataframe dataframe name and
#' @param  AccessionColumn the column name as a string.
#' @param  regString regular expression needed as a string
#' @importFrom stringr str_extract
#' @keywords Sample Accession number
#' @export
#' @examples v<-HistolChopperAccessionNumber(Mypath,'Histology',
#' 'SP-\\d{2}-\\d{7}')

HistolChopperAccessionNumber <- function(dataframe, AccessionColumn, regString) {
  dataframe <- data.frame(dataframe)
  # Accession number samples- not really necessary to extract:
  dataframe$AccessionNumber <- 
   str_extract(dataframe[, AccessionColumn], regString)
  return(dataframe)
}

#' Extracts histological diagnosis
#'
#' This extracts Diagnosis data from the report. The Diagnosis is the overall
#' impression of the pathologist for that specimen. At the moment, Only Capital
#' D included (not lower case d) to make sure picks up subtitle header as
#' opposed to mentioning 'diagnosis' as part of a sentence.  Column specific
#' cleanup and negative remover have also been implemented here.
#'
#' @param dataframe dataframe
#' @param HistolColumn column containing the Hisopathology report
#' @importFrom stringr str_extract str_replace
#' @keywords Histology Diagnosis
#' @export
#' @examples v<-HistolChopperDx(Mypath,'Diagnosis')

HistolChopperDx <- function(dataframe, HistolColumn) {
  dataframe[, HistolColumn] <- str_replace(dataframe[, HistolColumn],"Dr.*", "")
  dataframe[, HistolColumn] <- str_replace(dataframe[, HistolColumn],"[Rr]eported.*", "")
  # Column-generic cleanup
  dataframe[, HistolColumn] <- ColumnCleanUp(dataframe, HistolColumn)
  dataframe[, HistolColumn] <- NegativeRemove(dataframe, HistolColumn)
  dataframe$Dx_Simplified <- dataframe[, HistolColumn]
  dataframe$Dx_Simplified <-
    gsub("- ", "\n", dataframe$Dx_Simplified, fixed = TRUE)
  dataframe$Dx_Simplified <-
    gsub("-[A-Z]", "\n", dataframe$Dx_Simplified, fixed = TRUE)
  dataframe$Dx_Simplified <-
    str_replace(dataframe$Dx_Simplified,".*biopsies.*\n", "")
  dataframe$Dx_Simplified <-
    str_replace(dataframe$Dx_Simplified,".*biopsy.*\n", "")
  return(dataframe)
  
}

#' Extract specific diagnoses from the histology report
#'
#' This extracts other specific diagnoses from the report. These have been hard
#' coded to look for dysplasia cancer and GIST. Optional use.
#'
#' @param dataframe dataframe containing histology results,
#' @param Column the column to extract dysplasia, cancer, and GIST from- often the
#' Histology diagnosis column
#' @importFrom stringr str_extract
#' @keywords Histology diagnosis
#' @export
#' @examples v<-HistolChopperExtrapolDx(Mypath,'Diagnosis')

HistolChopperExtrapolDx <- function(dataframe, Column) {
  # Some further extraction to get commonly searched for data
  dataframe$Cancer <-
    str_extract(dataframe[, Column], "[Cc]arcin|[Cc]ance|[Ll]ymphoma|
                         [Tt]umour")
  dataframe$Dysplasia <- str_extract(dataframe[, Column], "[Dd]yspla")
  dataframe$GIST <-
    str_extract(dataframe[, Column], "G[Ii][Ss][Tt]|[Ss]tromal|[Ll]eio")
  return(dataframe)
}




#' Cleans spelt numbers in histology report
#'
#' This extracts numbers from written (spelt) numbers in the Macroscopic
#' description text. This means the text can then be used to extract the number
#' and size of biopsies.This is used as part of the
#' HistolChopperNumOfBx function below and normally not used as a stand alone
#' function.
#'
#' @param dataframe dataframe
#' @param MacroColumn column to extract the numbers from. Usually the column
#' with the Nature of the specimen or the Macroscopic description in it
#' @keywords Macroscopic
#' @importFrom stringr str_replace
#' @export
#' @examples t<-HistolChopperMacDescrip(Mypath, 'Macroscopicdescription')

HistolChopperMacDescrip <- function(dataframe, MacroColumn) {
  x <- data.frame(dataframe)
  # Conversion of text numbers to allow number of biopsies to be extracted
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],"[Oo]ne", "1")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],"[Ss]ingle", "1")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],"[Tt]wo", "2")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],"[Tt]hree", "3")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],"[Ff]our", "4")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],"[Ff]ive", "5")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],"[Ss]ix", "6")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],"[Ss]even", "7")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],"[Ee]ight", "8")
  return(x)
}

#' Extract the number of biopsies taken from histology report
#'
#' This extracts the number of biopsies taken from the pathology report.
#' This is usually from the Macroscopic description column.
#' It collects everything from the regex [0-9]{1,2}.{0,3}
#' to whatever the string boundary is (z).
#'
#' @param dataframe the dataframe
#' @param MacroColumn Column containing the Macroscopic description text
#' @param regString The keyword to remove and to stop at in the regex
#' @importFrom stringr str_match_all
#' @keywords Biopsy number
#' @export
#' @examples v<-HistolChopperNumbOfBx(Mypath,'Macroscopicdescription',
#' 'specimen')

HistolChopperNumbOfBx <- function(dataframe, MacroColumn, regString) {
  dataframe <- data.frame(dataframe)
  dataframe <- HistolChopperMacDescrip(dataframe, MacroColumn)
  mylist <-
    str_match_all(dataframe[, MacroColumn], paste("[0-9]{1,2}.{0,3}", 
                                                  regString, sep = ""))
  dataframe$NumbOfBx <-
    sapply(mylist, function(p)
      sum(as.numeric(gsub(regString, "", p))))
  return(dataframe)
}

#' Determine the largest biopsy size from the histology report
#'
#' This extracts the biopsy size from the report. If there are multiple
#' biopsies it will extract the overall size of each one (size is calculated
#' usually in cubic mm from the three dimensions provided). This will result
#' in row duplication.
#'
#' This is usually from the Macroscopic description column.
#' @param dataframe dataframe
#' @param MacroColumn Macdescrip
#' @importFrom stringr  str_match str_replace
#' @keywords biopsy size
#' @export
#' @examples v<-HistolChopperBxSize(Mypath,'Macroscopicdescription')

HistolChopperBxSize <- function(dataframe, MacroColumn) {
  # What's the average biopsy size this month?
  dataframe$BxSize <- str_extract(dataframe[, MacroColumn], "the largest.*?mm")
  dataframe$BxSize <- str_replace(dataframe$BxSize,"the largest measuring ", "")
  dataframe$BxSize <- str_replace(dataframe$BxSize,"mm", "")
  dataframe$BxSize <- str_replace(dataframe$BxSize,"less than", "")
  strBxSize <- "([0-9]+).*?([0-9])+.*?([0-9])"
  dataframe$BxSize <-
    as.numeric(str_match(dataframe$BxSize, strBxSize)[, 2]) *
    as.numeric(str_match(dataframe$BxSize, strBxSize)[, 3]) *
    as.numeric(str_match(dataframe$BxSize, strBxSize)[, 4])
  return(dataframe)
}
