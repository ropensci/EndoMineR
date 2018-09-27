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
      "setDT",
      "Myendo",
      "Mypath"
    )
  )


############## Endoscopy Clean-up functions##############


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
#' Mypath2<-Extractor(PathDataFrameFinal,"PathReportWhole",mywords)
#'

Extractor <- function(dataframeIn, Column, delim) {
  dataframeInForLater<-dataframeIn
  ColumnForLater<-Column
  Column <- rlang::sym(Column)
  dataframeIn <- data.frame(dataframeIn)
  dataframeIn<-dataframeIn %>%
    tidyr::separate(!!Column, into = c("added_name",delim),
                                          sep = paste(delim, collapse = "|"),
                    extra = "drop", fill = "right")
  names(dataframeIn) <- gsub(".", "", names(dataframeIn), fixed = TRUE)
  dataframeIn <- apply(dataframeIn, 2, function(x) gsub("\\\\.*", "", x))
  dataframeIn <- apply(dataframeIn, 2, function(x) gsub("       ", "", x))

  #Convert back to a dataframe as has been converted to a matrix
  dataframeIn<-data.frame(dataframeIn)
  dataframeIn<-dataframeIn[,-1]
  dataframeIn<-lapply(dataframeIn, ColumnCleanUpAll)
  names(dataframeIn) <- gsub(".","",names(dataframeIn),fixed=TRUE)
  dataframeIn<-data.frame(dataframeIn)
  #Add the original column back in so have the original reference
  dataframeIn<-cbind(dataframeInForLater[,ColumnForLater],dataframeIn)
  colnames(dataframeIn)[1]<-"Original"
  dataframeIn<-data.frame(dataframeIn)
  return(dataframeIn)
}




#' Extractor2
#'
#' This is the alternative extractor for the Endoscopy and Histology report.
#' THis performs the same essentially as the main extractor but is useful when the
#' semi-structured text is organised in a non-standard way ie the delimiting text is not always in the same order
#' As per the main Extractor, This function on the user creating a list of words or characters that
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
#'  Myendo<-Extractor2(Myendo,'OGDReportWhole',as.character(EndoscTree[i]),
#'  as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
#' }
#' res<-Myendo


Extractor2 <- function(x, y, stra, strb, t) {
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





#' Extracts the columns from the raw report
#'
#' This is the parent cleaning function for the endoscopy report. It contains
#' all the other functions for the endoscopy report to be cleaned up. It
#' relies on the columns being named in a standardised way as below

#' @param dataframe the dataframe
#' @keywords Extraction
#' @export
#' @examples
#' # Rename the columns in whatever endoscopy dataframe you have
#' names(Myendo)<-c("OGDReportWhole","HospitalNumber","PatientName",
#' "GeneralPractitioner","Dateofprocedure","Endoscopist","Secondendoscopist",
#' "Medications","Instrument","ExtentofExam","Indications","ProcedurePerformed",
#' "Findings" )
#' #Now use the function
#' bb<-EndoscAll(Myendo)
#' rm(Myendo)


EndoscAll<- function(dataframe) {

  if("Medications" %in% colnames(dataframe)){
    dataframe<-EndoscMeds(Myendo,'Medications')

  }
  if("Instruments" %in% colnames(dataframe)){
    dataframe<-EndoscInstrument(dataframe,'Instruments')
  }
  if("Indications" %in% colnames(dataframe)){
    dataframe<-EndoscIndications(dataframe,'Indications')
  }
  if("Procedure Performed" %in% colnames(dataframe)){
    dataframe<-EndoscProcPerformed(dataframe,'ProcedurePerformed')
  }
  if("Findings" %in% colnames(dataframe)){
    dataframe<-EndoscFindings(dataframe,'Findings')
  }
  if("Endoscopist" %in% colnames(dataframe)){
    dataframe<-EndoscEndoscopist(dataframe,'Endoscopist')
  }
  return(dataframe)
}



#' Cleans endoscopist column if present
#'
#' If an endoscopist column is part of the dataset once the extractor
#' function has been used this cleans the endoscopist column from the report.
#' It gets rid of titles
#' It gets rid of common entries that are not needed.
#' It should be used after the Extractor and the optional NewLines
#' has been used.
#'
#' @param dataframe dataframe
#' @param EndoReportColumn The endoscopy text column
#' @keywords Endoscopist extraction
#' @export
#' @importFrom stringr str_replace
#' @examples de<-EndoscEndoscopist(Myendo,'Endoscopist')


EndoscEndoscopist <- function(dataframe, EndoReportColumn) {
  # Extraction of the Endoscopist
  dataframe <- data.frame(dataframe)
  dataframe[, EndoReportColumn] <- str_replace(dataframe[, EndoReportColumn],
                                               "Mr|Professor|Prof|Dr", "")
  dataframe[, EndoReportColumn] <- str_replace(dataframe[, EndoReportColumn],
                                               "[^[:alnum:],]", "")
  # Put gaps between names
  dataframe[, EndoReportColumn] <- str_replace(dataframe[, EndoReportColumn],
                                               "([a-z])([A-Z])", "\\1 \\2")
  dataframe[, EndoReportColumn] <- str_replace(dataframe[, EndoReportColumn],
                                               "2nd.*", "")
  dataframe[, EndoReportColumn] <- trimws(dataframe[, EndoReportColumn],
                                          which = c("both"))
  return(dataframe)
}

#' Cleans medication column if present
#'
#' This cleans medication column from the report assuming such a column exists.
#' It gets rid of common entries that are not needed. It also splits the
#' medication into fentanyl and midazolam doses for use in the global rating
#' scale tables later. It should be used after the Extractor and the optional
#' NewLines has been used.
#' @param dataframe dataframe with column of interest
#' @param MedColumn column of interest
#' @keywords Endoscopy medications
#' @importFrom stringr str_extract str_replace
#' @export
#' @examples cc<-EndoscMeds(Myendo,'Medications')

EndoscMeds <- function(dataframe, MedColumn) {
  # Extraction of the Medications: Extract the fentanyl if present


  dataframe$Fent <-
    str_extract(dataframe[, MedColumn], "\\s*(\\d*(\\.\\d+)?)\\s*mcg")
  dataframe$Fent <- str_replace(dataframe$Fent,"Fentanyl", "")
  dataframe$Fent <- str_replace(dataframe$Fent,"mcg", "")
  dataframe$Fent <- as.numeric(dataframe$Fent)


  # Extract the midazolam if present

  dataframe$Midaz <-
    str_extract(dataframe$Medications, "Midazolam\\s*(\\d*(\\.\\d+)?)\\s*mg")
  dataframe$Midaz <- str_replace(dataframe$Midaz,"Midazolam ", "")
  dataframe$Midaz <- str_replace(dataframe$Midaz,"mg", "")
  dataframe$Midaz <- as.numeric(dataframe$Midaz)

  # Extract the pethidine if present
    dataframe$Peth <-
      str_extract(dataframe[, MedColumn], "\\s*(\\d*(\\.\\d+)?)\\s*mcg")
    dataframe$Peth <- str_replace(dataframe$Peth,"Pethidine", "")
    dataframe$Peth <- str_replace(dataframe$Peth,"mcg", "")
    dataframe$Peth <- as.numeric(dataframe$Peth)

    # Extract the propofol if present
    dataframe$Prop <-
      str_extract(dataframe[, MedColumn], "\\s*(\\d*(\\.\\d+)?)\\s*mcg")
    dataframe$Prop <- str_replace(dataframe$Prop,"Propofol", "")
    dataframe$Prop <- str_replace(dataframe$Prop,"mcg", "")
    dataframe$Prop <- as.numeric(dataframe$Prop)

  return(dataframe)
}


#' Cleans instrument column if present
#'
#' This cleans Instument column from the report assuming such a column exists
#' (where instrument usually refers to the endoscope number being used.
#' It gets rid of common entries that are not needed.
#' It should be used after the Extractor and the optional
#' NewLines has been used.
#' @param dataframe dataframe with column of interest
#' @param InstrumentColumn column of interest
#' @keywords Instrument
#' @importFrom stringr str_replace
#' @export
#' @examples dd<-EndoscInstrument(Myendo,'Instrument')

EndoscInstrument <- function(dataframe, InstrumentColumn) {
  # Extraction of the Instrument used:

  dataframe[, InstrumentColumn] <- str_replace(dataframe[, InstrumentColumn],
                                               "-.*", "")
  dataframe[, InstrumentColumn] <-
gsub("X.*[Ll][Oo[Aa][Nn] [Ss][Cc][Oo][Pp][Ee] \\(|
    Loan Scope \\(specify serial no:|
    Loan Scope \\(specify\\s*serial no|\\)|-.*",
    "",dataframe[, InstrumentColumn]
    )
  dataframe[, InstrumentColumn] <-
    str_replace(dataframe[, InstrumentColumn],
",.*|:|FC |[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee] |
         ^,",
         ""
         )
  dataframe[, InstrumentColumn] <- str_replace(dataframe[, InstrumentColumn],
                                               "FC ", "FC")
  dataframe[, InstrumentColumn] <- str_replace(dataframe[, InstrumentColumn],
                                               "^\\s*([1-9])", "A\\1")
  dataframe[, InstrumentColumn] <- str_replace(dataframe[, InstrumentColumn],
"[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee]
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
#' NewLinesfunction. There may be multiple indications.
#' It should be used after the Extractor and the optional NewLines has
#' been used.
#' @param dataframe dataframe with column of interest
#' @param IndicationColumn column of interest
#' @keywords Indications
#' @importFrom stringr str_replace
#' @export
#' @examples ee<-EndoscIndications(Myendo,'Indications')

EndoscIndications <- function(dataframe, IndicationColumn) {
  # Extraction of the Indications for examination
  # eg chest pain/ dysphagia etc.
  dataframe[, IndicationColumn] <- str_replace(dataframe[, IndicationColumn],
                                               "\r\n", "\n")
  dataframe[, IndicationColumn] <- str_replace(dataframe[, IndicationColumn],
                                               "\\.\n\\.\n|\\.\r\\.\r", "\\.")
  return(dataframe)
}


#'  Cleans Procedure performed column if present
#'
#' This cleans the Procedure Performed column from the report assuming
#' such a column exists. Procedure Performed relates to whether this was a
#' Gastroscopy or Colonoscopy and or the type of therapy used etc.
#' It gets rid of common entries that are not needed.
#' It should be used after the Extractor and the optional NewLines
#' has been used.
#' @param dataframe dataframe with column of interest
#' @param ProcPerformed column of interest
#' @keywords Procedure
#' @importFrom stringr str_replace
#' @export
#' @examples ff<-EndoscProcPerformed(Myendo,'ProcedurePerformed')

EndoscProcPerformed <- function(dataframe, ProcPerformed) {
  # Extraction of the eg Colonoscopy or gastroscopy etc:
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],
                                            "Withdrawal.*", "")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],
                                            "Quality.*", "")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],
                                       "Adequate.*|Good.*|Poor.*|None.*", "")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],
                                            "FINDINGS", "")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],
                                            "-\\s*$|-$|-\\s+$", "")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],
                                            "([A-Z])-", "\\1 -")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],
                                            "\\.", "")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],
                                            "-([A-Z])", "-\\1")
  dataframe[, ProcPerformed] <- str_replace(dataframe[, ProcPerformed],
                                            "\\)-", ") -")
  return(dataframe)
}


#' Cleans Procedure performed column if present
#'
#' This cleans the Findings column from the report assuming
#' such a column exists. Findings relates to what was found at the endoscopy
#' This is usually a separate entry to the overall 'Diagnosis' but any
#' are in which the description of the endoscopic findings, including
#' overall diagnosis or not, can be used.
#' It should be used after the Extractor and the optional NewLines
#' has been used. At present it only cleans cm measurement
#' @param dataframe dataframe with column of interest
#' @param FindingsColumn column of interest
#' @keywords Procedure
#' @export
#' @importFrom stringr str_replace
#' @examples gg<-EndoscFindings(Myendo,'Findings')

EndoscFindings <- function(dataframe, FindingsColumn) {
  # Extraction of the FINDINGS
  #dataframe[, FindingsColumn] <-
    str_replace(dataframe[, FindingsColumn],
                                             "cm\\s+[A-Z]|cm.+\\)", "cm\n")
  dataframe$EndoFindingsSimple<- NegativeRemove(dataframe, FindingsColumn)
  #Put in the Negative remove for FindingsSimplified Here
  return(dataframe)
}

####### General Clean-Up functions #####


############## Endoscopy Clean-up functions##############

#' Removes negative and normal sentences
#'
#' Extraction of the Negative sentences so that normal findings can be
#' removed and not counted when searching for true diseases. eg remove
#' No evidence of candidal infection so it doesn't get included if
#' looking for candidal infections.
#' It should be used after the Extractor and the optional
#' NewLines has been used. It can be used as part of the other functions
#' or as a way of providing a 'positive diagnosis only' type output (see
#' HistolDx)
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
#' hh<-NegativeRemove(anexample,"Thecol")

NegativeRemove <- function(dataframe, Column) {
  dataframe <- (data.frame(dataframe))
  # Conjunctions
  dataframe[, Column] <- gsub(
    "(but|although|however|though|apart|otherwise
    |unremarkable|\\,)[a-zA-Z0-9_ ]+(no |negative|
    unremarkable|-ve|normal).*?(\\.|\\n|:|$)\\R*",
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
      ".*(does|is|was|were|are|have) not.*?(\\.|\n|:|$)\\R*",
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
      ".*(?:((?<!with)|(?<!there is )|(?<!there are ))\\bno\\b(?![?:A-Za-z])|
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
  dataframe[, Column] <- str_replace(dataframe[, Column],
                                     ".*(?<!b)[Nn]ormal.*?(\\.|\n|:|$)", "")
  # Other negatives
  dataframe[, Column] <- gsub(
    ".*there (is|are|were) \\bno\\b .*?(\\.|\n|:|$)\\R*",
    "",
    dataframe[, Column],
    perl = TRUE,
    ignore.case = TRUE
)
  dataframe[, Column] <- gsub(
    "(within|with) (normal|\\bno\\b) .*?(\\.|\n|:|$)\\R*",
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
  return(dataframe[, Column])
}



#' Tidies up messy columns
#'
#' This does a general clean up of whitespace,
#' semi-colons,full stops at the start
#' of lines and converts end sentence full stops to new lines.
#' It should be used after the Extractor.
#' It is used for columns where there is a lot of free text to extract. It
#' really extracts and standardises the sentences.
#' @param dataframe dataframe with column of interest
#' @param Column column of interest
#' @keywords Cleaner
#' @export
#' @importFrom stringr str_replace
#' @examples ii<-ColumnCleanUp(Myendo,"Findings")


ColumnCleanUp <- function(dataframe, Column) {
  #Get rid of the empty lines with floating puctuation
  dataframe[, "Column"] <- str_replace(dataframe[, Column],"^\\.\n", "")
  dataframe[, Column] <- str_replace(dataframe[, Column],"^\\.", "")
  dataframe[, Column] <- str_replace(dataframe[, Column],"^:", "")

  #Get rid of breaks between lines
  dataframe[, Column] <- str_replace(dataframe[, Column],"(\n|\r){2,}", "\n")
  dataframe[, Column] <- str_replace(dataframe[, Column],
                                     "\\.\n\\.\n|\\.\r\\.\r", "\\.")

  #Get rid of floating whitespace
  dataframe[, Column] <- str_replace(dataframe[, Column],"\\s{5,}", "")
  dataframe[, Column] <- str_replace(dataframe[, Column],"$\\.", "")

  #Get rid of floating commas at the end of lines
  dataframe[, Column] <- str_replace(dataframe[, Column],"\n,", "\n")
  #Standardise the carriage returns
  dataframe[, Column] <- str_replace(dataframe[, Column],
                                               "\r\n", "\n")



  #Get rid of trailing dots from previous conversions
  dataframe[, Column] <- str_replace(dataframe[, Column],"\\.{2,}", "\\.")

  #Convert sentence endings to newlines as the sentence boundary
  dataframe[, Column] <- gsub(".", "\n", dataframe[, Column], fixed = TRUE)
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
#' HistolDx)
#' @param dataframe dataframe with column of interest
#' @keywords Cleaner
#' @export
#' @importFrom stringr str_replace
#' @examples jj<-lapply(Myendo, ColumnCleanUpAll)
#'

ColumnCleanUpAll <- function(dataframe) {

  #Get rid of the empty lines with floating puctuation
  dataframe <- str_replace(dataframe,"^\\.\n", "")
  dataframe <- str_replace(dataframe,"^\\.", "")
  dataframe <- str_replace(dataframe,"^:", "")

  #Get rid of breaks between lines
  dataframe <- str_replace(dataframe,"(\n|\r){2,}", "\n")
  dataframe<- str_replace(dataframe,"\\.\n\\.\n|\\.\r\\.\r", "\\.")

  #Get rid of floating whitespace
  dataframe <- str_replace(dataframe,"\\s{5,}", "")
  dataframe <- str_replace(dataframe,"$\\.", "")

  #Get rid of floating commas at the end of lines
  dataframe <- str_replace(dataframe,"\n,", "\n")

  #Get rid of trailing dots from previous conversions
  dataframe <- str_replace(dataframe,"\\.{2,}", "\\.")

  #Standardise the carriage returns
  dataframe<- str_replace(dataframe,"\r\n", "\n")

  #Convert sentence endings to newlines as the sentence boundary
  dataframe <- gsub(".", "\n", dataframe, fixed = TRUE)
  return(dataframe)
}

####### Histology Clean Up All functions #######





####### Histology Clean Up functions #######


#' Clean up all Histology data
#'
#' This is a parent function for all the functions below
#' @param dataframe dataframe with column of interest
#' @keywords Macroscopic
#' @export
#' @importFrom stringr str_replace
#' @examples
#' mywords<-c("Hospital Number","Patient Name:","DOB:","General Practitioner:",
#' "Date received:","Clinical Details:","Macroscopic description:",
#' "Histology:","Diagnosis:")
#' MypathExtracted<-Extractor(PathDataFrameFinal,"PathReportWhole",mywords)
#' names(Mypath)<-c("Original","HospitalNumber","PatientName","DOB",
#' "GeneralPractitioner","Datereceived","ClinicalDetails",
#' "Macroscopicdescription","Histology","Diagnosis")
#' kk<-HistolAll(MypathExtracted)
#' rm(MypathExtracted)



HistolAll <- function(dataframe) {

  if("Histology" %in% colnames(dataframe)){
    dataframe<-HistolHistol(dataframe,'Histology')
    dataframe<-HistolAccessionNumber(dataframe,'Histology','SP-\\d{2}-\\d{7}')
  }

  if("Macroscopicdescription" %in% colnames(dataframe)){
    dataframe<-HistolMacDescrip(dataframe,'Macroscopicdescription')
    dataframe<-HistolNumbOfBx(Mypath,'Macroscopicdescription',
                                     'specimen')
    dataframe<- HistolBxSize(Mypath,'Macroscopicdescription')
  }

  if("Diagnosis" %in% colnames(dataframe)){
    dataframe<-HistolDx(dataframe,'Diagnosis')
    dataframe<-HistolExtrapolDx(dataframe,'Diagnosis',"")
  }
  dataframe<-data.frame(dataframe)

  return(dataframe)
}


#' Extract the histology data from the report by removing negative findings
#'
#' This extracts Histology details data from the report and also removes
#' negative findings. The Histology details
#' usually relate to the description of the histological report.
#' @param dataframe dataframe with column of interest
#' @param HistolColumn column of interest
#' @keywords Histology
#' @export
#' @importFrom stringr str_replace
#' @examples ll<-HistolHistol(Mypath,'Histology')


HistolHistol <- function(dataframe, HistolColumn) {
  dataframe<-data.frame(dataframe)
  # HISTOLOGY
  dataframe[, HistolColumn] <- str_replace(dataframe[, HistolColumn],
                                           "\n|\r", " ")
  dataframe[, HistolColumn] <- ColumnCleanUp(dataframe, HistolColumn)
  dataframe$Histol_Simplified<- NegativeRemove(dataframe, HistolColumn)
  dataframe$Histol_Simplified <- gsub("- ", "\n", dataframe$Histol_Simplified,
                              fixed = TRUE)
  dataframe$Histol_Simplified <- gsub("-[A-Z]", "\n",
                                      dataframe$Histol_Simplified
                              , fixed = TRUE)
  #dataframe$Histol_Simplified <-
    #str_replace(dataframe$Histol_Simplified,"(?<=[A-Z].*)biopsies.*\n", "")
  #dataframe$Histol_Simplified <-
    #str_replace(dataframe$Histol_Simplified,"[A-Z].*biopsy.*\n", "")
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
#' @examples mm<-HistolAccessionNumber(Mypath,'Histology',
#' "SP-\\d{2}-\\d{7}")

HistolAccessionNumber <- function(dataframe, AccessionColumn, regString) {
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
#' @param HistolColumn column containing the Histopathology report
#' @importFrom stringr str_extract str_replace
#' @keywords Histology Diagnosis
#' @export
#' @examples nn<-HistolDx(Mypath,'Diagnosis')

HistolDx <- function(dataframe, HistolColumn) {
  dataframe<-data.frame(dataframe)
  dataframe[, HistolColumn] <- str_replace(dataframe[, HistolColumn],"Dr.*", "")
  dataframe[, HistolColumn] <- str_replace(dataframe[, HistolColumn],
                                           "[Rr]eported.*", "")
  # Column-generic cleanup
  dataframe[, HistolColumn] <- ColumnCleanUp(dataframe, HistolColumn)
  dataframe[, HistolColumn]<- NegativeRemove(dataframe, HistolColumn)
  dataframe$Dx_Simplified <- dataframe[, HistolColumn]
  dataframe$Dx_Simplified <-
    gsub("- ", "\n", dataframe$Dx_Simplified, fixed = TRUE)
  dataframe$Dx_Simplified <-
    gsub("-[A-Z]", "\n", dataframe$Dx_Simplified, fixed = TRUE)
  #dataframe$Dx_Simplified <-
    #str_replace(dataframe$Dx_Simplified,"[A-Z].*biopsies.*\n", "")
  #dataframe$Dx_Simplified <-
    #str_replace(dataframe$Dx_Simplified,"[A-Z].*biopsy.*\n", "")
  return(dataframe)

}


#' Extract specific diagnoses from the histology report
#'
#' This extracts other specific diagnoses from the report. These have been hard
#' coded to look for dysplasia cancer and GIST. Optional use for the user to
#' add regular expressions as well. All the diagnoses are extracted into
#' one column and made unique.
#'
#' @param dataframe dataframe containing histology results,
#' @param Column the column to extract dysplasia, cancer, and GIST from-
#' often the Histology diagnosis column
#' @param userString user defined string search for (regular expression)
#' @importFrom stringr str_extract_all
#' @keywords Histology diagnosis
#' @export
#' @examples oo<-HistolExtrapolDx(Mypath,'Diagnosis',"")

HistolExtrapolDx <- function(dataframe, Column,userString) {
  # Some further extraction to get commonly searched for data
  dataframe$Extracted <-
    str_extract_all(dataframe[, Column],
                    paste0("[Cc]arcin|[Cc]ance|[Ll]ymphoma|[Tt]umour|[Dd]yspla|G[Ii][Ss][Tt]|[Ss]tromal|[Ll]eio|[Cc]rohn",userString),
                   simplify = FALSE)
  #Make each entry unique
  dataframe$Extracted<-sapply(dataframe$Extracted, toString)
  return(dataframe)
}


#' Cleans spelt numbers in histology report
#'
#' This extracts numbers from written (spelt) numbers in the Macroscopic
#' description text. This means the text can then be used to extract the number
#' and size of biopsies.This is used as part of the
#' HistolNumOfBx function below and normally not used as a stand alone
#' function.
#'
#' @param dataframe dataframe
#' @param MacroColumn column to extract the numbers from. Usually the column
#' with the Nature of the specimen or the Macroscopic description in it
#' @keywords Macroscopic
#' @importFrom stringr str_replace
#' @export
#' @examples pp<-HistolMacDescrip(Mypath, 'Macroscopicdescription')


HistolMacDescrip <- function(dataframe, MacroColumn) {
  dataframe <- data.frame(dataframe)

  # Column specific cleanup
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],
                                          "[Dd]ictated by.*", "")
  # Conversion of text numbers to allow number of biopsies to be extracted
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],
                                          "[Oo]ne", "1")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],
                                          "[Ss]ingle", "1")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],
                                          "[Tt]wo", "2")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],
                                          "[Tt]hree", "3")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],
                                          "[Ff]our", "4")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],
                                          "[Ff]ive", "5")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],
                                          "[Ss]ix", "6")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],
                                          "[Ss]even", "7")
  dataframe[, MacroColumn] <- str_replace(dataframe[, MacroColumn],
                                          "[Ee]ight", "8")
  return(dataframe)
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
#' @examples
#' qq<-HistolNumbOfBx(Mypath,'Macroscopicdescription','specimen')


HistolNumbOfBx <- function(dataframe, MacroColumn, regString) {
  dataframe <- data.frame(dataframe)
  dataframe <- HistolMacDescrip(dataframe, MacroColumn)
  mylist <-
    str_match_all(dataframe[, MacroColumn], paste("[0-9]{1,2}.{0,3}",
                                                  regString, sep = ""))
  dataframe$NumbOfBx <-
    vapply(mylist, function(p)
      sum(as.numeric(gsub(regString, "", p))),numeric(1))
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
#' @examples rr<-HistolBxSize(Mypath,'Macroscopicdescription')

HistolBxSize <- function(dataframe, MacroColumn) {
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


#' #' Validate columns
#' #'
#' #' This is an attempt to determine whether functions are accurate in their extraction
#' #' It is part of a validation pipeline to make sure the data outputted from a function
#' #' is accurate
#' #' It essentially is a comparison of actual vs expected output. The intention is to 
#' #' then quantify the output of a function for a specific dataset.
#' #'
#' #' @param dataframe dataframe
#' #' @param testcolumn The dataframe column to run the function on
#' #' @param pHospitalNum the hospital number of the patient so can be identified
#' #' @importFrom openxlsx write.xlsx
#' #' @keywords validation
#' #' @export
#' #' @examples #ValidationR(HistolBxSize,myHistol,"histology_report")
#' 
#' ValidationR<-function(FUN =funct,dataframe,Column,pHospitalNum){
#'   dd<-funct(dataframe,Column)
#'   #Now just want the columns that have changed or have been added from the original as well
#'   #as well as the columns that sound very similar to the original
#'   #ie need to get the original column called Column as well as the new column created (can
#'   # I get this from the diff between two data frames? dd-dataframe??)
#'   #This gets the new column name from the dataframe that the function has run
#'   MyNegsFiltered$FindingsAlgoNegs<-as.list(strsplit(MyNegsFiltered$FindingsAlgoNegs, ";"))
#'   MyNegsFiltered$negativeresults<-as.list(strsplit(MyNegsFiltered$negativeresults, ";"))
#'   #pHospitalNum
#'   ff<-setdiff(union(names(dataframe), names(dd)), names(dataframe))
#'   ff<-data.frame(dd[,Column],dd[,ff],dd[,pHospitalNum])
#'   write.xlsx(ff, paste0(getwd(),"/MyValidation.xlsx"))
#' }
#' #dataframe[,pHospitalNum],
