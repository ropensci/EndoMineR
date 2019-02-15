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


#############Main extraction################


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
  dataframeIn <- data.frame(dataframeIn,stringsAsFactors = FALSE)
  dataframeIn<-dataframeIn %>%
    tidyr::separate(!!Column, into = c("added_name",delim),
                    sep = paste(delim, collapse = "|"),
                    extra = "drop", fill = "right")
  names(dataframeIn) <- gsub(".", "", names(dataframeIn), fixed = TRUE)
  dataframeIn <- apply(dataframeIn, 2, function(x) gsub("\\\\.*", "", x))
  dataframeIn <- apply(dataframeIn, 2, function(x) gsub("       ", "", x))
  
  #Convert back to a dataframe as has been converted to a matrix
  dataframeIn<-data.frame(dataframeIn,stringsAsFactors = FALSE)
  dataframeIn<-dataframeIn[,-1]
  
  dataframeIn<- lapply(dataframeIn, function(x) ColumnCleanUp(x))
  
  names(dataframeIn) <- gsub(".","",names(dataframeIn),fixed=TRUE)
  dataframeIn<-data.frame(dataframeIn,stringsAsFactors = FALSE)
  #Add the original column back in so have the original reference
  dataframeIn$Original<-dataframeInForLater[,ColumnForLater]
  dataframeIn<-data.frame(dataframeIn,stringsAsFactors = FALSE)
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
  x[, t]<- ColumnCleanUp(x[, t])
  
  
  return(x)
}









##########Entity Relation functions ###################



#' EntityPairs_TwoSentence
#'
#' This is used to look for relationships between site and event especially for endoscopy events
#' @keywords Find and replace
#' @param EventColumn1 The relevant pathology text column
#' @param EventColumn2 The alternative pathology text column
#' @importFrom stringr str_replace_na str_c str_split str_which
#' @importFrom purrr flatten_chr map_chr map map_if
#' @examples # tbb<-EntityPairs_TwoSentence(SelfOGD_Dunn,"FINDINGS")

EntityPairs_TwoSentence<-function(dataframe,EventColumn){
  
  dataframe<-data.frame(dataframe,stringsAsFactors = FALSE)
  text<-textPrep(dataframe,EventColumn)
  text<-lapply(text,function(x) tolower(x))
  
  
  #Some clean up to get rid of white space- all of this prob already covered in the ColumnCleanUp function but for investigation later
  text<-lapply(text,function(x) gsub("[[:punct:]]+"," ",x))
  tofind <-tolower(LocationList())
  EventList<-unique(tolower(unlist(EventList(),use.names = FALSE)))
  
  
  text<-sapply(text,function(x) {
    
    #browser()
    x<-trimws(x)
    
    #browser()
    try(words <-
          x %>%
          unlist() %>%
          str_replace_na()%>%
          str_c(collapse = ' ') %>%
          str_split(' ') %>%
          `[[`(1))
    
    
    
    words<-words[words != ""] 
    x1 <- str_extract_all(tolower(x),tolower(paste(unlist(EventList()), collapse="|")))
    i1 <- which(lengths(x1) > 0)
    
    
    try(if(any(i1)) {
      EventList %>%
        map(
          ~words %>%
            str_which(paste0('^.*', .x)) %>%
            map_chr(
              ~words[1:.x] %>%
                str_c(collapse = ' ') %>%
                
                str_extract_all(regex(tofind, ignore_case = TRUE)) %>%
                map_if(is_empty, ~ NA_character_) %>%
                purrr::flatten_chr()%>%
                `[[`(length(.)) %>%
                
                .[length(.)]
            ) %>%
            paste0(':', .x)
        ) %>%
        unlist() %>%
        str_subset('.+:')
      
    } else "")
    
  }
  )
  return(text)
}


#' EntityPairs_OneSentence 
#'
#' This needs some blurb to be written. Used in the SentenceWordPairs
#' @keywords PathPairLookup
#' @param EventColumn1 The relevant pathology text column
#' @param EventColumn2 The alternative pathology text column
#' @importFrom purrr flatten_chr map_chr map map_if
#' @examples # tbb<-EntityPairs_OneSentence(SelfOGD_Dunn,"MACROSCOPICALDESCRIPTION")

EntityPairs_OneSentence<-function(dataframe,EventColumn){
  
  dataframe<-data.frame(dataframe,stringsAsFactors = FALSE)
  
  HistolType<-paste0(unlist(tissue,use.names=F),collapse="|")
  
  LocationList<-paste0(unlist(All,use.names=F),collapse="|")
  
  text<-textPrep(dataframe,EventColumn)
  r1 <-lapply(text,function(x) Map(paste, str_extract_all(tolower(x),tolower(LocationList)), str_extract_all(tolower(x),tolower(HistolType)), MoreArgs = list(sep=":")))
  
  r1<-lapply(r1,function(x) unlist(x))
  
  #Unlist into a single row-This should output a character vector
  out<-lapply(r1,function(x) paste(x,collapse=","))
  
  return(out)
}



##########Single column clean up functions##########



#' textPrep function
#'
#' This is a helper function to prepare the data for the extraction of event, tissue type and site
#' from raw pathology and endoscopy files associated with the endoscopy. This is used within the 
#' BarrettsPathSite and BarrettsEventType function and is also used to generate OPCS-4 codes. Note
#' it needs a dataframe not a tibble so this needs to be converted prior to usage.
#' @keywords Find and replace
#' @param EventColumn The relevant pathology text column
#' @importFrom stringi stri_split_boundaries
#' @examples # textPrep(SelfOGD_Dunn,"FINDINGS")
#' 
textPrep<-function(dataframe,EventColumn){
  
  #1. Flatten the text
  dataframe[,EventColumn]<-tolower(dataframe[,EventColumn])
  
  HistolType<-paste0(unlist(tissue,use.names=F),collapse="|")
  
  LocationList<-paste0(unlist(All,use.names=F),collapse="|")
  
  EventList<-paste0(unlist(Event,use.names=F),collapse="|")

  
  #1b General cleanup tasks
  dataframe[, EventColumn] <- ColumnCleanUp(dataframe[,EventColumn])
  
  #1c. Get rid of unnecessary punctuation
  dataframe[,EventColumn]<-gsub("'","",dataframe[,EventColumn],fixed=TRUE)
  
  #2a . Fuzzy find and replace and term mapping using the find and replace function above using the Location list
  L <- tolower(str_split(LocationList,"\\|"))
  dataframe[,EventColumn]<-Reduce(function(x, nm) spellCheck(nm, L[[nm]], x), init = dataframe[,EventColumn], names(L))
  
  #2b . Fuzzy find and replace and term mapping using the find and replace function above using the Path_Type list
  L <- tolower(str_split(HistolType,"\\|"))
  dataframe[,EventColumn]<-Reduce(function(x, nm) spellCheck(nm, L[[nm]], x), init = dataframe[,EventColumn], names(L))
  
  #2c. Fuzzy find and replace and term mapping using the find and replace function above
  L <- tolower(unique(unlist(EventList, use.names = FALSE)))
  dataframe[,EventColumn]<-Reduce(function(x, nm) spellCheck(nm, L[[nm]], x), init = dataframe[,EventColumn], names(L))
  
  #3.Remove all the negative phrases from the report:
  dataframe[,EventColumn]<-NegativeRemove(dataframe,EventColumn)
  
  #4. Need to run the TermStandardLocalizer here to make sure everything standardised.
  dataframe[,EventColumn]<-DictionaryLookup(dataframe[,EventColumn])
  
  #5. Split the lines so text is tokenized by sentence
  standardisedTextOutput<-stri_split_boundaries(dataframe[,EventColumn], type="sentence")
  
  #returns a nested list
  return(standardisedTextOutput)
}


#' Standardise location of biopsies or tissue samples
#'
#' Standardises the location of biopsies by cleaning up the common typos and
#' abbreviations that are commonly used in free text of pathology reports
#'
#' @param dataframe The dataframe
#' @param SampleLocation Column describing the Macroscopic sample from histology
#' @keywords Withdrawal
#' @export
#' @examples #Firstly we extract histology from the raw report
#' # using the extractor function
#' mywords<-c("Hospital Number","Patient Name:","DOB:","General Practitioner:",
#' "Date received:","Clinical Details:","Macroscopic description:",
#' "Histology:","Diagnosis:")
#' MypathExtraction<-Extractor(PathDataFrameFinal,"PathReportWhole",mywords)
#' names(MypathExtraction)[names(MypathExtraction) == 'Datereceived'] <- 'Dateofprocedure'
#' MypathExtraction$Dateofprocedure <- as.Date(MypathExtraction$Dateofprocedure)
#' # The function then standardises the histology terms through a series of
#' # regular expressions
#' ll<-DictionaryLookup(Mypath$Histology)
#' rm(MypathExtraction)



DictionaryLookup <- function(inputString,list) {

  key<-names(list)
  value<-as.character(t(data.frame(list,stringsAsFactors=FALSE))[,1])
  list<-data.frame(key,value,stringsAsFactors = FALSE)
  
  new_string <- inputString
  vapply(1:nrow(list),
         function (k) {
           new_string <<- gsub(list$key[k], list$value[k], new_string)
           0L
         }, integer(1))
  
  #dataframe<-as.data.frame(dataframe,stringsAsFactors=FALSE)
  #so<-str_match_all(new_string, LocationList())
  #Collapse as str_match_all outputs a list so need to collapse it to make into a character vector
  #so<-sapply( so, paste0, collapse=",")
  #dataframe$AllSampleLocator<-so
  return(new_string)
}








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
    "(but|although|however|though|apart| -|otherwise
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
      [Aa]lthough| [Hh]owever| [Tt]hough| [Aa]part| -|[Oo]therwise|
      [Uu]nremarkable)\\R*",
      "",
      dataframe[, Column],
      perl = TRUE,
      ignore.case = TRUE
      )
  # Nots
  dataframe[, Column] <-
    gsub(
      ".*(?:does|is|was|were|are|have) not.*?(\\.|\n|:|$)\\R*",
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
  
  #Withouts
  dataframe[, Column] <-
    gsub(
      ".*without.*\\R*",
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
  
  
  # Time related phrases eg post and previous
  dataframe[, Column] <- gsub(" (post|previous|prior)[^a-z].+?[A-Za-z]{3}",
                              " TIME_REPLACED",
                              dataframe[, Column],
                              perl = TRUE,
                              ignore.case = TRUE)
  
  
  return(dataframe[, Column])
}

#' Find and Replace function
#'
#' This is a helper function for finding and replacing from dictionaries like the event list
#' It uses fuzzy find and replace to account for spelling errors
#' @keywords Find and replace
#' @examples # Pending
#' 


spellCheck <- function(pattern, replacement, x, fixed = FALSE, ...) {
  m <- aregexec(pattern, x, fixed = fixed,ignore.case = T)
  r <- regmatches(x, m)
  lens <- lengths(r)
  if (all(lens == 0)) return(x) else
    replace(x, lens > 0, mapply(sub, r[lens > 0], replacement, x[lens > 0]))
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
#' @importFrom stringi stri_split_boundaries
#' @examples ii<-ColumnCleanUp(Myendo$Findings)


ColumnCleanUp <- function(vector) {
  
  
  #Optimise for tokenisation eg full stops followed by a number need to change so add a Letter before the number
  vector<-gsub("\\.\\s*(\\d)","\\.T\\1",vector)
  vector<-gsub("([A-Za-z]\\s*)\\.(\\s*[A-Za-z])","\\1\n\\2",vector)
  vector<-gsub("([A-Za-z]+.*)\\?(.*[A-Za-z]+.*)","\\1 \\2",vector)
  
  #Get rid of query type punctuation:
  vector<-gsub("(.*)\\?(.*[A-Za-z]+)","\\1 \\2",vector)
  
  
  #Have to tokenize here so you can strip punctuation without getting rid of newlines
  standardisedTextOutput<-stri_split_boundaries(vector, type="sentence")
  
  #Get rid of whitespace
  standardisedTextOutput<-lapply(standardisedTextOutput, function(x) trimws(x))
  
  #Get rid of trailing punctuation
  standardisedTextOutput<-lapply(standardisedTextOutput,function(x) gsub("^[[:punct:]]+","",x))
  standardisedTextOutput<-lapply(standardisedTextOutput,function(x) gsub("[[:punct:]]+$","",x))
  #Question marks result in tokenized sentences so whenever anyone write query Barrett's, it gets split.
  standardisedTextOutput<-lapply(standardisedTextOutput,function(x) gsub("([A-Za-z]+.*)\\?(.*[A-Za-z]+.*)","\\1 \\2",x))
  
  
  #Get rid of strange things
  standardisedTextOutput<-lapply(standardisedTextOutput,function(x) gsub("\\.\\,"," ",x))
  
  retVector<-sapply(standardisedTextOutput, function(x) paste(x,collapse="\n"))
  return(retVector)
}


#' Extrapolation from lists
#'
#'This takes a list of terms that you want to look for and
#'then extracts these into a new column
#'

#'
#' @param dataframe dataframe
#' @param Column The column you want to extract from 
#' @param mylist The list of values
#' @importFrom stringr str_extract_all
#' @keywords sensitivity and specificity
#' @export
#' @examples #To be set up 


##########Extrapolation##########
ExtrapolateFromLists <- function(Column,mylist) {
  #Extract all the ablation types from the merged columns
  ablationPositive<- str_extract_all(Column,mylist)
}


############## Endoscopy Clean-up functions##############


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
  dataframe[, EndoReportColumn] <- str_replace_all(dataframe[, EndoReportColumn],
                                                   "Mr|Professor|Prof|Dr|2nd.*|[^[:alnum:],]", "")
  # Put gaps between names
  dataframe[, EndoReportColumn] <- str_replace(dataframe[, EndoReportColumn],
                                               "([a-z])([A-Z])", "\\1 \\2")
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
    str_extract(dataframe[, MedColumn], "Fentanyl\\s*(\\d*(\\.\\d+)?)\\s*mcg")
  dataframe$Fent <- as.numeric(str_replace_all(dataframe$Fent,"Fentanyl|mcg ", ""))
  
  # Extract the midazolam if present
  
  dataframe$Midaz <-
    str_extract(dataframe$Medications, "Midazolam\\s*(\\d*(\\.\\d+)?)\\s*mg")
  dataframe$Midaz <- as.numeric(str_replace_all(dataframe$Midaz,"Midazolam|mg ", ""))
  
  
  # Extract the pethidine if present
  dataframe$Peth <-
    str_extract(dataframe[, MedColumn], "Pethidine\\s*(\\d*(\\.\\d+)?)\\s*mcg")
  dataframe$Peth <- as.numeric(str_replace_all(dataframe$Peth,"Pethidine|mcg ", ""))
  
  
  # Extract the propofol if present
  dataframe$Prop <-
    str_extract(dataframe[, MedColumn], "Propofol\\s*(\\d*(\\.\\d+)?)\\s*mcg")
  dataframe$Prop <- as.numeric(str_replace_all(dataframe$Prop,"Propofol|mcg ", ""))
  
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
  
  dataframe[, InstrumentColumn] <- trimws(toupper(str_replace_all(dataframe[, InstrumentColumn],
"X.*[Ll][Oo][Aa][Nn] [Ss][Cc][Oo][Pp][Ee] \\(|
Loan Scope \\(specify serial no:|
Loan Scope \\(specify\\s*serial no|\\)|-.*|,.*|
:|FC |[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee] |^,|
[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee]\\(specify serial no\\)\\s*|
[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee]\\(specify serial no:\\)\\s*", "")))
  dataframe[, InstrumentColumn] <- str_replace(dataframe[, InstrumentColumn],
                                               "FC ", "FC")
  dataframe[, InstrumentColumn] <- str_replace(dataframe[, InstrumentColumn],
                                               "^\\s*([1-9])", "A\\1")
  
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
  dataframe[, ProcPerformed] <- str_replace_all(dataframe[, ProcPerformed],
                                                "Withdrawal.*|Quality.*|Adequate.*|
                                                Good.*|Poor.*|None.*|FINDINGS", "")
  
  return(dataframe)
}









#' EndoscopyEvent 
#'
#' This extracts the endoscopic event. It looks for the event term and then looks in the event sentence as well as the one above to see if
#' the location is listed. It needs to be run AFTER the HistolTypeAndSite function as emr needs to be
#' added to the event. Used in the OPCS4 coding
#' @keywords Find and replace
#' @param EventColumn1 The relevant endoscopt free text column describing the findings
#' @param Procedure Column saying which procedure was performed
#' @param Macroscopic Column describing all the macroscopic specimens
#' @param Histology Column with free text histology (usually microscopic histology)
#' @export
#' @examples # SelfOGD_Dunn$EndoscopyEvent<-EndoscopyEvent(SelfOGD_Dunn,"FINDINGS","PROCEDUREPERFORMED","MACROSCOPICALDESCRIPTION","HISTOLOGY")

EndoscopyEvent<-function(dataframe,EventColumn1,Procedure,Macroscopic,Histology){

  
  dataframe<-data.frame(dataframe,stringsAsFactors = FALSE)
  
  # Extract the events from the 
  output<-EntityPairs_TwoSentence(dataframe,EventColumn1)
  
  MyHistolEvents<-HistolTypeAndSite(dataframe,Procedure,Histology,Macroscopic)
  output<-unlist(lapply(output, function(x) paste(x,collapse=";")))
  
  #Add emr only if this is seen in the histopath
  #Remove EMR from events if not seen in histopath
  
  #If emr is in the histology and in the event then leave it
  output<-ifelse(grepl("emr",MyHistolEvents,ignore.case = TRUE)&grepl("(oesophagus|goj):emr",output,ignore.case = TRUE),output,
                 #If emr is in the histology but not in the event then dont add it (sometimes EMR written in the request as past therapy
                 #but  it hasn't actually been done)
                 ifelse(grepl("emr",MyHistolEvents,ignore.case = TRUE)&!grepl("emr",output,ignore.case = TRUE),gsub("[A-Za-z]+:emr","",output),
                        #If emr is not in the histology but is in the event then remove from the event, otherwise leave as output
                        ifelse(!grepl("emr|nodul",MyHistolEvents,ignore.case = TRUE)&grepl("emr",output,ignore.case = TRUE),gsub("[A-Za-z]+:emr","",output),output)))
  
  
  
  d<-lapply(output, function(x) strsplit(x,";"))
  t<-lapply(d,function(x) unlist(x))
  out<-lapply(t,function(x) unique(x))
  output<-unlist(lapply(out, function(x) paste(x,collapse=";")))
  #output<-unlist(output)
  #Need to know if emr done here so can add it
  return(output)
  
  ######To do
  #2. Add the emr as a therapy- get this from where?
  #3. Make sure that if the column is empty or there is an error that the Procedure Column is then looked at and entereed
  #4. 
}






##########Histology clean up functions##########


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
  dataframe[, HistolColumn] <- str_replace(dataframe[, HistolColumn],
                                           "Dr.*?[A-Za-z]+", "")
  dataframe[, HistolColumn] <- str_replace(dataframe[, HistolColumn],
                                           "[Rr]eported.*", "")
  # Column-generic cleanup
  
  ListToConvert<-textPrep(dataframe, HistolColumn)
  
  dataframe[, HistolColumn] <- sapply(ListToConvert, function(x) paste(x,collapse="\n"))
  
  
  dataframe$Dx_Simplified <- dataframe[, HistolColumn]
  
  # Column-specific cleanup
  dataframe$Dx_Simplified <-
    gsub("- ", "\n", dataframe$Dx_Simplified, fixed = TRUE)
  dataframe$Dx_Simplified <-
    gsub("-[A-Z]", "\n", dataframe$Dx_Simplified, fixed = TRUE)
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
#' @importFrom stringr str_match_all str_replace_all
#' @keywords Biopsy number
#' @export
#' @examples
#' qq<-HistolNumbOfBx(Mypath,'Macroscopicdescription','specimen')


HistolNumbOfBx <- function(dataframe, MacroColumn, regString) {
  dataframe <- data.frame(dataframe)
  dataframe <- HistolMacDescrip(dataframe, MacroColumn)
  mylist <-
    #I need to collapse the unlist
    stringr::str_match_all(dataframe[, MacroColumn], 
                           paste(unlist(lapply(strsplit(regString,"\\|",fixed=FALSE),
                                               function(x){paste("[0-9]{1,2}.{0,3}",x, sep = "")})),collapse="|"))
  dataframe$NumbOfBx <-
    vapply(mylist, function(p)
      sum(as.numeric(stringr::str_replace_all(p,regString,""))),numeric(1))
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
  dataframe$BxSize <- str_replace(dataframe$BxSize,"the largest measuring |mm|less than", "")
  strBxSize <- "([0-9]+).*?([0-9])+.*?([0-9])"
  dataframe$BxSize <-
    as.numeric(str_match(dataframe$BxSize, strBxSize)[, 2]) *
    as.numeric(str_match(dataframe$BxSize, strBxSize)[, 3]) *
    as.numeric(str_match(dataframe$BxSize, strBxSize)[, 4])
  return(dataframe)
}




#' HistolTypeAndSite 
#'
#' This needs some blurb to be written. Used in the OPCS4 coding
#' @keywords Find and replace
#' @param Procedure The procedure performed
#' @param EventColumn1 The relevant pathology text column
#' @param EventColumn2 The alternative pathology text column
#' @export
#' @examples # SelfOGD_Dunn$PathSite<-HistolTypeAndSite(SelfOGD_Dunn,"PROCEDUREPERFORMED","MACROSCOPICALDESCRIPTION","HISTOLOGY")

HistolTypeAndSite<-function(dataframe,Procedure,EventColumn1,EventColumn2){
  library(stringi)
  library(stringr)
  
  dataframe<-data.frame(dataframe,stringsAsFactors = FALSE)
  
  # RUN THE LOOKUP IN THE TWO COLUMNS
  
  output<-ifelse(EntityPairs_OneSentence(dataframe,EventColumn1)=="NA:NA", 
                 EntityPairs_OneSentence(dataframe,EventColumn2),
                 EntityPairs_OneSentence(dataframe,EventColumn1))
  
  #If there is only a colon (punctuation) and then empty then assume it is a biopsy
  output<-str_replace_all(output, ":,|:$", ":biopsy,")
  
  #Make sure only unique values represented:
  output<-lapply(output, function(x) paste(unlist(unique(unlist(strsplit(x,",")))),collapse=","))
  
  output<-ifelse(grepl("Gastroscopy",dataframe[,Procedure]),
                 str_remove_all(output, paste0('(',tolower( paste0(unlist(lower,use.names=F),collapse="|")),')',':biopsy')),
                 ifelse(grepl("Colonoscopy|Flexi",dataframe[,Procedure]),
                        str_remove_all(output, paste0('(',tolower(paste0(unlist(upper,use.names=F),collapse="|")),')',':biopsy')),output))
  return(output)
}



#' HistolTissueIndex 
#'
#' This returns a number for all the biopsies taken based on distance from orifice. It is for biopsies only
#' @keywords Pathology biopsy index
#' @export
#' @param PathSite The column that has the pathology locatio and tissue type from HistolTypeAndSite
#' @ImportFrom stringr str_extract_all
#' @Import tidyverse
#' @Import fuzzyjoin
#' @examples # SelfOGD_Dunn<-read_excel("/home/rstudio/GenDev/DevFiles/EndoMineRFunctionDev/SelfOGD_Dunn.xlsx")
#' SelfOGD_Dunn$PathSite<-HistolTypeAndSite(SelfOGD_Dunn,"MACROSCOPICALDESCRIPTION","HISTOLOGY")
#' HistolBiopsyIndex(SelfOGD_Dunn,"PathSite") 

HistolBiopsyIndex<-function(dataframe,PathSite){
  library(fuzzyjoin)

  HistolType<-paste0(unlist(tissue,use.names=F),collapse="|")
  ToIndex<-str_extract_all(dataframe$PathSite,paste0("(^|,)[a-z]+:?(",tolower(HistolType),")(|$)"))
  
  
  
  #paste0("(^|,)[a-z]+:?(",tolower(HistolType),")(|$)")
  #tolower(HistolType)
  ToIndex<-lapply(ToIndex, function(x) unique(x))
  #Give each an index in the list (taken from the location list)
  
  
  #The results to replace 
  replace<-c("ileum:biopsy","ileocaecal:biopsy","caecum:biopsy","ascending:biops","hepatic:biopsy","transverse:biopsy", "splenic:biopsy","descending:biopsy",
             "sigmoid:biopsy","rectosigmoid:biopsy","rectum:biopsy", 
             "ileoanal:biopsy","prepouch:biopsy","pouch:biopsy", 
             "duodenum:biopsy","antrum:biopsy","stomach:biopsy","goj:biopsy", "cardia:biopsy",
             "oesophagus:biopsy","colon:biopsy","oesophagus:emr","goj:emr","stomach:emr","duodenum:emr")
  
  #C stand for colon (and all lower bowel investigations) S stands for surgical O stands for OGD. 
  replaceValue<-c("C11","C10","C9","C8","C7","C6","C5","C4","C3","C2","C1","S1","S2","S3","O5","O4","O3","O1","O2","O1","colon","O1","O1","O3","O5")
  
  #Create a tibble to merge with the list
  d1 <- tibble(key = replace, val = replaceValue)
  
  
  #Select the elements that have characters in them
  i1 <- lengths(ToIndex) > 0 
  
  #Do the merge
  ToIndex[i1] <- map(ToIndex[i1], ~ 
                       tibble(key = .x) %>%
                       regex_left_join(d1) %>%
                       pull(val))
  
  ToIndex<-lapply(ToIndex, function(x) unlist(x,recursive=F))
  ToIndex<-unlist(lapply(ToIndex, function(x) paste(x,collapse=";")))
  
  return(ToIndex)
}






##########Lists##########

#' Extract pathology type
#'
#' This standardizes terms to describe the pathology tissue type being exmained
#' @param dataframe dataframe with column of interest
#' @keywords Pathology type
#' @export
#' @examples #No examples as just returns a list



HistolType <- function() {
  
  #First standardise the terms
  
  tissue<-list("Resection" = "Duodenum", 
              "bx|biopsies"="Biopsy",
              "(endoscopic mucosal resection)|(endoscopic mucosectomy)"="EMR",
              "endoscopic submucosal (dissection|resection)"="ESD",
              "nodul"="nodul",
              "polyp "="polyp")
  
  
  #To get the list as a list of values only in a regex use
  #paste0(unlist(tissue,use.names=F),collapse="|")
  
  return(tissue)
}



#' Use list of standard locations
#'
#' The is a list of standard locations at endoscopy that is used in the TermStandardLocator as well
#' as extraction of the site of biopsies/EMRs and potentially in functions looking at the site of a 
#' therapeutic event. It just returns the list in the function
#'
#'
#' @keywords Location
#' @export
#' @examples #No example needed

LocationList<-function(){
  
  All<-append(LocationListLower(), LocationListUpper())
  All<-append(All,LocationListUniversal())
  
  #To get the list as a list of values only in a regex use
  #paste0(unlist(All,use.names=F),collapse="|")
  
  return(All)
  
}

#' Use list of standard locations for upper GI endoscopy
#'
#' The is a list of standard locations at endoscopy that is used in the TermStandardLocator as well
#' as extraction of the site of biopsies/EMRs and potentially in functions looking at the site of a 
#' therapeutic event. It just returns the list in the function
#'
#'
#' @keywords Location
#' @export
#' @examples #No example needed

LocationListUpper<-function(){
  
  
  upper<-list("duodenum|d2|D2|duodenal" = "Duodenum", 
       "gastric|stomach"="Stomach",
       "antrum|antral"="Antrum",
       "ogj|goj|gastrooesophageal"="GOJ",
       "fundal|fundus"="stomach",
       "pyloric"="Pylorus",
       "gastric cardia"="Cardia",
       "oesophagus|oesophageal"="Oesophageal")

  
  #To get the list as a list of values only in a regex use
  #paste0(unlist(upper,use.names=F),collapse="|")
  
  return(upper)
  
}

#' Use list of standard locations for upper GI endoscopy
#'
#' The is a list of standard locations at endoscopy that is used in the TermStandardLocator as well
#' as extraction of the site of biopsies/EMRs and potentially in functions looking at the site of a 
#' therapeutic event. It just returns the list in the function
#'
#'
#' @keywords Location
#' @export
#' @examples #No example needed

LocationListUniversal<-function(){
  universal<-list("Anastomosis" = "Ascending ")
  return(universal)
  
  #If need to use then use
  #paste0(unlist(universal,use.names=F),collapse="|")

}


#' Use list of standard locations for lower GI endoscopy
#'
#' The is a list of standard locations at endoscopy that is used in the TermStandardLocator as well
#' as extraction of the site of biopsies/EMRs and potentially in functions looking at the site of a 
#' therapeutic event. It just returns the list in the function
#'
#'
#' @keywords Location
#' @export
#' @examples #No example needed

LocationListLower<-function(){
  
  lower<-list("asce |ascending|(ascend[^a-z])|( colon r )|(r colon)|(asc )|(right colon)" = "Ascending ",
  "descending|(descen[^a-z])|(desc[^a-z])|(des[^a-z])|(colon l)|(l colon)|(left colon)" = "Descending ",
  "sigmoid|(sigm[^a-z])|sigmo "= "Sigmoid ",
  "rectal|rectum|(rectum[a-z])|rect "="Rectum ",
  "transverse|(transv[^a-z])|tranv |trans "="Transverse ",
  "caecum|caecal"="Caecum ",
  "splenic"="Splenic ",
  "(ileum )|( ileal )"="Ileum ",
  "rectosigmoid"="Rectosigmoid ",
  "(ileocaecal\\s*)|icv|(ileo-caecum)"="Ileocaecal", 
  "(hep[^a-z])|([Hh]epatic)"="Hepatic ",
  "colonic|colon |(col[^a-z])"="Colon ",
  "term |terminal"="Terminal",
  "TI "="Terminal",
  "caec"="Caecum ",
  "[Ss]ig "="Sigmoid ",
  "ileo\\s*-\\s*anal|ileo\\s*anal "="Ileoanal ",
  "(pre\\s*pouch)|(pre-[Pp]ouch)"="PrePouch",
  "pouch"="Pouch",
  "term |terminal"="Terminal")
  
  
  #To get the list as a list of values only in a regex use
  #paste0(unlist(lower,use.names=F),collapse="|")

  return(lower)
  
}


#' Use list of catheters used in radiofrequency ablation
#'
#' The takes a list of catheters used in radiofrequency ablation
#'
#' @keywords RFA
#' @export
#' @examples #No example needed

RFACath<-function(){
  
  tofind <-
    paste(
      c(
        "90","360","HALO60"," 60","TTS",
        "[Cc]hannel","APC"
      ),
      collapse = "|"
    )
  
  return(tofind)
  
}

#' Event list
#'
#' This function returns all the conversions from common version of events to 
#' a standardised event list, much like the Location standardidastion function
#' It is used in the Barretts_EventType. This does not include EMR as this is 
#' extracted from the pathology so is part of pathology type.
#' @keywords Event extraction
#' @examples # unique(unlist(EventList(), use.names = FALSE))
#' 
EventList<-function(){
  
  Event <- list("radiofrequency ablation" = "RFA", 
                "argon plasma coagulation" = "APC",
                "halo" = "RFA",
                " rfa"= "RFA",
                "dilatation"="dilat",
                "dilated"="dilat",
                "apc"="APC",
                " emr"="EMR",
                "(endoscopic mucosal resection)|(endoscopic mucosectomy)"="EMR",
                "clip"="clip",
                "grasp"="grasp",
                "iodine"="iodine", 
                "acetic"="acetic",
                "NAC"="NAC",
                "Brushings"="brushings"
  )
  
  #To get the list as a list of values only in a regex use
  #paste0(unlist(Event,use.names=F),collapse="|")
  
  return(Event)
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
#' sensAndSpecificMultip<-function(FUN =funct,dataframe,Column,pHospitalNum){
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


#' #' Validate columns
#' #'
#' #' This is an attempt to determine whether functions are accurate in their extraction
#' #' It is part of a validation pipeline to make sure the data outputted from a function
#' #' is accurate
#' #' It essentially is a comparison of actual vs expected output where there are multiple 
#' #' outputs in the column
#' #' The intention is to then quantify the output of a function for a specific dataset.
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
#' 


##################Validatation Workflow##########################
#' Sensitivity and Specificity of text mining functions
#'
#'This is part of the validation workflow. The idea is that 
#'There is a dataframe which contains a column with the reference value
#'and another column with the actual value. If the test or reference has not
#'detected a value then this is a negative and is labelled as 'Insufficient'
#'

#'
#' @param dataframe dataframe
#' @param ref The column with the reference values
#' @param actual The column with the actual values
#' @importFrom rlang sym
#' @importFrom dplyr case_when mutate
#' @keywords sensitivity and specificity
#' @export
#' @examples #To be set up 


SensAndSpecif<-function(dataframe,ref,actual){
  
  refa <- rlang::sym(ref)
  actuala <- rlang::sym(actual)
  
  df<-dataframe %>%
    mutate(
      SensAndSpec= case_when(
        !grepl("[Ii]nsufficient",!!actuala,ignore.case = TRUE) & !!actuala==!!refa ~ "TP",
        !!actuala!=!!refa & !grepl("[Ii]nsufficient",!!actuala) ~  "FP",
        grepl("[Ii]nsufficient",!!actuala) & !!actuala!=!!refa ~ "FN",
        grepl("[Ii]nsufficient",!!actuala,ignore.case = TRUE) & grepl("[Ii]nsufficient",!!refa,ignore.case = TRUE) ~ "TN",
        TRUE~"other")
    )
  
  
  TP<-ifelse(is.na(as.integer(table(df$SensAndSpec)["TP"])),0,as.integer(table(df$SensAndSpec)["TP"]))
  FP<-ifelse(is.na(as.integer(table(df$SensAndSpec)["FP"])),0,as.integer(table(df$SensAndSpec)["FP"]))
  TN<-ifelse(is.na(as.integer(table(df$SensAndSpec)["TN"])),0,as.integer(table(df$SensAndSpec)["TN"]))
  FN<-ifelse(is.na(as.integer(table(df$SensAndSpec)["FN"])),0,as.integer(table(df$SensAndSpec)["FN"]))
  
  
  Sensitivity<-round(TP/(TP+FN)*100,digits=2)
  Specificity<-round(TN/(TN+FP)*100,digits=2)
  PPV<-round(TP / (TP + FP),digits=2)
  NPV<- round(TN / (FN + TN),digits=2)
  Accuracy<-round(TP+TN/(TP+TN+FP+FN),digits=2)
  
  stats<-list(Sensitivity=Sensitivity,Specificity=Specificity,PPV=PPV,NPV=NPV,Accuracy=Accuracy)
  
  return(stats)
}



