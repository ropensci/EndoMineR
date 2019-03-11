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
      "Mypath",
      "doc_id",
      "sentence",
      "upos",
      "xpos",
      "dep_rel",
      "misc",
      "has_morph",
      "morph_abbr",
      "morph_case",
      "morph_definite",
      "morph_degree",
      "morph_foreign",
      "morph_gender",
      "morph_mood",
      "morph_number",
      "morph_numtype",
      "morph_person",
      "morph_poss",
      "morph_prontype",
      "morph_reflex",
      "morph_tense",
      "morph_verbform",
      "str_subset",
      "tibble",
      "lower",
      "upper",
      "pathSiteString",
      "outputFinal",
      "theme_foundation",
      "udmodel",
      "x3",
      "FINDINGSmyDx",
      "FindingsAfterProcessing",
      "primDxVector"
    )
  )











##########Text preparation##########



#' textPrep function
#'
#' THis function prepares teh data by cleaning 
#' punctuation, checking spelling against the lexicons, mapping terms
#' accorsing to the lexicons, removing negative expressions
#' and lower casing everything. 
#' @keywords Find and replace
#' @param inputText The relevant pathology text column
#' @importFrom stringi stri_split_boundaries
#' @export
#' @return This returns a string vector.
#' @examples CleanResults<-textPrep(TheOGDReportFinal$OGDReportWhole)
#' 
textPrep<-function(inputText){
  
  #1. Flatten the text
  inputText<-tolower(inputText)
  

  #1b General cleanup tasks
  inputText <- ColumnCleanUp(inputText)
  

  
  #2a . Fuzzy find and replace and term mapping using the find and replace function above using the Location list
  
  HistolType<-paste0(unlist(HistolType(),use.names=F),collapse="|")
  LocationList<-paste0(unlist(LocationList(),use.names=F),collapse="|")
  EventList<-paste0(unlist(EventList(),use.names=F),collapse="|")
  
  #Spellcheck using the lexicons as reference
  L <- tolower(str_split(LocationList,"\\|"))
  inputText<-Reduce(function(x, nm) spellCheck(nm, L[[nm]], x), init = inputText, names(L))
  L <- tolower(str_split(HistolType,"\\|"))
  inputText<-Reduce(function(x, nm) spellCheck(nm, L[[nm]], x), init = inputText, names(L))
  L <- tolower(unique(unlist(EventList, use.names = FALSE)))
  inputText<-Reduce(function(x, nm) spellCheck(nm, L[[nm]], x), init = inputText, names(L))
  
  
  #3.Remove all the negative phrases from the report:
  inputText<-NegativeRemove(inputText)
  
  #4. Need to map the terms to the lexicons to make sure everything standardised.
  inputText<-DictionaryInPlaceReplace(inputText,LocationList())
  inputText<-DictionaryInPlaceReplace(inputText,EventList())
  inputText<-DictionaryInPlaceReplace(inputText,HistolType())
  
  #5. Split the lines so text is tokenized by sentence
  #standardisedTextOutput<-stri_split_boundaries(inputText, type="sentence")
  
  #returns a 
  inputText<-tolower(inputText)
  return(inputText)
}


#' Dictionary In Place Replace
#'
#' This maps terms according to the lexicons and replaces them with the 
#' standardised term within the text. It is used within the textPrep function
#'
#' @param inputString the input string
#' @param list The replacing list
#' @keywords Replace
#' @export
#' @return This returns a character vector
#' @examples inputText<-DictionaryInPlaceReplace(TheOGDReportFinal$OGDReportWhole,LocationList())



DictionaryInPlaceReplace <- function(inputString,list) {

  key<-names(list)
  value<-as.character(t(data.frame(list,stringsAsFactors=FALSE))[,1])
  list<-data.frame(key,value,stringsAsFactors = FALSE)
  
  new_string <- inputString
  vapply(1:nrow(list),
         function (k) {
           new_string <<- gsub(list$key[k], list$value[k], new_string)
           0L
         }, integer(1))
  
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
#' @param inputText column of interest
#' @keywords Negative Sentences
#' @importFrom stringr str_replace
#' @export
#' @return This returns a column within a dataframe. THis should be changed to a 
#' character vector eventually
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
#' hh<-NegativeRemove(anexample$Thecol)

NegativeRemove <- function(inputText) {
  # Conjunctions
  inputText <- gsub(
    "(but|although|however|though|apart| -|otherwise
    |unremarkable|\\,)[a-zA-Z0-9_ ]+(no |negative|
    unremarkable|-ve|normal).*?(\\.|\\n|:|$)\\R*",
    "\\.\n",
    inputText,
    perl = TRUE,
    ignore.case = TRUE
    )
  inputText <-
    gsub(
      "(no |negative|unremarkable|-ve| normal) .*?([Bb]ut|
      [Aa]lthough| [Hh]owever| [Tt]hough| [Aa]part| -|[Oo]therwise|
      [Uu]nremarkable)\\R*",
      "",
      inputText,
      perl = TRUE,
      ignore.case = TRUE
      )
  # Nots
  inputText<-
    gsub(
      ".*(?:does|is|was|were|are|have) not.*?(\\.|\n|:|$)\\R*",
      "",
      inputText,
      perl = TRUE,
      ignore.case = TRUE
    )
  inputText <-
    gsub(
      "not (biop|seen).*?(\\.|\n|:|$)\\R*",
      "",
      inputText,
      perl = TRUE,
      ignore.case = TRUE
    )
  # Nos
  inputText <-
    gsub(
      ".*(?:((?<!with)|(?<!there is )|(?<!there are ))\\bno\\b(?![?:A-Za-z])|
      ([?:]\\s*N?![A-Za-z])).*\\R*",
      "",
      inputText,
      perl = TRUE,
      ignore.case = TRUE
      )
  
  #Withouts
  inputText <-
    gsub(
      ".*without.*\\R*",
      "",
      inputText,
      perl = TRUE,
      ignore.case = TRUE
    )
  
  
  inputText <-
    gsub(
      ".*(:|[?])\\s*(\\bno\\b|n)\\s*[^A-Za-z0-9].*?(\\.|\n|:|$)
      \\R*",
      "",
      inputText,
      perl = TRUE,
      ignore.case = TRUE
    )
  inputText <-
    gsub(
      ".*(negative|neither).*?(\\.|\n|:|$)\\R*",
      "",
      inputText,
      perl = TRUE,
      ignore.case = TRUE
    )
  # Keep abnormal in- don't ignore case as it messes it up
  inputText <- str_replace(inputText,
                                     ".*(?<!b)[Nn]ormal.*?(\\.|\n|:|$)", "")
  # Other negatives
  inputText <- gsub(
    ".*there (is|are|were) \\bno\\b .*?(\\.|\n|:|$)\\R*",
    "",
    inputText,
    perl = TRUE,
    ignore.case = TRUE
  )
  inputText <- gsub(
    "(within|with) (normal|\\bno\\b) .*?(\\.|\n|:|$)\\R*",
    "",
    inputText,
    perl = TRUE,
    ignore.case = TRUE
  )
  # Specific cases
  inputText <- gsub(
    ".*duct.*clear.*?(\\.|\n|:|$)\\R*",
    "",
    inputText,
    perl = TRUE,
    ignore.case = TRUE
  )
  # Unanswered prompt lines
  inputText <- gsub(".*:(\\.|\n)\\R*",
                              "",
                    inputText,
                              perl = TRUE,
                              ignore.case = TRUE)
  
  
  # Time related phrases eg post and previous
  inputText <- gsub(" (post|previous|prior)[^a-z].+?[A-Za-z]{3}",
                              " TIME_REPLACED",
                    inputText,
                              perl = TRUE,
                              ignore.case = TRUE)
  
  
  return(inputText)
}

#' Find and Replace function
#'
#' This is a helper function for finding and replacing from dictionaries like the event list
#' It uses fuzzy find and replace to account for spelling errors
#' @keywords Find and replace
#' @importFrom utils aregexec
#' @param pattern the pattern to look for
#' @param replacement the pattern replaceme with
#' @param x the target string
#' @return This returns a character vector
#' @examples L <- tolower(str_split(HistolType,"\\|"))
#' inputText<-Reduce(function(x, nm) spellCheck(nm, L[[nm]], x), init = inputText, names(L))
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
#' @param vector column of interest
#' @keywords Cleaner
#' @export
#' @importFrom stringr str_replace 
#' @importFrom stringi stri_split_boundaries
#' @return This returns a character vector
#' @examples ii<-ColumnCleanUp(Myendo$Findings)


ColumnCleanUp <- function(vector) {
  
  
  #Optimise for tokenisation eg full stops followed by a number need to change so add a Letter before the number
  vector<-gsub("\\.\\s*(\\d)","\\.T\\1",vector)
  vector<-gsub("([A-Za-z]\\s*)\\.(\\s*[A-Za-z])","\\1\n\\2",vector)
  vector<-gsub("([A-Za-z]+.*)\\?(.*[A-Za-z]+.*)","\\1 \\2",vector)
  
  #Get rid of query type punctuation:
  vector<-gsub("(.*)\\?(.*[A-Za-z]+)","\\1 \\2",vector)
  vector<-gsub("'","",vector,fixed=TRUE)
  
  #Have to tokenize here so you can strip punctuation without getting rid of newlines
  standardisedTextOutput<-stri_split_boundaries(vector, type="sentence")
  
  #Get rid of whitespace
  standardisedTextOutput<-lapply(standardisedTextOutput, function(x) trimws(x))
  
  #Get rid of trailing punctuation
  standardisedTextOutput<-lapply(standardisedTextOutput,function(x) gsub("^[[:punct:]]+","",x))
  standardisedTextOutput<-lapply(standardisedTextOutput,function(x) gsub("[[:punct:]]+$","",x))
  #Question marks result in tokenized sentences so whenever anyone write query Barrett's, it gets split.
  standardisedTextOutput<-lapply(standardisedTextOutput,function(x) gsub("([A-Za-z]+.*)\\?(.*[A-Za-z]+.*)","\\1 \\2",x))
  standardisedTextOutput<-lapply(standardisedTextOutput,function(x) gsub("(Dr.*?[A-Za-z]+)|([Rr]eported.*)|([Dd]ictated by.*)","",x))
  
  
  #Get rid of strange things
  standardisedTextOutput<-lapply(standardisedTextOutput,function(x) gsub("\\.\\,"," ",x))
  
  retVector<-sapply(standardisedTextOutput, function(x) paste(x,collapse="\n"))
  return(retVector)
}






############# Main extraction ################


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
  Columna <- rlang::sym(Column)
  dataframeIn <- data.frame(dataframeIn,stringsAsFactors = FALSE)
  dataframeIn<-dataframeIn %>%
    tidyr::separate(!!Columna, into = c("added_name",delim),
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






############## Endoscopy Clean-up functions##############


#' Cleans endoscopist column if present
#'
#' If an endoscopist column is part of the dataset once the extractor
#' function has been used this cleans the endoscopist column from the report.
#' It gets rid of titles
#' It gets rid of common entries that are not needed.
#' It should be used after the Extractor.
#'
#' @param EndoscopistColumn The endoscopy text column
#' @keywords Endoscopist extraction
#' @export
#' @return This returns a character vector
#' @importFrom stringr str_replace
#' @examples Myendo$Endoscopist<-EndoscEndoscopist(Myendo$Endoscopist)


EndoscEndoscopist <- function(EndoscopistColumn) {
  # Extraction of the Endoscopist
  EndoscopistColumn <- str_replace_all(EndoscopistColumn,"Mr|Professor|Prof|Dr|2nd.*|[^[:alnum:],]", "")
  # Put gaps between names
  EndoscopistColumn <- str_replace(EndoscopistColumn,"([a-z])([A-Z])", "\\1 \\2")
  #Trim the whitespace
  EndoscopistColumn <- trimws(EndoscopistColumn,which = c("both"))
  return(EndoscopistColumn)
}




#' Cleans instrument column if present
#'
#' This cleans the Instument column from the report assuming such a column exists
#' (where instrument usually refers to the endoscope number being used.)
#' It gets rid of common entries that are not needed.
#' It should be used after the Extractor.
#' @param EndoInstrument column of interest
#' @keywords Instrument
#' @return This returns a character vector
#' @importFrom stringr str_replace
#' @export
#' @examples Myendo$Instrument<-EndoscInstrument(Myendo$Instrument)

EndoscInstrument <- function(EndoInstrument) {
  # Extraction of the Instrument used:
  EndoInstrument<- trimws(toupper(str_replace_all(EndoInstrument,
"X.*[Ll][Oo][Aa][Nn] [Ss][Cc][Oo][Pp][Ee] \\(|
Loan Scope \\(specify serial no:|
Loan Scope \\(specify\\s*serial no|\\)|-.*|,.*|
:|FC |[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee] |^,|
[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee]\\(specify serial no\\)\\s*|
[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee]\\(specify serial no:\\)\\s*", "")))
  EndoInstrument <- str_replace(EndoInstrument, "FC ", "FC")
  EndoInstrument <- str_replace(EndoInstrument,"^\\s*([1-9])", "A\\1")
  return(EndoInstrument)
}

############## Extraction Tools ###################


#' Temporal sentences
#'
#' This extracts sentences that have a tense to them so you can see
#' if there are intended actions for example in assessing quality
#' of a report eg 'I will prescribe this medication. The function
#' requires you to decide whether you want present, past of future tenses. You
#' can also choose to exclude tenses.
#' 
#'
#' @param inputString the string to be analysed
#' @keywords Macroscopic
#' @importFrom stringr str_replace_all
#' @importFrom udpipe udpipe_download_model udpipe_load_model udpipe_annotate cbind_morphological
#' @importFrom dpylr group_by summarise %>%
#' @export
#' @examples pp<-udmodel_english <- udpipe_load_model(file = udmodel$file_model)
#' Temporal(Myendo$Findings)

Temporal<-function(inputString){
  
  
  #Load udpipe annotator outside of function as take too much memory
  x <- udpipe_annotate(udmodel_english, x = inputString)
  x2 <- as.data.frame(x)
  
  
  #Ways to determine the temporality of the sentence. English has no future tense so it is determined
  #by examining the Verbform not being past or present and the presence of a modal auxilliary
  
  x2$Temporal<-ifelse(grepl("Pres",x2$feats),"Present",
                      ifelse(grepl("Past",x2$feats),"Past",
                             ifelse(grepl("AUX",x2$upos),"Future","NoTense")))
  
  #Now group back into the report text
  Newx<-x2 %>% group_by(doc_id,sentence)  %>% summarise(upos = paste(upos, collapse=";"),
                                                        xpos = paste(xpos, collapse=";"),
                                                        dep_rel = paste(dep_rel, collapse=";"),
                                                        misc = paste(misc, collapse=";"),
                                                        Temporal = paste(Temporal, collapse=";"))%>%group_by(doc_id)
  #This now gives the per sentence temporality
  Newx<-data.frame(Newx)
  
  #Now tag each sentence with paste and get rid of no tense and merge with the sentence
  Newx$Temporal<-paste(str_replace_all(Newx$Temporal,";NoTense|NoTense|^;",""),":::",Newx$sentence)
  
  #Now to group by whole text:
  Newxdoc<-Newx %>% 
    group_by(doc_id) %>% summarise(
      Temporal = paste(Temporal, collapse="\n"))
  
  #Return tagged character vector
return(Newxdoc$Temporal)
  
}





#' Parts of speech tagging for reports
#'
#' This uses udpipe to tag the text. It then compresses all of the text 
#' so you have continuous POS tagging or the whole text. The udpipe package has to be
#' pre downloaded to run this.
#' 
#'
#' @param dataframe dataframe
#' @keywords Macroscopic
#' @importFrom stringr str_replace
#' @importFrom udpipe udpipe_download_model udpipe_load_model udpipe_annotate cbind_morphological
#' @export
#' @examples pp<-HistolMacDescrip(Mypath$Macroscopicdescription)

EndoPOS<-function(dataframe){
  #Load outsie of function as take too much memory
#udmodel <- udpipe_download_model(language = "english")
#udmodel_english <- udpipe_load_model(file = udmodel$file_model)
#Get into a tokenised form first
#Myendo$Findings<-textPrep(Myendo$Findings)
x <- udpipe_annotate(udmodel_english, x = head(Myendo$Findings,100))
x2 <- as.data.frame(x)
#x3 <- cbind_morphological(x2) #Dont need this for temporal assessment
#x4<-cbind_dependencies(x3)  #Dont need this for temporal assessment

#now to select out the 




#To get all in one long cell per original document:
Newx<-x3 %>% group_by(doc_id,sentence)  %>% summarise(upos = paste(upos, collapse=";"),
                                             xpos = paste(xpos, collapse=";"),
                                             dep_rel = paste(dep_rel, collapse=";"),
                                             misc = paste(misc, collapse=";"),
                                             has_morph = paste(has_morph, collapse=";"))%>%group_by(doc_id)
Newx<-data.frame(Newx)


Newxdoc<-Newx %>% 
  group_by(doc_id) %>% summarise(
                                 sentence = paste(sentence, collapse="\n"),
                                 upos = paste(upos, collapse="\n"),
                                 xpos = paste(xpos, collapse=";"),
                                 dep_rel = paste(dep_rel, collapse=";"),
                                 misc = paste(misc, collapse=";"),
                                 has_morph = paste(has_morph, collapse=";"))

return(Newxdoc)
}

#' Extrapolate from Dictionary
#'
#' Provides term mapping and extraction in one.
#' Standardises any term according to a mapping lexicon provided and then
#' extracts the term. Thus is
#' different to the DictionaryInPlaceReplace in that it provides a new column
#' with the extracted terms as opposed to changing it in place
#'
#' @param inputString The text string to process
#' @param list of words to iterate through
#' @keywords Withdrawal
#' @importFrom fuzzyjoin regex_left_join
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
#' ll<-ExtrapolatefromDictionary(Mypath$Histology,HistolType())
#' rm(MypathExtraction)






ExtrapolatefromDictionary<-function(inputString,list){
  #lower case the input string
  inputString<-tolower(inputString)
  
  #Get the names from the named list
  mylist<-paste0(unlist(list,use.names=F),collapse="|")
  
  #Make the inputstrings unique
  ToIndex<-lapply(inputString, function(x) unique(x))
  #Give each an index in the list (taken from the location list)
  
  #The results to replace in the string
  replace<-names(list)
  
  #The result of the replacement 
  replaceValue<-paste0(unlist(list,use.names=F))
  
  #Create a tibble to merge with the list
  d1 <- tibble(key = replace, val = replaceValue)
  
  
  #Select the elements that have characters in them
  i1 <- lengths(ToIndex) > 0 
  
  #Do the merge. This takes the key to lookup and then if found, replaces
  #the value with value associated with it in the table. I think the pull
  #function also creates a new column with the value in it. This is an important
  #function as uses a table lookup
  ToIndex[i1] <- map(ToIndex[i1], ~ 
                       tibble(key = .x) %>%
                       regex_left_join(d1) %>%
                       pull(val))
  
  #This unlists the nested list
  ToIndex<-lapply(ToIndex, function(x) unlist(x,recursive=F))
  
  #This collapses the list so that the output is a single string
  ToIndex<-unlist(lapply(ToIndex, function(x) paste(x,collapse=";")))
  
  return(ToIndex)
}

############## Colocate ###################

#' EntityPairs_OneSentence 
#'
#' Use to see if words from two lists co-exist within a sentence. Eg site and tissue type.
#' @keywords PathPairLookup
#' @param inputText The relevant pathology text column
#' @param list1 First list to refer to
#' @param list2 The second list to look for
#' @importFrom purrr flatten_chr map_chr map map_if
#' @export
#' @examples # tbb<-EntityPairs_OneSentence(Mypath$Histology,HistolType(),LocationList())

EntityPairs_OneSentence<-function(inputText,list1,list2){
  
  #dataframe<-data.frame(dataframe,stringsAsFactors = FALSE)
  list1<-paste0(unlist(list1,use.names=F),collapse="|")
  list2<-paste0(unlist(list2,use.names=F),collapse="|")
  
  #text<-textPrep(inputText)
  text<-standardisedTextOutput<-stri_split_boundaries(inputText, type="sentence")
  r1 <-lapply(text,function(x) Map(paste, str_extract_all(tolower(x),tolower(list2)), str_extract_all(tolower(x),tolower(list1)), MoreArgs = list(sep=":")))
  
  r1<-lapply(r1,function(x) unlist(x))
  #Unlist into a single row-This should output a character vector
  out<-lapply(r1,function(x) paste(x,collapse=","))
  
  return(out)
}

#' EntityPairs_TwoSentence
#'
#' This is used to look for relationships between site and event especially for endoscopy events
#' @keywords Find and replace
#' @param inputString The relevant pathology text column
#' @param list1 The intial list to assess
#' @param list2 The other list to look for
#' @importFrom stringr str_replace_na str_c str_split str_which
#' @importFrom purrr flatten_chr map_chr map map_if
#' @export
#' @examples # tbb<-EntityPairs_TwoSentence(Myendo,"Findings")

EntityPairs_TwoSentence<-function(inputString,list1,list2){
    
  #Prepare the text to be back into a tokenised version.
  #text<-textPrep(inputText)
  text<-standardisedTextOutput<-stri_split_boundaries(inputString, type="sentence")
  text<-lapply(text,function(x) tolower(x))
  
  
  #Some clean up to get rid of white space- all of this prob already covered in the ColumnCleanUp function but for investigation later
  text<-lapply(text,function(x) gsub("[[:punct:]]+"," ",x))
  #Prepare the list to use as a lookup:
  tofind <-tolower(list2)
  
  #Prepare the second list to use as a lookup
  EventList<-unique(tolower(unlist(list1,use.names = FALSE)))
  
  
  text<-sapply(text,function(x) {
    
    #Cleaning
    x<-trimws(x)
    
    #Prepare the text so that all empty text is replaced with NA and 
    #ready for processing
    try(words <-
          x %>%
          unlist() %>%
          str_replace_na()%>%
          str_c(collapse = ' ') %>%
          str_split(' ') %>%
          `[[`(1))
    
    
    
    words<-words[words != ""] 
    x1 <- str_extract_all(tolower(x),tolower(paste(unlist(list1), collapse="|")))
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
                flatten_chr()%>%
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

############################# Extrapolating Endoscopy ################################


#' Cleans medication column if present
#'
#' This cleans medication column from the report assuming such a column exists.
#' It gets rid of common entries that are not needed. It also splits the
#' medication into fentanyl and midazolam numeric doses for use. 
#' It should be used after the Extractor
#' @param dataframe dataframe with column of interest
#' @param MedColumn column of interest
#' @keywords Endoscopy medications
#' @return This returns a dataframe
#' @importFrom stringr str_extract str_replace
#' @export
#' @examples cc<-EndoscMeds(Myendo,'Medications')

EndoscMeds <- function(dataframe, MedColumn) {
  # Extraction of the Medications: Extract the fentanyl if present
  
  dataframe<-data.frame(dataframe,stringsAsFactors = FALSE)
  dataframe[,MedColumn]<-as.character(dataframe[,MedColumn])
  
  dataframe$Fent <-
    str_extract(dataframe[, MedColumn], "Fentanyl\\s*(\\d*(\\.\\d+)?)\\s*mcg")
  dataframe$Fent <- as.numeric(str_replace_all(dataframe$Fent,"Fentanyl|mcg", ""))
  
  # Extract the midazolam if present
  
  dataframe$Midaz <-
    str_extract(dataframe$Medications, "Midazolam\\s*(\\d*(\\.\\d+)?)\\s*mg")
  dataframe$Midaz <- as.numeric(str_replace_all(dataframe$Midaz,"Midazolam|mg", ""))
  
  
  # Extract the pethidine if present
  dataframe$Peth <-
    str_extract(dataframe[, MedColumn], "Pethidine\\s*(\\d*(\\.\\d+)?)\\s*mcg")
  dataframe$Peth <- as.numeric(str_replace_all(dataframe$Peth,"Pethidine|mcg", ""))
  
  
  # Extract the propofol if present
  dataframe$Prop <-
    str_extract(dataframe[, MedColumn], "Propofol\\s*(\\d*(\\.\\d+)?)\\s*mcg")
  dataframe$Prop <- as.numeric(str_replace_all(dataframe$Prop,"Propofol|mcg ", ""))
  
  return(dataframe)
}


#' EndoscopyEvent 
#'
#' This extracts the endoscopic event. It looks for the event term and then looks in the event sentence as well as the one above to see if
#' the location is listed. It needs to be run AFTER the HistolTypeAndSite function as emr needs to be
#' added to the event. Used in the OPCS4 coding
#' @keywords Find and replace
#' @param dataframe datafrane of interest
#' @param EventColumn1 The relevant endoscopt free text column describing the findings
#' @param Procedure Column saying which procedure was performed
#' @param Macroscopic Column describing all the macroscopic specimens
#' @param Histology Column with free text histology (usually microscopic histology)
#' @return This returns a character vector
#' @export
#' @examples # Myendo$EndoscopyEvent<-EndoscopyEvent(Myendo,"Findings",
#' #"ProcedurePerformed","MACROSCOPICALDESCRIPTION","HISTOLOGY")

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
}



############################# Extrapolating Histology ################################


#' Extract the number of biopsies taken from histology report
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
#' @examples
#' qq<-HistolNumbOfBx(Mypath$Macroscopicdescription,'specimen')


HistolNumbOfBx <- function(inputString, regString) {
  inputString <- DictionaryInPlaceReplace(inputString,WordsToNumbers())
    mylist <-
    #I need to collapse the unlist
    stringr::str_match_all(inputString, 
                           paste(unlist(lapply(strsplit(regString,"\\|",fixed=FALSE),
                                               function(x){paste("[0-9]{1,2}.{0,3}",x, sep = "")})),collapse="|"))
  NumbOfBx <-
    vapply(mylist, function(p)
      sum(as.numeric(stringr::str_replace_all(p,regString,""))),numeric(1))
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
#' @examples rr<-HistolBxSize(Mypath$Macroscopicdescription)

HistolBxSize <- function(MacroColumn) {
  # What's the average biopsy size this month?
  BxSize <- str_extract(MacroColumn, "the largest.*?mm")
  BxSize <- str_replace(BxSize,"the largest measuring |mm|less than", "")
  strBxSize <- "([0-9]+).*?([0-9])+.*?([0-9])"
  BxSize <-
    as.numeric(str_match(BxSize, strBxSize)[, 2]) *
    as.numeric(str_match(BxSize, strBxSize)[, 3]) *
    as.numeric(str_match(BxSize, strBxSize)[, 4])
  return(BxSize)
}




#' HistolTypeAndSite 
#'
#' This needs some blurb to be written. Used in the OPCS4 coding
#' @keywords Find and replace
#' @param inputString1 The first column to look in 
#' @param inputString2 The second column to look in 
#' @param procedureString The column with the procedure in it
#' @importFrom stringr str_remove_all str_replace_all
#' @export
#' @return a list with two columns, one is the type and site and the other
#' is the index to be used for OPCS4 coding later if needed.
#' @examples # SelfOGD_Dunn$PathSite<-HistolTypeAndSite(SelfOGD_Dunn$PROCEDUREPERFORMED,
#' SelfOGD_Dunn$MACROSCOPICALDESCRIPTION,SelfOGD_Dunn$HISTOLOGY)
#####To do: Perhaps create a BiopsySiteIndex as part of this function as it is an inevitable
#result of the function prior to OPCS4 coding.
HistolTypeAndSite<-function(inputString1,inputString2,procedureString){

  output<-ifelse(EntityPairs_OneSentence(inputString1,HistolType(),LocationList())=="NA:NA", 
                 EntityPairs_OneSentence(inputString2,HistolType(),LocationList()),
                 EntityPairs_OneSentence(inputString1,HistolType(),LocationList()))
  
  #If there is only a colon (punctuation) and then empty then assume it is a biopsy
  output<-str_replace_all(output, ":,|:$", ":biopsy,")
  
  #Make sure only unique values represented:
  output<-lapply(output, function(x) paste(unlist(unique(unlist(strsplit(x,",")))),collapse=","))
  
  output<-ifelse(grepl("Gastroscopy",procedureString),
                 str_remove_all(output, paste0('(',tolower( paste0(unlist(lower,use.names=F),collapse="|")),')',':biopsy')),
                 ifelse(grepl("Colonoscopy|Flexi",procedureString),
                        str_remove_all(output, paste0('(',tolower(paste0(unlist(upper,use.names=F),collapse="|")),')',':biopsy')),output))
  
  output<-unlist(output)
  
  biopsyIndexresults<-ExtrapolatefromDictionary(output,BiopsyIndex())
  
  output<-list(HistolTypeAndSite=output,BiopsyIndex=biopsyIndexresults)
  return(output)
}



#' HistolBiopsyIndex 
#'
#' This returns a number for all the biopsies taken based on distance from orifice. It is for biopsies only
#' YOU COULD DITCH THIS ONE AS IT IS JUCT A FUNCTION TO EXTRPOLATE FROM A DICTIONARY 
#' AS PER THE FUNCTION INSIDE THIS FUNCTION
#' @keywords Pathology biopsy index
#' @export
#' @param pathSiteString The column that has the pathology locatio and tissue type from HistolTypeAndSite
#' @importFrom stringr str_extract_all
#' @examples # SelfOGD_Dunn<-read_excel("/home/rstudio/GenDev/DevFiles/EndoMineRFunctionDev/SelfOGD_Dunn.xlsx")
#' SelfOGD_Dunn$PathSite<-HistolTypeAndSite(SelfOGD_Dunn,"MACROSCOPICALDESCRIPTION","HISTOLOGY")
#' HistolBiopsyIndex(SelfOGD_Dunn$PathSite) 
#Consider getting rid of this and integrating (as I've done but not tested) into the HistolType
#and Site function.

HistolBiopsyIndex<-function(pathSiteString){
  
 myresults<-ExtrapolatefromDictionary(pathSiteString,BiopsyIndex())
 return(myresults)
}




############################# Extrapolating Codes ################################



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
#' SelfOGD_Dunn$OPCS4w<-ExtrapolateOPCS4Prep(SelfOGD_Dunn,"PROCEDUREPERFORMED",
#' "PathSite","EndoscopyEvent")


#Take the PathSite codes which should have been coded from PathSite using the HistolBiopsyIndex

# #####################################################   Sandbox    ##################################################################################################################
# SelfOGD_Dunn<-read_excel("/home/rstudio/GenDev/DevFiles/EndoMineRFunctionDev/SelfOGD_Dunn.xlsx")
# EndoscTree<-list("Patient Name","Date of Birth","General Practicioner","Hospital Number","Date of Procedure","Endoscopist","Second endoscopist","Trainee","Referring Physician","Nurses","Medications","Instrument","Extent of Exam","Complications","Co-morbidity","INDICATIONS FOR EXAMINATION","PROCEDURE PERFORMED","FINDINGS","ENDOSCOPIC DIAGNOSIS","RECOMMENDATIONS","COMMENTS","FOLLOW UP","OPCS4 Code")
# 
# SelfOGD_Dunn<-SelfOGD_Dunn%>%select(PatientID,Endo_ResultText,Histo_ResultText)
# for(i in 1:(length(EndoscTree)-1)) {SelfOGD_Dunn<-Extractor2(SelfOGD_Dunn,'Endo_ResultText',as.character(EndoscTree[i]),as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))}
# PathTree<-c("NATURE OF SPECIMEN","CLINICAL DETAILS","MACROSCOPICAL DESCRIPTION","HISTOLOGY","DIAGNOSIS")
# for(i in 1:(length(PathTree)-1)) {SelfOGD_Dunn<-Extractor2(SelfOGD_Dunn,'Histo_ResultText',as.character(PathTree[i]),as.character(PathTree[i+1]),as.character(PathTree[i]))}
# 
# SelfOGD_Dunn$PathSite<-HistolTypeAndSite(SelfOGD_Dunn,"PROCEDUREPERFORMED","Histo_ResultText","MACROSCOPICALDESCRIPTION")
# SelfOGD_Dunn$PathSiteIndex<-HistolBiopsyIndex(SelfOGD_Dunn,"PathSite")
# 
# SelfOGD_Dunn$EndoscopyEvent<-EndoscopyEvent(SelfOGD_Dunn,"FINDINGS")
# 
# 
# #ForRules<-SelfOGD_Dunn%>%filter(grepl("Gastroscopy",PROCEDUREPERFORMED),grepl("OESOPHAGUS: Barrett's oesophagus C0 M1 ",FINDINGS))
# #EndoscopyEvent(ForRules,"FINDINGS")
# 
# #ForRules<-SelfOGD_Dunn%>%filter(grepl("Gastroscopy",PROCEDUREPERFORMED))%>%select(INDICATIONSFOREXAMINATION,ExtentofExam,Histo_ResultText,FINDINGS,EndoscopyEvent,PathSite,PathSiteIndex)
# #View(ForRules)
# SelfOGD_Dunn<-ExtrapolateOPCS4Prep(SelfOGD_Dunn,"PROCEDUREPERFORMED","PathSite","EndoscopyEvent")
# write_xlsx(SelfOGD_Dunn, "/home/rstudio/EndoscopyEventToValidate.xlsx")
# 
# 
# 
# #Checking against actual coding data
# ManualOPCS_4<-read_excel("/home/rstudio/GenDev/DevFiles/EndoMineRFunctionDev/TB_ALLPATID_Dunn_2013ToPresent.xls")
# library(janitor)
# selectedClean<-ManualOPCS_4%>%select("Prim Proc Code & Description","2nd Proc Code","Trust ID",
#                                      "Consultant","Admission Date","Prim Diag Code & Description",
#                                      "2nd Diagnosis Code",
#                                      "3rd Diagnosis Code",
#                                      "5th Diagnosis Code",
#                                      "6th Diagnosis Code",
#                                      "7th Diagnosis Code",
#                                      "8th Diagnosis Code",
#                                      "9th Diagnosis Code",
#                                      "10th Diagnosis Code")
# selectedClean<-clean_names(selectedClean,"snake")
# names(selectedClean) <- gsub('admission_date', 'Endo_ResultEntered', names(selectedClean))
# names(selectedClean) <- gsub('trust_id', 'PatientID', names(selectedClean))
# 
# #convert the date column names so can do a merge
# SelfOGD_Dunn$Endo_ResultEntered<-as.Date(SelfOGD_Dunn$Endo_ResultEntered)
# selectedClean$Endo_ResultEntered<-as.Date(selectedClean$Endo_ResultEntered)
# 
# mergedData <- merge(selectedClean,SelfOGD_Dunn,by=c("Endo_ResultEntered","PatientID"))
# 
# 
# ForRules<-SelfOGD_Dunn%>%filter(grepl("Gastroscopy",PROCEDUREPERFORMED))%>%select(ExtentofExam,Histo_ResultText,FINDINGS,EndoscopyEvent,prim_proc_code_description,x2nd_proc_code,PathSite,PathSiteIndex)%>%sample_n(100)
# View(ForRules)
#ToSee<-ForRules%>%select(FINDINGS,EndoscopyEvent,PathSite)%>% filter(grepl("Error", EndoscopyEvent))

######################################################################################################################################################################################################
#For each event site:

ExtrapolateOPCS4Prep <- function(dataframe, Procedure,PathSite,Event) {  
  dataframe<-data.frame(dataframe,stringsAsFactors=FALSE)
  
  
  
  #For the primary codes:
  dataframe$EndoscopyEvent<-gsub("(Oesophagus|GOJ):apc","G143  -  Fibreoptic Endoscopic Cauterisation of Lesion of Oesophagus",dataframe$EndoscopyEvent,ignore.case = TRUE)
  dataframe$EndoscopyEvent<-gsub("(Oesophagus|GOJ):emr","G146  -  Fibreoptic endoscopic submucosal resection of lesion of oesophagus",dataframe$EndoscopyEvent,ignore.case = TRUE)  
  dataframe$EndoscopyEvent<-gsub("(Oesophagus|GOJ):polypectomy","G141  -  Fibreoptic endoscopic snare resection of lesion of oesophagus",dataframe$EndoscopyEvent,ignore.case = TRUE) 
  dataframe$EndoscopyEvent<-gsub("(Oesophagus|GOJ):rfa","G145  -  Fibreoptic endoscopic destruction of lesion of oesophagus NEC",dataframe$EndoscopyEvent,ignore.case = TRUE)
  dataframe$EndoscopyEvent<-gsub("(Oesophagus|GOJ):esd","G146  -  Fibreoptic endoscopic submucosal resection of lesion of oesophagus",dataframe$EndoscopyEvent,ignore.case = TRUE)
  dataframe$EndoscopyEvent<-gsub("(Oesophagus|GOJ):dilat", "G152  -  Fibreoptic Endoscopic Balloon Dilation of Oesophagus",dataframe$EndoscopyEvent,ignore.case = TRUE)
  dataframe$EndoscopyEvent<-gsub("APC","G432  -  Fibreoptic endoscopic laser destruction of lesion of upper gastrointestinal tract",dataframe$EndoscopyEvent,ignore.case = TRUE) 
  dataframe$EndoscopyEvent<-gsub("EMR","G423  -  Fibreoptic endoscopic mucosal resection of lesion of upper gastrointestinal tract",dataframe$EndoscopyEvent,ignore.case = TRUE)  
  dataframe$EndoscopyEvent<-gsub("Polypectomy","G431  -  Fibreoptic endoscopic snare resection of lesion of upper gastrointestinal tract",dataframe$EndoscopyEvent,ignore.case= TRUE)
  dataframe$EndoscopyEvent<-gsub("RFA","G435  -  Fibreoptic endoscopic destruction of lesion of upper gastrointestinal tract NEC",dataframe$EndoscopyEvent,ignore.case = TRUE)
  dataframe$EndoscopyEvent<-gsub("ESD","G421  -  Fibreoptic endoscopic submucosal resection of lesion of upper gastrointestinal tract",dataframe$EndoscopyEvent,ignore.case = TRUE)
  dataframe$EndoscopyEvent<-gsub("Dilatation","G443  -  Fibreoptic endoscopic dilation of upper gastrointestinal tract NEC",dataframe$EndoscopyEvent,ignore.case = TRUE)
  
  
  #For the non-event entries:
  
  dataframe<-dataframe %>%   
    mutate(OPCS4Primary = case_when(
      grepl("OGD", dataframe$PROCEDUREPERFORMED,ignore.case = TRUE) ~  case_when(
        
        #No event and no biopsy taken:
        dataframe$EndoscopyEvent==""&(dataframe$PathSite==""|dataframe$PathSite=="NA:NA") ~ "G459  -  Unspecified diagnostic fibreoptic endoscopic examination of upper gastrointestinal tract",
        
        #No event and upper GI biopsy taken:
        dataframe$EndoscopyEvent==""& grepl("O",dataframe$PathSite,ignore.case = TRUE) ~ "G451  -  Fibreoptic endoscopic exam of upper gastrointestinal tract and biopsy of lesion of upper GI tract",
        
        #Event (oesophageal) and upper GI biopsy taken
        grepl("oesophagus",dataframe$EndoscopyEvent,ignore.case = TRUE) & grepl("O",dataframe$PathSite,ignore.case = TRUE) ~ "G161  -  Diagnostic fibreoptic endoscopic examination of oesophagus and biopsy of lesion of oesophagus"
        
      ),
      TRUE ~ "SomethingElse"
    ))
  
  
  #For the secondary codes:
  dataframe$MAXOFPATHSITE<-str_extract_all(dataframe$PathSite,"\\d")
  dataframe$MAXOFPATHSITE<-lapply(dataframe$MAXOFPATHSITE,function(x) max(as.numeric(x)))
  
  dataframe<-dataframe %>%   
    mutate(OPCS4ZCode = case_when( 
      dataframe$PathSite==""~ case_when(
        #1. if no biopsy and no Event (covers oesophageal and non-oesophageal), then give the extent reached
        tolower(dataframe$ExtentofExam)=="second part of duodenum"~  "Z27.4",
        tolower(dataframe$ExtentofExam)=="pylorus"~  "Z27.3",
        tolower(dataframe$ExtentofExam)=="stomach"~  "Z27.2",
        tolower(dataframe$ExtentofExam)=="oesophagus"~  "Z27.1",
      ),
      
      #2.If event (oesophageal) and biopsy 
      dataframe$PathSite!="" ~ case_when(
        dataframe$MAXOFPATHSITE== 5 ~  "Z27.4",
        dataframe$MAXOFPATHSITE== 4 ~  "Z27.3",
        dataframe$MAXOFPATHSITE== 3 ~  "Z27.2",
        dataframe$MAXOFPATHSITE== 1|2 ~  "Z27.1",
      ),
      TRUE ~ "No code here"
    )
    )
  return(dataframe)
}


#' Primary Diagnosis from Endoscopy
#'
#' This function extracts the primary diagnosis from the endoscopy free text. It will also add
#' pathology diagnosis to the final primary code
#' 
#'
#' @param dataframe the dataframe
#' @param Procedure the Procedure column
#' @param PathSite the PathSite column
#' @param FINDINGS the FINDINGS column
#' @param ENDOSCOPICDIAGNOSIS the ENDOSCOPICDIAGNOSIS column
#' @param EndoscopyEvent the EndoscopyEvent column
#' @keywords OPCS-4 codes extraction
#' @importFrom dplyr mutate case_when
#' @importFrom rlang sym
#' @importFrom stringr str_detect
#' @importFrom stringi stri_split_lines
#' @export
#' @examples # Need to run the HistolTypeSite and EndoscopyEvent functions first here
#' SelfOGD_Dunn$OPCS4w<-ExtrapolateOPCS4Prep(SelfOGD_Dunn,"PROCEDUREPERFORMED",
#' "PathSite","EndoscopyEvent")



#The plan: 
#1. Look at th ENDOSCOPICDIAGNOSIS column first and implement algorithm to look for keywords that map to diagnostic codes
#2. Need to establish an order of importance of diagnostic codes.
#3. Map all the diagnoses from the drop down box to primary diagnosis codes
ExtrapolateEndoscopicICD10 <- function(dataframe, Procedure,PathSite,FINDINGS,ENDOSCOPICDIAGNOSIS,EndoscopyEvent) { 
  
  
  #Just get the uppers for now:
  dataframe<-SelfOGD_Dunn[grepl("Gastroscopy",dataframe$Procedure),]
  
  #Merge the findings in with the myDx:
  dataframe$FINDINGSmyDx<-paste(dataframe$FINDINGS,dataframe$ENDOSCOPICDIAGNOSIS)
  
  #Split into a string
  dataframe$FINDINGSmyDx<-stri_split_lines(dataframe$FINDINGSmyDx,omit_empty = TRUE)
  
  #Copy over to a new column to be sampled
  dataframe$FindingsAfterProcessing<-dataframe$FINDINGSmyDx
  
  
  #Use case when on nested list to generate ICD-10 codes and then remove from the list.
  
  #If it matches the grep then add the diagnosis, and then chop into list and remove that string
  
  
  
  
  
  dataframe<-dataframe %>%
    mutate(
      PrimaryDiagnosisCode = map(
        FINDINGSmyDx, ~ case_when(
          grepl("mitotic|emr|tumour", unlist(tolower(.x)),ignore.case=TRUE) ~ "C159  -  Malignant neoplasm oesophagus, unspecified - Oesophagus - unspecified",
          grepl("dysplasia", unlist(tolower(.x)),ignore.case=TRUE) ~ "K229  -  Disease of oesophagus - unspecified",
          grepl("stricture", unlist(tolower(.x)),ignore.case=TRUE) ~  "K222  -  Oesophageal obstruction",
          grepl("barrett", unlist(tolower(.x)),ignore.case=TRUE) ~  "K227  -  Barrett's oesophagus",
          grepl("(\\.|^)(?=[^\\.]*inlet)(?=[^\\.]*patch)[^\\.]*(\\.|$)", unlist(tolower(.x)),ignore.case=TRUE,perl=TRUE) ~  "K228  -  Other specified diseases of oesophagus",
          grepl("hiatus",tolower(.x), unlist(tolower(.x)),ignore.case=TRUE,perl=TRUE) ~  "K449  -  Diaphragmatic hernia without obstruction or gangrene",
          grepl("oesophagitis",tolower(.x), unlist(tolower(.x)),ignore.case=TRUE,perl=TRUE) ~  "K20  -  Oesophagitis",
          grepl("(?=[^\\.]*(duodenal))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)", unlist(tolower(.x)),ignore.case=TRUE,perl=TRUE) ~  "K26  -  Duodenal ulcer",
          grepl("(?=[^\\.]*(oesophageal))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)", unlist(tolower(.x)),ignore.case=TRUE,perl=TRUE) ~  "K221  -  Oesophageal ulcer",
          grepl("(?=[^\\.]*(gastric|stomach|pylor))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)", unlist(tolower(.x)),ignore.case=TRUE,perl=TRUE) ~  "K25  -  Gastric ulcer",
          TRUE ~ "")
      )
    )
  
  dataframe$FINDINGSmyDx<-lapply(dataframe$FINDINGSmyDx, function(x) x[!grepl("OESOPH.*","", x,ignore.case=TRUE,perl=TRUE)])
  
  dataframe<-dataframe %>%  FindingsAfterProcessing = map(
    FindingsAfterProcessing, ~ .x[!grepl("(mitotic|emr)", tolower(.x),ignore.case=TRUE,perl=TRUE)]
  )
  
  
  
  
  ######### Go through it again for the second codes:
  
  dataframe<-dataframe %>%
    mutate(
      OverallDiagnosisCode = map(
        FindingsAfterProcessing, ~ case_when(
          grepl("mitotic|emr|tumour", tolower(.x)) ~ "C159  -  Malignant neoplasm oesophagus, unspecified - Oesophagus - unspecified",
          grepl("dysplasia", tolower(.x)) ~ "K229  -  Disease of oesophagus - unspecified",
          grepl("stricture",tolower(.x),ignore.case=TRUE) ~  "K222  -  Oesophageal obstruction",
          grepl("barrett",tolower(.x),ignore.case=TRUE) ~  "K227  -  Barrett's oesophagus",
          grepl("(\\.|^)(?=[^\\.]*inlet)(?=[^\\.]*patch)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K228  -  Other specified diseases of oesophagus",
          grepl("hiatus",tolower(.x),ignore.case=TRUE) ~  "K449  -  Diaphragmatic hernia without obstruction or gangrene",
          grepl("oesophagitis",tolower(.x),ignore.case=TRUE) ~  "K20  -  Oesophagitis",
          grepl("(?=[^\\.]*(duodenal))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K26  -  Duodenal ulcer",
          grepl("(?=[^\\.]*(oesophageal|oesophagus))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K221  -  Oesophageal ulcer",
          grepl("(?=[^\\.]*(gastric|stomach|pylor))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K25  -  Gastric ulcer",
          grepl("gastritis",tolower(.x),ignore.case=TRUE) ~  "K297  -  Gastritis - unspecified",
          grepl("duodenitis",tolower(.x),ignore.case=TRUE) ~  "K298  -  Duodenitis",
          grepl("(?=[^\\.]*(gastric))(?=[^\\.]*polyp)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K317  -  Polyp of stomach and duodenum",
          grepl("candid",tolower(.x),ignore.case=TRUE) ~  "B387  -  Candidiasis of other sites",
          TRUE ~ "")
      ),
      FindingsAfterProcessing = map(
        FindingsAfterProcessing, ~ .x[!grepl("(mitotic|emr|tumour|dysplasia|hiatus|stricture|barrett|inlet patch|
                                             hiatus|esophagitis|duodenitis|gastritis)|(?:(?=[^\\.]*(duodenum|oesophagus|gastric|stomach|pylor))(?=[^\\.]*ulcer)[^\\.]*(\\.|$))|
                                             (?:(?=[^\\.]*(stomach))(?=[^\\.]*polyp)[^\\.]*(\\.|$))|(candid)", tolower(.x),ignore.case=TRUE,perl=TRUE)]
      )
        )
  
  
  dataframe$OverallDiagnosisCode<-lapply(dataframe$OverallDiagnosisCode,function(x) (unique(x)))
  dataframe$OverallDiagnosisCode<-lapply(dataframe$OverallDiagnosisCode, function(x) x[x != "" & x != "\n"])
  
  
  #Now go through the OverallCodes and determine the 
  
  
  ######### Go through it again for the third codes: 
  
  dataframe<-dataframe %>%
    mutate(
      ThirdDiagnosisCode = map(
        FindingsAfterProcessing, ~ case_when(
          grepl("mitotic|emr|tumour", tolower(.x)) ~ "C159  -  Malignant neoplasm oesophagus, unspecified - Oesophagus - unspecified",
          grepl("dysplasia", tolower(.x)) ~ "K229  -  Disease of oesophagus - unspecified",
          grepl("stricture",tolower(.x),ignore.case=TRUE) ~  "K222  -  Oesophageal obstruction",
          grepl("barrett",tolower(.x),ignore.case=TRUE) ~  "K227  -  Barrett's oesophagus",
          grepl("(\\.|^)(?=[^\\.]*inlet)(?=[^\\.]*patch)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K228  -  Other specified diseases of oesophagus",
          grepl("hiatus",tolower(.x),ignore.case=TRUE) ~  "K449  -  Diaphragmatic hernia without obstruction or gangrene",
          grepl("oesophagitis",tolower(.x),ignore.case=TRUE) ~  "K20  -  Oesophagitis",
          grepl("(?=[^\\.]*(duodenal))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K26  -  Duodenal ulcer",
          grepl("(?=[^\\.]*(oesophageal|oesophagus))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K221  -  Oesophageal ulcer",
          grepl("(?=[^\\.]*(gastric|stomach|pylor))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K25  -  Gastric ulcer",
          grepl("gastritis",tolower(.x),ignore.case=TRUE) ~  "K297  -  Gastritis - unspecified",
          grepl("duodenitis",tolower(.x),ignore.case=TRUE) ~  "K298  -  Duodenitis",
          grepl("(?=[^\\.]*(gastric))(?=[^\\.]*polyp)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K317  -  Polyp of stomach and duodenum",
          grepl("candid",tolower(.x),ignore.case=TRUE) ~  "B387  -  Candidiasis of other sites",
          TRUE ~ "")
      ),
      FindingsAfterProcessing = map(
        FindingsAfterProcessing, ~ .x[!grepl("(mitotic|emr|tumour|dysplasia|hiatus|stricture|barrett|inlet patch|
                                             hiatus|esophagitis|duodenitis|gastritis)|(?:(?=[^\\.]*(duodenum|oesophagus|gastric|stomach|pylor))(?=[^\\.]*ulcer)[^\\.]*(\\.|$))|
                                             (?:(?=[^\\.]*(gastric))(?=[^\\.]*polyp)[^\\.]*(\\.|$))|(candid)", tolower(.x),ignore.case=TRUE,perl=TRUE)]
      )
    )
  
  #To Do: Also assess the findings column
  
  
  
  return(primDxVector)
}



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



