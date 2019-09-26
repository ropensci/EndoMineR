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
      "feats",
      "udmodel_english",
      "cbind_dependencies",
      "head_token_id",
      "deps",
      "morph_voice",
      "OriginalReport",
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
      "lower",
      "upper",
      "pathSiteString",
      "outputFinal",
      "theme_foundation",
      "udmodel",
      "x3",
      "FINDINGSmyDx",
      "FindingsAfterProcessing",
      "primDxVector",
      "Temporal",
      "sentence_id",
      "img",
      "Endo_ResultEntered",
      "V1",
      "pandoc.image.return"
    )
  )











##########Text preparation##########



#' textPrep function
#'
#' This function prepares the data by cleaning 
#' punctuation, checking spelling against the lexicons, mapping terms
#' according to the lexicons and lower casing everything. 
#' It contains several of the other functions
#' in the package for ease of use. 
#' @keywords text cleaning
#' @param inputText The relevant pathology text column
#' @param delim the delimitors so the extractor can be used
#' @importFrom stringi stri_split_boundaries
#' @export
#' @return This returns a string vector.
#' @examples mywords<-c("Hospital Number","Patient Name:","DOB:","General Practitioner:",
#' "Date received:","Clinical Details:","Macroscopic description:",
#' "Histology:","Diagnosis:")
#' CleanResults<-textPrep(PathDataFrameFinal$PathReportWhole,mywords)



#Need to make sure the sentences are separated in the Extractor column by a separator such as carriage return
#So that a tokenizer can be used for NegEx or any other function.
#Also need to get rid of ASCII \\X10 etc in the Column Cleanup.


textPrep<-function(inputText,delim){
  
  #1. Flatten the text..
  inputText<-tolower(inputText)
  
  #1b General cleanup tasks tokenize then clean then recombine
  standardisedTextOutput<-stri_split_boundaries(inputText, type="sentence")
  standardisedTextOutput<-lapply(standardisedTextOutput, function(x) ColumnCleanUp(x))
  inputText<-lapply(standardisedTextOutput, function(x) paste0(unlist(x),collapse="\n"))
  
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
  

  #4. Need to map the terms to the lexicons to make sure everything standardised.
  inputText<-DictionaryInPlaceReplace(inputText,LocationList())
  inputText<-DictionaryInPlaceReplace(inputText,EventList())
  inputText<-DictionaryInPlaceReplace(inputText,HistolType())
  
  #returns a lower case version
  inputText<-tolower(inputText)
  #Will also need to add the Extractor output to the dataframe.
  MyCompleteFrame<-Extractor(as.character(inputText),tolower(delim))
  #Last minute clean up:
  names(MyCompleteFrame) <- gsub(".", "", names(MyCompleteFrame), fixed = TRUE)
  
  return(MyCompleteFrame)
}




#' Extracts the columns from the raw report
#'
#' This is the main extractor for the Endoscopy and Histology report.
#' This relies on the user creating a list of words representing the
#' subheadings. The list is then fed to the
#' Extractor so that it acts as the beginning and the end of the
#' regex used to split the text. Whatever has been specified in the list
#' is used as a column header. Column headers don't tolerate special characters
#' like : or ? and / and don't allow numbers as the start character so these
#' have to be dealt with in the text before processing
#'
#' @param inputString the column to extract from
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
#' Mypath2<-Extractor(PathDataFrameFinal$PathReportWhole,mywords)


Extractor <- function(inputString, delim) {

#Create a named list of words
delim <- gsub(":","",delim)
names(delim) <- trimws(delim)
#Add a : to the tags 

delim <- gsub("(.*)","\\1: ",delim)
delim<-as.list(delim)
inputString<-gsub(":","",inputString)
#Do the find and replace to place the tags in the input text
inputString<-EndoMineR::DictionaryInPlaceReplace(inputString,delim)

#Do a bit more cleaning to make it into a dcf file:
inputString<-gsub(": :",": ",inputString)
inputString<-gsub(":([A-Za-z0-9])",": \\1",inputString)
inputString<-gsub("(","",inputString,fixed=TRUE)
#Don't remove newlines as these are used as the sentence separators
inputString<-gsub("\n",". ",inputString,fixed=TRUE)
inputString<-gsub(")","",inputString,fixed=TRUE)
inputString<-gsub("'","",inputString,fixed=TRUE)
inputString<-gsub("^","Start:",inputString)
inputString<-gsub("::",":",inputString,fixed=TRUE)

#Create the dcf file
pat <- sprintf("(%s)", paste(delim, collapse = "|"))
g <- gsub(pat, "\n\\1", paste0(inputString, "\n"))
m <- read.dcf(textConnection(g))
m<-data.frame(m,stringsAsFactors = FALSE)
return(m)
}


#' Dictionary In Place Replace
#'
#' This maps terms in the text and replaces them with the 
#' standardised term (mapped in the lexicon file) within the text.
#' It is used within the textPrep function.

#'
#' @param inputString the input string (ie the full medical report)
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
#' Extraction of the negative sentences so that normal findings can be
#' removed and not counted when searching for true diseases. eg remove
#' 'No evidence of candidal infection' so it doesn't get included if
#' looking for candidal infections. It is used by default as part of
#' the textPrep function but can be turned off as an optional parameter
#' @param inputText column of interest
#' @keywords Negative Sentences
#' @importFrom stringr str_replace
#' @export
#' @return This returns a column within a dataframe. THis should be changed to a 
#' character vector eventually
#' @examples # Build a character vector and then incorporate into a dataframe
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
  
  # Time related phrases eg post and previous
  inputText <- gsub(" (post|previous|prior)[^a-z].+?[A-Za-z]{3}",
                    " TIME_REPLACED-",
                    inputText,
                    perl = TRUE,
                    ignore.case = TRUE)
  
  
  return(inputText)
}

#' Find and Replace function
#'
#' This is a helper function for finding and replacing from lexicons
#' like the event list. The lexicons are all named lists where the name
#' is the text to replace and the value what it should be replaced with
#' It uses fuzzy find and replace to account for spelling errors
#' @keywords Find and replace
#' @importFrom utils aregexec
#' @param pattern the pattern to look for
#' @param replacement the pattern replaceme with
#' @param x the target string
#' @return This returns a character vector
#' @examples L <- tolower(stringr::str_split(HistolType(),"\\|"))
#' inputText<-TheOGDReportFinal$OGDReportWhole
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
  vector<-gsub("\r"," ",vector)
  vector<-gsub("\\.,","\\.",vector)
  vector<-gsub(",([A-Z])","\\.\\1",vector)
  vector<-gsub("\\. ,",".",vector)
  vector<-gsub("\\.\\s+\\,"," ",vector)
  vector<-gsub("^\\s+\\,"," ",vector)
  #Get rid of ASCCII hex here

  vector<-gsub("\\\\[Xx].*?\\\\", " ", vector)
  vector<-gsub("       ", " ", vector)

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
  standardisedTextOutput<-lapply(standardisedTextOutput,function(x) gsub("(Dr.*?[A-Za-z]+)|([Rr]eported.*)|([Dd]ictated by.*)"," ",x))
  
  #Get rid of strange things
  standardisedTextOutput<-lapply(standardisedTextOutput,function(x) gsub("\\.\\s+\\,","\\.",x))
  standardisedTextOutput<-lapply(standardisedTextOutput,function(x) gsub("^\\s+\\,"," ",x))
  retVector<-sapply(standardisedTextOutput, function(x) paste(x,collapse="."))
  return(retVector)
}


#' Paste into one
#'
#' As spreadsheets are likely to be submitted with pre-segregated data as appears from 
#' endoscopy software output, these should be remerged prior to cleaning. This function
#' takes the column headers and places it before each text so that the original
#' full text is recreated. It will use the column headers as the delimiter. This should 
#' be used before textPrep as the textPrep function takes a character vector (ie the whole
#' report and not a segregated one) only
#' @keywords Merge dataframe columns into one text
#' @param x the dataframe
#' @export
#' @return This returns a list with a dataframe containingone column of the merged text
#' and a character vector which is the delimiter list for when the textPrep function is used
#' @examples testList<-structure(list(PatientName = c("Tom Hardy", "Elma Fudd", "Bingo Man"
#' ), HospitalNumber = c("H55435", "Y3425345", "Z343424"), Text = c("All bad. Not good", 
#' "Serious issues", "from a land far away")), class = "data.frame", row.names = c(NA, -3L))
#' EndoPaste(testList)


EndoPaste<-function(x){
  names(x)<-ColumnCleanUp(names(x))
  names(x)<-gsub("\n+"," ",names(x))
  delim<-paste(names(x))
  v1<-do.call(paste, c(Map(paste, names(x), x), " ",sep="\n"))
  df<-data.frame(X1_X2_X3 = unname(v1))
  return(list(df,delim))
}



############## Extraction Utilities - Basic ###################

#' Extrapolate from Dictionary
#'
#' Provides term mapping and extraction in one.
#' Standardises any term according to a mapping lexicon provided and then
#' extracts the term. This is
#' different to the DictionaryInPlaceReplace in that it provides a new column
#' with the extracted terms as opposed to changing it in place
#'
#' @param inputString The text string to process
#' @param list of words to iterate through
#' @keywords Withdrawal
#' @importFrom fuzzyjoin regex_left_join
#' @importFrom dplyr as_tibble pull
#' @export
#' @examples #Firstly we extract histology from the raw report
#' # The function then standardises the histology terms through a series of
#' # regular expressions and then extracts the type of tissue 

#' Mypath$Tissue<-suppressWarnings(
#' suppressMessages(
#' ExtrapolatefromDictionary(Mypath$Histology,HistolType()
#' )
#' )
#' )
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
  
  #Create a dataframe
  df1<-data.frame(key = replace, val = replaceValue)
  
  #Create a tibble to merge with the list
  d1 <- as_tibble(df1)
  
  
  #Select the elements that have characters in them
  i1 <- lengths(ToIndex) > 0 
  
  #Do the merge. This takes the key to lookup and then if found, replaces
  #the value with value associated with it in the table. I think the pull
  #function also creates a new column with the value in it. This is an important
  #function as uses a table lookup
  ToIndex[i1] <- map(ToIndex[i1], ~ 
                       tibble::tibble(key = .x) %>%
                       fuzzyjoin::regex_left_join(d1) %>%
                       pull(val))
  
  #This unlists the nested list
  ToIndex<-lapply(ToIndex, function(x) unlist(x,recursive=F))
  
  #This collapses the list so that the output is a single string
  ToIndex<-unlist(lapply(ToIndex, function(x) paste(x,collapse=";")))
  
  return(ToIndex)
}

############## Extraction Utilities- Colocation ###################

#' EntityPairs_OneSentence 
#'
#' Use to see if words from two lists co-exist within a sentence. Eg site and tissue type.
#' This function only looks in one sentence for the two terms. If you suspect the terms may
#' occur in adjacent sentences then use the EntityPairs_TwoSentence function.
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
  r1 <-lapply(text,function(x) Map(paste, str_extract_all(tolower(x),tolower(list2)), 
                                   str_extract_all(tolower(x),tolower(list1)), MoreArgs = list(sep=":")))
  
  r1<-lapply(r1,function(x) unlist(x))
  #Unlist into a single row-This should output a character vector
  out<-lapply(r1,function(x) paste(x,collapse=","))
  
  return(out)
}

#' EntityPairs_TwoSentence
#'
#' This is used to look for relationships between site and event especially for endoscopy events
#' where sentences such as 'The stomach polyp was large. It was removed with a snare' ie the therapy
#' and the site are in two different locations.
#' @keywords Find and replace
#' @param inputString The relevant pathology text column
#' @param list1 The intial list to assess
#' @param list2 The other list to look for
#' @importFrom stringr str_replace_na str_c str_split str_which str_extract_all regex str_subset
#' @importFrom stringi stri_split_boundaries
#' @importFrom rlang is_empty
#' @importFrom purrr flatten_chr map_chr map map_if
#' @export
#' @examples # tbb<-EntityPairs_TwoSentence(Myendo$Findings,EventList(),HistolType())

EntityPairs_TwoSentence<-function(inputString,list1,list2){
  
  #Prepare the text to be back into a tokenised version.
  #text<-textPrep(inputText)
  text<-standardisedTextOutput<-stri_split_boundaries(inputString, type="sentence")
  text<-lapply(text,function(x) tolower(x))
  
  
  #Some clean up to get rid of white space- all of this prob already covered in the ColumnCleanUp function but for investigation later
  text<-lapply(text,function(x) gsub("[[:punct:]]+"," ",x))
  #Prepare the list to use as a lookup:
  tofind <-paste(tolower(list2),collapse="|")
  
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
                map_if(rlang::is_empty, ~ NA_character_) %>%
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

#' MyImgLibrary
#'
#' This is used to pick and clean endoscopic images from html exports so they can be prepared
#' before being linked to pathology and endoscopy reports
#' @keywords Image extraction
#' @param file The html report to extract (the html will have all the images references in it)
#' @param delim The phrase that separates individual endoscopies
#' @param location The folder containing the actual images
#' @importFrom stringr str_extract 
#' @importFrom lubridate parse_date_time
#' @importFrom tidyr separate_rows
#' @importFrom pander pandoc.image.return
#' @importFrom data.table as.data.table
#' @export
#' @examples # MyImgLibrary("S:/Gastroenterology/Seb/R/Data/Barretts/Images Captured with Proc Data Audit_Findings1.html",
#' #                         "procedureperformed","S:/Gastroenterology/Seb/R/Data/Barretts/")

MyImgLibrary<-function(file,delim,location){
  
  #Get the relative path from the www folder location which is where all files should be stored 
  location<-gsub(".*\\/","",location)
  htmlCode = readLines(file)
  
  #Collapse all the html together
  mergedhtml<-paste(htmlCode, sep="", collapse="")
  
  #Split according to Procedure Performed which separates each record
  df<-strsplit(mergedhtml, delim, fixed = FALSE, perl = FALSE, useBytes = FALSE)
  df<-as.data.frame(df)
  colnames(df)<-c("df")
  
  
  #Extract and format dates properly
  df$Endo_ResultEntered<-str_extract(df$df,"(\\d{4}[[:punct:]]\\d{2}[^:alnum:]\\d{2})|(\\d{2}[^:alnum:]\\d{2}[^:alnum:]\\d{4})")
  #Get them all as dates:
  df$Endo_ResultEntered<-parse_date_time(df$Endo_ResultEntered, orders = c("dmy", "ymd"))
  
  #Extract the patient ID:
  df$PatientID<-str_extract(df$df,"[A-Z0-9]{1,}[0-9]{3,}[A-Z0-9]{1}")
  
  #Extract the images with the folder name which needs to be kept (but is relative in html so no need to strip it off)
  df$img<-stringr::str_extract(df$df,"img src.*?(jpg|png|gif|bmp)")
  
  #Need to replace the with the current path here
  #df$img<-gsub("img src=\"Images Captured with Proc Data Audit.files/","",df$img)
  df$df<-NULL
  
  #Now collapse the table so all image files for a procedure in one row only:
  mergeddf<-as.data.frame(as.data.table(df)[, toString(img), by = list(Endo_ResultEntered,PatientID)])
  
  #Split the comma separated img list into a list within the data frame so you should then be able to iterate over it:
  mergeddf<-separate_rows(mergeddf,V1,sep=",")
  mergeddf$V1<-gsub("img src=\"","",mergeddf$V1)
  mergeddf$V1<-trimws(mergeddf$V1)
  mergeddf$img<-str_extract(mergeddf$V1,"[A-Za-z0-9]+[.][a-z]+$")
  mergeddf$url<-lapply(mergeddf$img,function(x) paste0("<img src=",location,"/",x,"'>"))
  mergeddf$base64<-lapply(mergeddf$img,function(x) paste0(location,"/",x)) 
  mergeddf$V1<-NULL
  mergeddf$url<-gsub("=","=\'",mergeddf$url)
  
  #For pandoc
  mergeddf$url<-sapply(mergeddf$url,pandoc.image.return)
  return(mergeddf)
}





