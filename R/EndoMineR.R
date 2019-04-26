

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      ".SD",
      "b",
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
      "origin",
      "FreqYear",
      "Original.y"
    )
  )
}

######## Surveillance functions ######

#' Extract the time difference between each test in days
#'
#' This determines the time difference between each test for a patient in days
#' It returns the time since the first and the last study as a new dataframe.
#'
#' @param dataframe dataframe,
#' @param HospNum_Id Patient ID
#' @param Endo_ResultPerformed Date of the Endoscopy
#' @importFrom dplyr arrange group_by mutate lead
#' @importFrom magrittr '%>%'
#' @importFrom rlang sym
#' @keywords Surveillance
#' @export
#' @examples
#' aa <- SurveilTimeByRow(
#'   Myendo, "HospitalNumber",
#'   "Dateofprocedure"
#' )
SurveilTimeByRow <-
  function(dataframe, HospNum_Id, Endo_ResultPerformed) {
    HospNum_Ida <- rlang::sym(HospNum_Id)
    Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
    ret <- dataframe %>%
      arrange(!!HospNum_Ida, !!Endo_ResultPerformeda) %>%
      group_by(!!HospNum_Ida) %>%
      mutate(TimeToNext = difftime(as.Date(!!Endo_ResultPerformeda), lead(as.Date(
        !!Endo_ResultPerformeda
      ), 1), units = "days")) %>%
      mutate(TimeSinceLast = difftime(Sys.Date(), dplyr::last(!!Endo_ResultPerformeda),
        units = "days"
      ))
    dataframe <- data.frame(ret)
    return(dataframe)
  }



#' Extract the last test done by a patient only
#'
#' This extracts the last test only per patient and returns a new dataframe listing the
#' patientID and the last test done
#' @param dataframe dataframe
#' @param HospNum_Id Patient ID
#' @param Endo_ResultPerformed Date of the Endoscopy
#' @importFrom magrittr '%>%'
#' @importFrom dplyr arrange group_by filter row_number
#' @importFrom rlang sym
#' @keywords Surveillance
#' @export
#' @examples
#' cc <- SurveilLastTest(Myendo, "HospitalNumber", "Dateofprocedure")
SurveilLastTest <-
  function(dataframe, HospNum_Id, Endo_ResultPerformed) {
    HospNum_Ida <- rlang::sym(HospNum_Id)
    Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)
    ret <- dataframe %>%
      group_by(!!HospNum_Ida) %>%
      arrange(!!Endo_ResultPerformeda) %>%
      filter(row_number() == dplyr::n())
    dataframe <- data.frame(ret)
    return(dataframe)
  }


#' Extracts the first test only per patient
#'
#' Extracts the first test only per patient and returns a new dataframe listing the
#' patientID and the first test done
#' @param dataframe dataframe
#' @param HospNum_Id Patient ID
#' @param Endo_ResultPerformed Date of the Endoscopy
#' @importFrom dplyr arrange group_by filter
#' @importFrom magrittr '%>%'
#' @importFrom rlang sym
#' @keywords Surveillance
#' @export
#' @examples
#' dd <- SurveilFirstTest(
#'   Myendo, "HospitalNumber",
#'   "Dateofprocedure"
#' )
SurveilFirstTest <-
  function(dataframe, HospNum_Id, Endo_ResultPerformed) {
    HospNum_Ida <- rlang::sym(HospNum_Id)
    Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)
    ret <- dataframe %>%
      group_by(!!HospNum_Ida) %>%
      arrange(!!Endo_ResultPerformeda) %>%
      filter(row_number() == 1)
    dataframe <- data.frame(ret)
    return(dataframe)
  }


#' Last status
#'
#' This function selects patients who have had a start event and an end
#' event of the users choosing so you can determine things like how long
#' it takes to get a certain outcome. For example, how long does it take to
#' get a patient into a fully squamous oesophagus after Barrett's ablation
#' for dysplasia?
#' @param dataframe The dataframe
#' @param HospNum The Hospital Number column
#' @param EVENT The column that contains the outcome of choice
#' @param indicatorEvent The name of the start event (can be a regular expression)
#' @param endEvent The name of the endpoint (can be a regular expression)
#' @keywords ourcome
#' @importFrom dplyr group_by slice mutate lead
#' @importFrom rlang sym
#' @export
#' @examples
#' # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' v <- Mypath
#' v$NumBx <- HistolNumbOfBx(v$Macroscopicdescription, "specimen")
#' v$BxSize <- HistolBxSize(v$Macroscopicdescription)
#' 
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v <- Endomerge2(
#'   Myendo, "Dateofprocedure", "HospitalNumber", v, "Dateofprocedure",
#'   "HospitalNumber"
#' )
#' 
#' # The function relies on the other Barrett's functions being run as well:
#' b1 <- Barretts_PragueScore(v, "Findings")
#' b1$IMorNoIM <- Barretts_PathStage(b1, "Histology")
#' colnames(b1)[colnames(b1) == "pHospitalNum"] <- "HospitalNumber"
#' 
#' # The function groups the procedures by patient and gives
#' # all the procedures between
#' # the indicatorEvent amd the procedure just after the endpoint.
#' # Eg if the start is RFA and the
#' # endpoint is biopsies then it will give all RFA procedures and
#' # the first biopsy procedure
#' 
#' b1$EndoscopyEvent <- EndoscopyEvent(
#'   b1, "Findings", "ProcedurePerformed",
#'   "Macroscopicdescription", "Histology"
#' )
#' nn <- TimeToStatus(b1, "HospitalNumber", "EndoscopyEvent", "rfa", "dilat")
#' rm(v)
TimeToStatus <- function(dataframe, HospNum, EVENT, indicatorEvent, endEvent) {
  dataframe <- data.frame(dataframe)
  HospNuma <- rlang::sym(HospNum)
  EVENTa <- rlang::sym(EVENT)
  startAndEnd <-
    dataframe %>%
    group_by(!!HospNuma) %>%
    mutate(ind = grepl(indicatorEvent, !!EVENTa, ignore.case = TRUE, perl = TRUE) &
      grepl(endEvent, !!EVENTa, ignore.case = TRUE, perl = TRUE)) %>%
    slice(sort(c(which(ind), which(ind) + 1)))
  startAndEnd <- data.frame(startAndEnd)
  return(startAndEnd)
}



#' Number of tests done per month and year by indication
#'
#' Get an overall idea of how many endoscopies have been done for an indication
#' by year and month. This is a more involved version of
#' SurveilCapacity function. It takes string for
#' the Indication for the test
#'
#' This returns a list which contains a plot (number of tests for that
#' indication over time and a table with the same information broken down
#' by month and year).
#' @param dataframe dataframe
#' @param Indication Indication column
#' @param Endo_ResultPerformed column containing date the Endoscopy was
#' performed
#' @param StringToSearch The string in the Indication to search for
#' @importFrom magrittr '%>%'
#' @importFrom dplyr arrange group_by mutate lead filter
#' @importFrom lubridate day week year month dmy
#' @importFrom magrittr '%>%'
#' @importFrom stringr str_detect
#' @importFrom rlang sym
#' @importFrom ggplot2 geom_smooth geom_line geom_point scale_x_date theme_bw
#' @keywords Tests number
#' @export
#' @examples
#' # This takes the dataframe MyEndo (part of the package examples) and looks in
#' # the column which holds the test indication (in this example it is called
#' # 'Indication' The date of the procedure column(which can be date format or
#' # POSIX format) is also necessary.  Finally the string which indicates the text
#' # indication needs to be inpoutted. In this case we are looking for all endoscopies done
#' # where the indication is surveillance (so searching on 'Surv' will do fine).
#' # If you want all the tests then put '.*' instead of Surv
#' rm(list = ls(all = TRUE))
#' ff <- HowManyOverTime(Myendo, "Indications", "Dateofprocedure", ".*")
HowManyOverTime <-
  function(dataframe,
             Indication,
             Endo_ResultPerformed,
             StringToSearch) {
    Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)

    TestNumbers <-
      dataframe %>%
      filter(
        str_detect(dataframe[, Indication], StringToSearch)
      ) %>%
      arrange(as.Date(!!Endo_ResultPerformeda)) %>%
      group_by(
        day = day(as.Date(!!Endo_ResultPerformeda)),
        week = week(as.Date(!!Endo_ResultPerformeda)),
        month = month(as.Date(!!Endo_ResultPerformeda)),
        year = year(as.Date(!!Endo_ResultPerformeda))
      ) %>%
      summarise(Number = dplyr::n())

    names(TestNumbers) <- c("day", "week", "month", "year", "freq")
    TestNumbers$MonthYear <-
      paste("01_", TestNumbers$month, "_", TestNumbers$year, sep = "")

    TestNumbers$MonthYear <- dmy(TestNumbers$MonthYear)
    TestNumbers <- data.frame(TestNumbers) %>% arrange(year, month, week, day)
    return(TestNumbers)
  }






##### Endoscopic Performance Quality- documentation.######

#' Extract from report, using words from a list
#'
#' The aim here is simply to
#' produce a document term matrix to get the frequency
#' of all the words, then extract the words you are
#' interested in with tofind then find which reports
#' have those words. Then find what proportion of the reports
#' have those terms.
#' @param theframe the dataframe,
#' @param EndoReportColumn the column of interest,
#' @param myNotableWords list of words you are interested in
#' @import tm
#' @keywords Lookup
#' @export
#' @examples
#' # The function relies on defined a list of
#' # words you are interested in and then choosing the column you are
#' # interested in looking in for these words. This can be for histopathology
#' # free text columns or endoscopic. In this example it is for endoscopic
#' # columns
#' myNotableWords <- c("arrett", "oeliac")
#' jj <- ListLookup(Myendo, "Findings", myNotableWords)
ListLookup <- function(theframe, EndoReportColumn, myNotableWords) {
  jeopCorpus <- Corpus(VectorSource(theframe[, EndoReportColumn]))
  # Get the frequency table of terms being used over all the reports
  # (ie counts x2 if mentioned twice in the report)
  dtm <- TermDocumentMatrix(jeopCorpus)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  d$word <- as.character(d$word)
  d$Prop <- (d$freq / nrow(theframe)) * 100

  # group all the words containing stems as per myNotableWords
  d <-
    vapply(myNotableWords, function(x)
      sum(d$Prop[grepl(x, d$word)]), numeric(1))
  d <- data.frame(X2 = names(d), Prop = as.vector(d))
  return(d)
}


############## Group by endoscopist #####

# Groups anything by Endoscopist and returns the table and a ggplot


#' Plot a metric by endoscopist
#'
#' This takes any of the numerical metrics in the dataset and plots it by
#' endoscopist.
#' It of course relies on a Endoscopist column being present
#' @param dataframe The dataframe
#' @param Column The column (numeric data) of interest
#' @param EndoscopistColumn The endoscopist column
#' @importFrom ggplot2 ggplot
#' @importFrom tidyr drop_na
#' @importFrom dplyr group_by summarise
#' @importFrom rlang sym
#' @keywords Endoscopist
#' @export

#' @examples #The function gives a table with any numeric
#' # metric by endoscopist
#' # In this example we tabulate medication by
#' # endoscopist
#' # Lets bind the output of EndoscMeds to the main dataframe so we
#' # have a complete dataframe with all the meds extracted
#' MyendoNew<-cbind(EndoscMeds(Myendo$Medications),Myendo)
#' 
#' # Now lets look at the fentanly use per Endoscopist:
#' kk<-MetricByEndoscopist(MyendoNew,'Endoscopist','Fent')
#' #EndoBasicGraph(MyendoNew, "Endoscopist", "Fent") #run this
#' #if you want to see the graph
#' rm(Myendo)
MetricByEndoscopist <- function(dataframe, Column, EndoscopistColumn) {
  group <- rlang::sym(Column)
  variable <- rlang::sym(EndoscopistColumn)

  NumBxPlot <-
    dataframe %>%
    tidyr::drop_na(!!variable) %>%
    group_by(!!group) %>%
    summarise(avg = mean(!!variable))

  NumBxPlot <- data.frame(NumBxPlot, stringsAsFactors = FALSE)
  return(NumBxPlot)
}


############## Group by endoscopist #####

# Groups anything by Endoscopist and returns the table

#' This creates a proportion table for categorical variables by endoscopist
#' It of course relies on a Endoscopist column being present
#' @param ProportionColumn The column (categorical data) of interest
#' @param EndoscopistColumn The endoscopist column
#' @importFrom ggplot2 ggplot
#' @importFrom tidyr drop_na
#' @importFrom dplyr group_by summarise
#' @importFrom rlang sym
#' @keywords Endoscopist
#' @export
#' @examples
#' # The function plots any numeric metric by endoscopist
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' v <- Mypath
#' v$NumBx <- HistolNumbOfBx(Mypath$Macroscopicdescription, "specimen")
#' v$BxSize <- HistolBxSize(v$Macroscopicdescription)
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v <- Endomerge2(
#'   Myendo, "Dateofprocedure", "HospitalNumber", v, "Dateofprocedure",
#'   "HospitalNumber"
#' )
#' # The function relies on the other Barrett's functions being run as well:
#' v$IMorNoIM <- Barretts_PathStage(v, "Histology")
#' colnames(v)[colnames(v) == "pHospitalNum"] <- "HospitalNumber"
#' # The function takes the column with the extracted worst grade of
#' # histopathology and returns the proportion of each finding (ie
#' # proportion with low grade dysplasia, high grade etc.) for each
#' # endoscopist
#' kk <- CategoricalByEndoscopist(v$IMorNoIM, v$Endoscopist)
#' rm(Myendo)
CategoricalByEndoscopist <- function(ProportionColumn, EndoscopistColumn) {

  # Need to include indefinite for dysplasia
  PropByEndo <- prop.table(table(
    EndoscopistColumn,
    ProportionColumn
  ))
  return(PropByEndo)
}
