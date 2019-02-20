

if (getRversion() >= "2.15.1")
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
      "FreqYear"
    )
  )

######## Surveillance functions ######

#' Extract the time difference between each test in days
#'
#' This determines the time difference between each test for a patient in days
#' It returns the time since the first and the last study.
#'
#' @param dataframe dataframe,
#' @param HospNum_Id Patient ID
#' @param Endo_ResultPerformed Date of the Endoscopy
#' @importFrom dplyr arrange group_by mutate lead
#' @importFrom magrittr '%>%'
#' @importFrom rlang sym
#' @keywords Surveillance
#' @export
#' @examples aa<-SurveilTimeByRow(Myendo,'HospitalNumber',
#' 'Dateofprocedure')

SurveilTimeByRow <-
  function(dataframe, HospNum_Id, Endo_ResultPerformed) {
    HospNum_Ida <- rlang::sym(HospNum_Id)
    Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
    ret<-dataframe %>% arrange(!!HospNum_Ida,!!Endo_ResultPerformeda) %>%
      group_by(!!HospNum_Ida) %>%
      mutate(TimeToNext = difftime(as.Date(!!Endo_ResultPerformeda), lead(as.Date(
        !!Endo_ResultPerformeda
      ), 1), units = "days")) %>%
      mutate(TimeSinceLast = difftime(Sys.Date(), dplyr::last(!!Endo_ResultPerformeda),
                                      units = "days"))
    dataframe<-data.frame(ret)
    return(dataframe)
  }



#' Extract the last test done by a patient only
#'
#' Extracts the last test only per patient
#' @param dataframe dataframe
#' @param HospNum_Id Patient ID
#' @param Endo_ResultPerformed Date of the Endoscopy
#' @importFrom magrittr '%>%'
#' @importFrom dplyr arrange group_by filter row_number
#' @importFrom rlang sym
#' @keywords Surveillance
#' @export
#' @examples cc<-SurveilLastTest(Myendo,'HospitalNumber','Dateofprocedure')



SurveilLastTest <-
  function(dataframe, HospNum_Id, Endo_ResultPerformed) {
    HospNum_Ida <- rlang::sym(HospNum_Id)
    Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)
    ret<-dataframe %>% group_by(!!HospNum_Ida) %>%
      arrange(!!Endo_ResultPerformeda) %>%
      filter(row_number() == n())
    dataframe<-data.frame(ret)
    return(dataframe)
  }


#' Extracts the first test only per patient
#'
#' Extracts the first test only per patient
#' @param dataframe dataframe
#' @param HospNum_Id Patient ID
#' @param Endo_ResultPerformed Date of the Endoscopy
#' @importFrom dplyr arrange group_by filter
#' @importFrom magrittr '%>%'
#' @importFrom rlang sym
#' @keywords Surveillance
#' @export
#' @examples dd<-SurveilFirstTest(Myendo,'HospitalNumber',
#' 'Dateofprocedure')


SurveilFirstTest <-
  function(dataframe, HospNum_Id, Endo_ResultPerformed) {
    HospNum_Ida <- rlang::sym(HospNum_Id)
    Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)
    ret<-dataframe %>% group_by(!!HospNum_Ida) %>%
      arrange(!!Endo_ResultPerformeda) %>%
      filter(row_number() == 1)
    dataframe<-data.frame(ret)
    return(dataframe)
  }


#' Last status
#'
#' This looks at the last EVENT a patient had to get time to outcomes.
#' For example if a patient underwent ablation for Barrett's oesophagus we
#' could get the CRIM score by assessing the first time-point where the event
#' is nothing after RFA has been done.
#' @param dataframe The dataframe
#' @param HospNum The Hospital Number column
#' @param EVENT The column  called EVENT that determines what procedure the
#' patient had at that endoscopy
#' @param indicatorEvent The name of the Event that references the outcome
#' @param endEvent The event that indicates the outcome has been reached
#' @keywords CRIM
#' @importFrom dplyr group_by slice mutate lead
#' @importFrom rlang sym
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
#' colnames(b2)[colnames(b2) == 'pHospitalNum'] <- 'HospitalNumber'
#' # The function groups the procedures by patient and then looks at those which
#' # have 'nothing' in the event column (which means biopsies only) which was
#' # preceded by radiofrequency ablation (RFA) so that these patients are
#' # labelled as having clearance of intestinal metaplasia. The result is a true
#'# or false column.
#' b2$EVENT<-EndoscopyEvent(b2,"Findings","ProcedurePerformed","Macroscopicdescription","Histology")
#' nn<-Barretts_CRIM(b2,'HospitalNumber',"EVENT")
#' rm(v)

LastStatus <- function(dataframe, HospNum, EVENT,indicatorEvent,endEvent) {
  dataframe <- data.frame(dataframe)
  HospNuma <- rlang::sym(HospNum)
  EVENTa <- rlang::sym(EVENT)
  
  CRIM <-
    dataframe %>% group_by(!!HospNuma) %>%
    mutate(ind = (!!EVENTa) == indicatorEvent &
             lead(!!EVENTa) == endEvent) %>%
    slice(sort(c(which(ind), which(ind) + 1)))
  CRIM<-data.frame(CRIM)
  return(CRIM)
}

#' Number of tests done per month
#'
#' This determines the number of tests done per month
#' @param dataframe dataframe
#' @param Endo_ResultPerformed Column with the date the Endoscopy was performed
#' @importFrom dplyr group_by summarise
#' @importFrom lubridate dmy month
#' @importFrom magrittr '%>%'
#' @importFrom rlang sym
#' @keywords cats
#' @export
#' @examples ee<-SurveilCapacity(Myendo,'Dateofprocedure')

SurveilCapacity <- function(dataframe, Endo_ResultPerformed) {
  Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)
  ret<-dataframe %>% mutate(month =format(as.Date(!!Endo_ResultPerformeda), "%m"),
                            year = format(!!Endo_ResultPerformeda, "%Y")) %>%
    group_by(year,month) %>% summarise(n = n())
  dataframe<-data.frame(ret)
  return(dataframe)
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
#' @examples # This takes the dataframe MyEndo (part of the package examples) and looks in
#' # the column which holds the test indication (in this example it is called
#' # 'Indication' The date of the procedure column(which can be date format or
#' # POSIX format) is also necessary.  Finally the string which indicates the text
#' # indication needs to be inpoutted. In this case we are looking for all
#' # endoscopies done
#' # where the indication is surveillance (so searching on 'Surv' will do fine) .
#' rm(list=ls(all=TRUE))
#' ff<-HowManyTests(Myendo,'Indications','Dateofprocedure','Surv')


HowManyTests <-
  function(dataframe,
           Indication,
           Endo_ResultPerformed,
           StringToSearch) {
    Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)
    TestNumbers <-
      dataframe %>% filter(
        str_detect(dataframe[, Indication], StringToSearch)) %>%
      arrange(as.Date(!!Endo_ResultPerformeda)) %>% group_by(
        day = day(as.Date(!!Endo_ResultPerformeda)),
        week = week(as.Date(!!Endo_ResultPerformeda)),
        month = month(as.Date(!!Endo_ResultPerformeda)),
        year = year(as.Date(!!Endo_ResultPerformeda))
      ) %>%
      summarise(Number = n())
    names(TestNumbers) <- c("day", "week", "month", "year", "freq")
    TestNumbers$MonthYear <-
      paste("01_", TestNumbers$month, "_", TestNumbers$year, sep = "")
    TestNumbers$MonthYear <- dmy(TestNumbers$MonthYear)
    
    TestNumbers<-data.frame(TestNumbers)%>% arrange(year,month,week,day)
    TestNumbers2<-TestNumbers%>%select(year,freq) %>%
      group_by(year) %>%
      summarise(FreqYear=n())
    TestNumbers2<-data.frame(TestNumbers2)

    return(TestNumbers2)
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
#' @examples #The function relies on defined a list of
#' # words you are interested in and then choosing the column you are
#' # interested in looking in for these words. This can be for histopathology
#' # free text columns or endoscopic. In this example it is for endoscopic
#' # columns
#' myNotableWords<-c('arrett','oeliac')
#' jj<-ListLookup(Myendo,'Findings',myNotableWords)

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
      sum(d$Prop[grepl(x, d$word)]),numeric(1))
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
#' @examples #The function plots any numeric metric by endoscopist
#' # and also gives a table with it. In this example we plot medication by
#' # endoscopist
#' Myendo<-EndoscMeds(Myendo,'Medications')
#' kk<-MetricByEndoscopist(Myendo,'Endoscopist','Fent')
#' rm(Myendo)


MetricByEndoscopist <- function(dataframe, Column, EndoscopistColumn) {
  group <- rlang::sym(Column)
  variable <- rlang::sym(EndoscopistColumn)
  
  NumBxPlot <-
    dataframe %>% tidyr::drop_na(!!variable) %>% group_by(!!group) %>%
    summarise(avg = mean(!!variable))
  NumBxPlot<-data.frame(NumBxPlot,stringsAsFactors = FALSE)
}


############## Group by endoscopist #####

# Groups anything by Endoscopist and returns the table and a ggplot


#' Plot a frequency table for categorical variables by endoscopist
#'
#' This takes any of the numerical metrics in the dataset and plots it by
#' endoscopist.
#' It of course relies on a Endoscopist column being present
#' @param ProportionColumn The column (categorical data) of interest
#' @param EndoscopistColumn The endoscopist column
#' @importFrom ggplot2 ggplot
#' @importFrom tidyr drop_na
#' @importFrom dplyr group_by summarise
#' @importFrom rlang sym
#' @keywords Endoscopist
#' @export
#' @examples #The function plots any numeric metric by endoscopist
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
#' b1<-Barretts_PragueScore(v,'Findings')
#' b2<-Barretts_PathStage(b1,'Histology')

#' # The follow-up group depends on the histology and the Prague score for a
#' # patient so it takes the processed Barrett's data and then looks in the
#' # Findings column for permutations of the Prague score.
#' b4<-Barretts_FUType(b2,"CStage","MStage","IMorNoIM")
#' colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#' # The function takes the column with the extracted worst grade of
#' # histopathology and returns the proportion of each finding (ie
#' # proportion with low grade dysplasia, high grade etc.) for each
#' # endoscopist
#' kk<-CategoricalByEndoscopist(b4$IMorNoIM,b4$Endoscopist)
#' rm(Myendo)



CategoricalByEndoscopist <- function(ProportionColumn, EndoscopistReportColumn) {
 
  
  # Need to include indefinite for dysplasia
  PropByEndo <- prop.table(table(EndoscopistReportColumn,
                            ProportionColumn))
  return(PropByEndo)
}




########################### Diagnostic yield functions #######

#' Create GRS metrics by endoscopist (X-ref with pathology)
#'
#' This extracts the polyps types from the data
#' (for colonoscopy and flexible sigmoidosscopy data)
#' and output the adenoma,adenocarcinoma and
#' hyperplastic detection rate by endoscopist as well
#' as overall number of colonoscopies.
#' This will be extended to other GRS outputs in the future.
#' @param dataframe The dataframe
#' @param ProcPerformed The column containing the Procedure type performed
#' @param Endo_Endoscopist column containing the Endoscopist name
#' @param Dx The column with the Histological diagnosis
#' @param Histol The column with the Histology text in it
#' @importFrom dplyr group_by_ do full_join
#' @keywords Withdrawal
#' @export
#' @examples # Firstly merge histology and endoscopy datasets for the colon:
#'
#' MypathColon<-PathDataFrameFinalColon
#' MyendoColon <- ColonFinal
#' MyendoColon$OGDReportWhole <-gsub("2nd Endoscopist:","Second endoscopist:",
#' MyendoColon$OGDReportWhole)
#' EndoscTree <-c("Hospital Number:","Patient Name:","General Practitioner:",
#'        "Date of procedure:","Endoscopist:","Second endoscopist:","Medications",
#'        "Instrument","Extent of Exam:","Indications:","Procedure Performed:",
#'        "Findings:","Endoscopic Diagnosis:")
#' MyendoColon<-Extractor(MyendoColon,"OGDReportWhole",EndoscTree)
#' Histoltree <-c(
#'     "Hospital Number:","Patient Name:","DOB:","General Practitioner:",
#'     "Date received:","Clinical Details","Nature of specimen","Macroscopic description:","Histology",
#'     "Diagnosis")
#'
#' MypathColon <-Extractor(MypathColon,"PathReportWhole",Histoltree)
#'
#' names(MypathColon)[names(MypathColon) == 'Datereceived'] <- 'Dateofprocedure'
#' MypathColon$Dateofprocedure <- as.Date(MypathColon$Dateofprocedure)
#' vColon <-Endomerge2(MypathColon, "Dateofprocedure","HospitalNumber",
#'                     MyendoColon, "Dateofprocedure","HospitalNumber")
#' nn<-GRS_Type_Assess_By_Unit(vColon,'ProcedurePerformed',
#' 'Endoscopist','Diagnosis','Histology')
#' rm(vColon)
#' rm(MypathColon)
#' rm(MyendoColon)

################### GRS IS JUST A JOINED PROPORTIONS PROBLEM #########################
GRS_Type_Assess_By_Unit <-
  function(dataframe,
           ProcPerformed,
           Endo_Endoscopist,
           Dx,
           Histol) {
    dataframe <- data.frame(dataframe)
    
    
    dataframe <- dataframe[grepl("Colonoscopy", dataframe[, ProcPerformed]),]
    
    #Function should get proportions of a procedure that result in a finding:
    
    Adenoma2<-CategoricalByEndoscopist(Endo_Endoscopist,Histol)
    Adenoma<-dataframe %>% group_by_(Endo_Endoscopist) %>% summarise(Adenoma=(sum(grepl("[Aa]denoma", Original.y))/n())*100)
    Adenocarcinoma<-dataframe %>% group_by_(Endo_Endoscopist) %>% summarise(Adenocarcinoma=(sum(grepl(".*denoca.*", Original.y))/n())*100)
    HGD<-dataframe %>% group_by_(Endo_Endoscopist) %>% summarise(HGD=(sum(grepl(".*[Hh]igh [Gg]rade.*", Original.y))/n())*100)
    LGD<-dataframe %>% group_by_(Endo_Endoscopist) %>% summarise(LGD=(sum(grepl(".*[Ll]ow [Gg]rade.*", Original.y))/n())*100)
    Serrated<-dataframe %>% group_by_(Endo_Endoscopist) %>% summarise(Serrated=(sum(grepl(".*[Ss]errated.*", Original.y))/n())*100)
    Hyperplastic<-dataframe %>% group_by_(Endo_Endoscopist) %>% summarise(Hyperplastic=(sum(grepl(".*yperplastic.*", Original.y))/n())*100)
    
    
    
    FinalTable <-
      full_join(Adenoma, Adenocarcinoma, by = Endo_Endoscopist)
    FinalTable <-
      full_join(FinalTable, HGD, by = Endo_Endoscopist)
    FinalTable <-
      full_join(FinalTable, LGD, by = Endo_Endoscopist)
    FinalTable <-
      full_join(FinalTable, Serrated, by = Endo_Endoscopist)
    FinalTable <-
      full_join(FinalTable, Hyperplastic, by = Endo_Endoscopist)
    
    # Need to add the total colonoscopy count in here
    FinalTable<-data.frame(FinalTable)
    return(FinalTable)
  }





