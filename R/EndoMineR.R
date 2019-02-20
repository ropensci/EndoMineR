

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





###### Graphics ########################################## 

# EndoSubsetEMR <- EndoSubsetEMR[EndoSubsetEMR$EVENT == "EMR", ]
# 
# # Create the matrix
# df3 <-
#   data.frame(EndoSubsetEMR$ParisClass, EndoSubsetEMR$IMorNoIM)
# # Reorganise the column names and rows Get rid of no Paris EMR's
# dfy <- df3[!df3$EndoSubsetEMR.ParisClass == "No_Paris", ]
# # Get the histology proportions by the Paris grade
# tr4 <- as.data.frame.matrix(prop.table(table(dfy), 1))
# 
# tr5 <- as.matrix(tr4)
# tr5 <- head(tr5, -1)
# # Create the heatmap par(oma = c(4, 0, 0, 4))
# 
# tr5 <- tr5[!!rowSums(!is.na(tr5)), ]
# tr5 <- t(tr5)
# tr5 <- tr5[!!rowSums(!is.na(tr5)), ]
# tr5 <- t(tr5)
# if (nrow(tr5) > 2 & ncol(tr5) > 2) {
#   colors <- c(seq(-1, 0.2, length = 100),
#               seq(0.21, 0.8, length = 100),
#               seq(0.81, 1, length = 100))
#   
#   gplots::heatmap.2(
#     tr5,
#     trace = "none",
#     breaks = colors,
#     density.info = "none",
#     dendrogram = "none",
#     Rowv = FALSE,
#     Colv = FALSE,
#     cexRow = 3.5,
#     cexCol = 1.5
#   )
# }


########################################## Patient flow functions#######




#' #' Determine the patient metric of choice over time WORK IN PROGRESS
#' #'
#' #' This function aims to show what is happening over time to a metric
#' #' of choice on a per patient basis.
#' #' An example might be to demonstrate the worst grade of histopathology
#' #' on repeated endoscopic biopsies eg for Barrett's oesophagus
#' #' @param theframe the dataframe,
#' #' @param EndoReportColumn the column of interest,
#' #' @param myNotableWords list of words you are interested in
#' #' @import ggplot2 
#' #' @import stringr
#' #' @import ggplus
#' #' @keywords patient flow
#' #' @export
#' #' @examples #The function relies on defined a list of
#' #' # words you are interested in and then choosing the column you are
#' #' # interested in looking in for these words. This can be for histopathology
#' #' # free text columns or endoscopic. In this example it is for endoscopic
#' #' # columns
#' #' v<-HistolAll(Mypath)
#' #' v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure','HospitalNumber')
#' #' b<-Barretts_PathStage(v,'Histology')
#' #' aa<-Barretts_PragueScore(b,'Findings')
#' #' aa<-SurveilTimeByRow(aa,'pHospitalNum','Date.y')
#' #' myNotableWords<-c("No_IM","IM","LGD","HGD","T1a)
#' #' PatientFlowBasic(aa,"IMorNoIM",myNotableWords)
#' 
#' 
#' 
#' PatientFlowBasic <- function(theframe, EndoReportColumn, myNotableWords) {
#' 
#'   theframe["RecodedColumn"] <- as.integer(factor(theframe[,EndoReportColumn], myNotableWords, ordered = TRUE))
#'   
#'   #Now develop the patient specific journey with faceted plot in ggplot2
#'   f<-ggplot(theframe) +
#'     geom_point(aes(Date.x,type),shape=16,size=1) +
#'     xlab("Date") + 
#'     ylab("Histopathological State") +
#'     theme(axis.text.x=element_text(angle=-90)) 
#'   
#'   
#'   
#'   t<-facet_multiple(plot = f, 
#'                  facets = 'pHospitalNum', 
#'                  ncol = 2, 
#'                  nrow = 2)
#'   mylist<-as.list(aa,t)
#'   
#'   return(mylist)
#' }










#' Create a Sankey plot for patient flow
#'
#' This creates a Sankey plot to see the order of tests for all patients:
#' dfw is the dataframe, y is the value of in this case
#' the procedure type (eg EMR,
#'  radiofrequency ablation for Barrett's but can be
#'  any description of a procedure you desire)
#'  Note the Hospital Number column MUST be called PatientID.
#' @param dfw the dataframe extracted using the standard cleanup scripts
#' @param ProcPerformedColumn the column containing the test like P
#' rocPerformed for example
#' @param PatientID the column containing the patients unique identifier
#' eg hostpital number
#' @importFrom dplyr group_by
#' @importFrom magrittr '%>%'
#' @importFrom data.table 'setDT' 'rowid'
#' @keywords Sankey
#' @export
#' @examples # The purpose of the function is to
#' # provide a Sankey plot which allows the analyst to see the proportion
#' # of patients moving from one state (in this case type of Procedure) to
#' # another. This allows us to see for example how many EMRs are done after
#' #RFA. For further patient flow examples see PatientFlow_CircosPlots
#' names(Myendo)[names(Myendo) == 'HospitalNumber'] <- 'PatientID'
#' gg<-SurveySankey(Myendo,"ProcedurePerformed","PatientID")

SurveySankey <- function(dfw, ProcPerformedColumn, PatientID) {
  # Create the Sankey diagrams
  Sankey <-
    reshape2::dcast(setDT(dfw)[, .SD, PatientID],
                    PatientID ~ rowid(PatientID),
                    value.var = ProcPerformedColumn)
  PtFlow <- Sankey
  PtFlow <- data.frame(PtFlow)
  PtFlow <- PtFlow[!is.na(names(PtFlow))]
  r <- c()
  
  #names(PtFlow)<-gsub("X(\\d+)","Event\\1",names(PtFlow))
  for (i in seq_along(PtFlow)) {
    t <- paste("ord", i, sep = "")
    r <- c(r, t)
    names(PtFlow) <- r
  }
  orders <- PtFlow %>% select(names(PtFlow))
  orders.plot <- data.frame()
  for (i in 3:ncol(orders)) {
    ord.cache <-
      orders %>% group_by(orders[, i - 1], orders[, i]) %>% summarise(n = n())
    
    colnames(ord.cache)[1:2] <- c("from", "to")
    
    # adding tags to carts
    ord.cache$from <-
      paste(ord.cache$from, "(", i - 1, ")", sep = "")
    ord.cache$to <- paste(ord.cache$to, "(", i, ")", sep = "")
    
    ord.cache <- data.frame(ord.cache)
    orders.plot <- rbind(orders.plot, ord.cache)
    
  }
  
  
  orders.plot <- data.frame(orders.plot)
  orders.plot <-
    orders.plot[grepl("[A-Z]", orders.plot$from) &
                  grepl("[A-Z]", orders.plot$to),]
  orders.plot <-
    orders.plot[!grepl("NA", orders.plot$from) &
                  !grepl("NA", orders.plot$to),]
  plot(
    googleVis::gvisSankey(
      orders.plot,
      from = "from",
      to = "to",
      weight = "n",
      options = list(
        height = 900,
        width = 1800,
        sankey = "{link:{color:{fill:'black',stroke: 'black', strokeWidth: 1 }},
        node: { color: { fill: '#a61d4c' },
        label: { color: '#871b47',fontName: 'Open Sans',fontSize: 35 } }}"
      )
      )
      )
}


#' Create a Circos plot for patient flow
#'
#' This allows us to look at the overall flow from one
#' type of procedure to another using circos plots.
#' @param dataframe dataframe
#' @param Endo_ResultPerformed the column containing the date of the procedure
#' @param ProcPerformed The procedure that you want to plot (eg EMR,
#'  radiofrequency ablation for Barrett's but can be
#'  any dscription of a procedure you desire)
#' @param HospNum_Id Column with the patient's unique hospital number
#' @importFrom dplyr arrange group_by mutate select summarise lag ungroup rename
#' @importFrom tidyr separate
#' @importFrom magrittr '%>%'
#' @importFrom rlang sym
#' @keywords Circos
#' @export
#' @examples # This function builds a circos plot which gives a more aggregated
#' # overview of how patients flow from one state to another than the
#' # SurveySankey function
#' # Build a list of procedures
#' Event <- list(x1 = "Therapeutic- Dilatation",
#' x2 = "Other-", x3 = "Surveillance",
#' x4 = "APC", x5 = "Therapeutic- RFA TTS",
#' x5 = "Therapeutic- RFA 90",
#' x6 = "Therapeutic- EMR", x7 = "Therapeutic- RFA 360")
#' EndoEvent<-replicate(2000,sample(Event,1, replace = FALSE))
#' # Merge the list with the Myendo dataframe
#' fff<-unlist(EndoEvent)
#' fff<-data.frame(fff)
#' names(fff)<-"col1"
#' Myendo<-cbind(fff$col1,Myendo)
#' names(Myendo)[names(Myendo) == 'HospitalNumber'] <- 'PatientID'
#' names(Myendo)[names(Myendo) == 'fff$col1'] <- 'EndoEvent'
#' # Myendo$EndoEvent<-as.character(Myendo$EndoEvent)
#' # Run the function using the procedure information (the date of the
#' # procedure, the Event type and the individual patient IDs)
#' hh<-PatientFlow_CircosPlots(Myendo,"Dateofprocedure","PatientID","EndoEvent")
#' rm(Myendo)
#' rm(EndoEvent)


PatientFlow_CircosPlots <-
  function(dataframe,
           Endo_ResultPerformed,
           HospNum_Id,
           ProcPerformed) {
    Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)
    HospNum_Ida <- rlang::sym(HospNum_Id)
    ProcPerformeda <- rlang::sym(ProcPerformed)
    
    mydf <-
      dataframe %>% arrange(!!Endo_ResultPerformeda) %>%
      group_by(!!HospNum_Ida) %>%
      mutate(origin = lag(!!ProcPerformeda, 1),
             destination = !!ProcPerformeda) %>%
      select(origin, destination, PatientID) %>%
      group_by(origin, destination, PatientID) %>%
      summarise(n = n()) %>% ungroup()
    
    mydf <- data.frame(reshape2::dcast(mydf, origin ~ destination))
    
    # Get rid of NA's
    mydf <- mydf[complete.cases(mydf),]
    
    V1 <- c("2", "7", "3", "10")
    V2 <- c("210,150,12", "110,255,233", "125,175,0", "255,219,0")
    
    mydf <- cbind(V1, V2, mydf)
    
    df_format <-
      mydf %>% select(1:3) %>% rename(order = V1,
                                      rgb = V2,
                                      region = origin) %>%
      mutate(region = gsub("_", " ", region))
    # flow matrix. Need to add V1 and V2 to the matrix here
    
    matmydf <- as.matrix(mydf[,-(1:3)])
    dimnames(matmydf) <-
      list(orig = df_format$region, dest = df_format$region)
    # library('tidyr')
    df_format <-
      df_format %>% dplyr::arrange(order) %>%
      separate(rgb, c("r", "g", "b")) %>%
      mutate(col = rgb(r, g, b, max = 255),
             max = rowSums(matmydf) + colSums(matmydf))
    
    
    
    circlize::circos.clear()
    par(mar = rep(0, 4), cex = 0.9)
    circlize::circos.par(start.degree = 90, gap.degree = 4)
    par(cex = 0.8, mar = c(0, 0, 0, 0))
    circlize::chordDiagram(
      x = matmydf,
      directional = 1,
      order = df_format$region,
      grid.col = df_format$col,
      annotationTrack = "grid",
      transparency = 0.25,
      annotationTrackHeight = c(0.1, 0.1),
      diffHeight = -0.04
    )
    
    
    circlize::circos.trackPlotRegion(
      track.index = 1,
      panel.fun = function(x, y) {
        xlim <- circlize::get.cell.meta.data("xlim")
        ylim <- circlize::get.cell.meta.data("ylim")
        sector.index <- circlize::get.cell.meta.data("sector.index")
        circlize::circos.text(
          mean(xlim),
          mean(ylim),
          sector.index,
          col = "black",
          cex = 0.6,
          facing = "inside",
          niceFacing = TRUE
        )
      },
      bg.border = NA
    )
  }
##################### How many tests #########################
#Use ggthemes
#Make sure the data is inputted in the correct format
#Make sure the user just has to input the data structure with what gets plotted
#from the original function.

# # Then just plot it:
# Myplot <-
#   ggplot(data = TestNumbers2, aes(x = year, y = FreqYear)) +
#   geom_point() +
#   geom_line() +
#   geom_smooth(method = "loess") +
#   theme_bw() +
#   labs(title="Number of procedures per year")

# 
# #################Metrics ByEndoscopist############
# 
# ggplot(NumBxPlot) + geom_point(aes(x = Endoscopist, y = avg),
#                                colour = "red",
#                                size = 3) + labs(title = "Average  by endoscopist") +
#   theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
#   theme(axis.text.x = element_text(angle = -90)) +
#   theme(axis.text.y = element_text(angle = -90)) +
#   theme(legend.position = "top")
# 
# 
# 
# #################BarrettsBasicNumbers#################
# xNumPlot <-
#   ggplot(xNum, aes(
#     x = year,
#     y = n,
#     group = EVENT,
#     colour = EVENT
#   )) + 
#   geom_line() + geom_smooth()+
#   labs(title="Number of procedures by type")


##################BarrettsBxQual#########################
# e) Then show shortfall of number of biopsies on a graph
t <-
  ggplot() +
  geom_point(aes(BxShortfallPre$Endoscopist, BxShortfallPre$MeanDiff),
             size = 9) + geom_point(cex = 2) +
  labs(title = "Shortfall number of biopsies on Barrett's Surveillance list",
       x = "", y = "") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
  theme(legend.position = "top") +
  xlab("Endoscopist") +
  ylab("Shortfall(Obs-\nexpec number Bx)") +
  theme(axis.text.x = element_text(angle = -90, size = 10)) +
  theme(axis.text.y = element_text(angle = -90, size = 10)) +
  theme(axis.title = element_text(size = 10)) +
  theme(title = element_text(size = 10)) +
  theme(legend.position = "top")

##################BarrettsDocumentQuality###############
t <-ggplot(EndoMinDataSet, aes(x=b, y=Proportion)) + 
  geom_bar(stat="identity",fill="blue")+
  xlab("Documentation")+
  labs(title="Proportion of Reports Containing Terms")+
  coord_flip()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))
##################EMRResults###############
barplot(
  EMRResult$n,
  names.arg = c(
    "AllEMRs",
    "SM2",
    "SM1",
    "T1b_Unspec",
    "T1a",
    "HGD",
    "LGD",
    "IM",
    "No IM"
  ),
  xlab = "Tissue grade",
  ylab = "Number of EMRs",
  cex.lab = 2.0,
  cex.axis = 1.5,
  cex.main = 1.5,
  cex.names = 1.5,
  main = "EMR Tissue pathology results"
)
