

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
    HospNum_Ida <- sym(HospNum_Id)
    Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
    ret<-dataframe %>% arrange(!!HospNum_Ida,!!Endo_ResultPerformeda) %>%
      group_by(!!HospNum_Ida) %>%
      mutate(TimeToNext = difftime(as.Date(!!Endo_ResultPerformeda), lead(as.Date(
        !!Endo_ResultPerformeda
      ), 1), units = "days")) %>%
      mutate(TimeSinceLast = difftime(Sys.Date(), last(!!Endo_ResultPerformeda),
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
    HospNum_Ida <- sym(HospNum_Id)
    Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
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
    HospNum_Ida <- sym(HospNum_Id)
    Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
    ret<-dataframe %>% group_by(!!HospNum_Ida) %>%
      arrange(!!Endo_ResultPerformeda) %>%
      filter(row_number() == 1)
    dataframe<-data.frame(ret)
    return(dataframe)
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
  Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
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
    Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
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
    
    
    # # Then just plot it:
    Myplot <-
      ggplot(data = TestNumbers2, aes(x = year, y = FreqYear)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "loess") +
      theme_bw() +
      labs(title="Number of procedures per year")
    functionResults <-
      list(Myplot = Myplot, TestNumbers = TestNumbers)
    return(functionResults)
  }




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
#' #' bb<-Barretts_EventType(aa,'Histology','ProcedurePerformed','Indications','Findings')
#' #' aa<-SurveilTimeByRow(bb,'pHospitalNum','Date.y')
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
    Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
    HospNum_Ida <- sym(HospNum_Id)
    ProcPerformeda <- sym(ProcPerformed)
    
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


############## Pathology Quality #####

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
  group <- sym(Column)
  variable <- sym(EndoscopistColumn)
  
  NumBxPlot <-
    dataframe %>% tidyr::drop_na(!!variable) %>% group_by(!!group) %>%
    summarise(avg = mean(!!variable))
  
  
  ggplot(NumBxPlot) + geom_point(aes(x = Endoscopist, y = avg),
                                 colour = "red",
                                 size = 3) + labs(title = "Average  by endoscopist") +
    theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    theme(axis.text.x = element_text(angle = -90)) +
    theme(axis.text.y = element_text(angle = -90)) +
    theme(legend.position = "top")
}








#' Determine polyp location
#'
#' This should be used after the TermStandardizer
#' as it relies on the presence of a SampleLocation column
#' which the TermStandardLocation produces.
#' @param dataframe The dataframe
#' @param SampleLocationColumn The column containing the SampleLocation from the
#' TermStandardLocation
#' @keywords Withdrawal
#' @importFrom stringr str_match_all
#' @export
#' @examples
#' mywords<-c("Hospital Number","Patient Name:","DOB:","General Practitioner:",
#' "Date received:","Clinical Details:","Macroscopic description:",
#' "Histology:","Diagnosis:")
#' MypathExtract1<-Extractor(PathDataFrameFinal,"PathReportWhole",mywords)
#' names(MypathExtract1)[names(MypathExtract1) == 'Datereceived'] <- 'Dateofprocedure'
#' MypathExtract1$Dateofprocedure <- as.Date(MypathExtract1$Dateofprocedure)
#' # The polyp locator then determines where the biopsies were taken from
#' # by assessing the SampleLocation column which comes from the
#' # TermStandardLocation function.
#' f<-TermStandardLocation(MypathExtract1,'Histology')
#' mm<-PolypLocator(f,'SampleLocation')
#' rm(MypathExtract1)



PolypLocator <- function(dataframe, SampleLocationColumn) {
  dataframe <- data.frame(dataframe)
  
  
  dataframe$PolypLocator <-
    str_match_all(dataframe[, SampleLocationColumn], ".*[Pp]olyp.*")
  
  dataframe$PolypLocator <- str_match_all(dataframe$PolypLocator
                                          , LocationList())
  dataframe$PolypLocator <- lapply(dataframe$PolypLocator, function(p)
    unique(p))
  
  
  return(dataframe)
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

GRS_Type_Assess_By_Unit <-
  function(dataframe,
           ProcPerformed,
           Endo_Endoscopist,
           Dx,
           Histol) {
    dataframe <- data.frame(dataframe)
    
    # Adenomas by endoscopist ============
    
    dataframe <- dataframe[grepl("Colonoscopy", dataframe[, ProcPerformed]),]
    MyColonDataAdenomaDetectionByEndoscopist <-
      dataframe[grep(".*[Aa]denom.*", dataframe[, Dx]),]
    MyColonDataAdenomaDetectionByEndoscopist <-
      MyColonDataAdenomaDetectionByEndoscopist %>%
      group_by_(Endo_Endoscopist) %>%
      do(data.frame(NumAdenomas = nrow(.)))
    
    MyColonDataColonoscopiesByEndoscopist <-
      dataframe %>% group_by_(Endo_Endoscopist) %>%
      do(data.frame(NumColons = nrow(.)))
    
    # Merge the two above by column to get proportion:
    MyColonDataADR <-
      full_join(
        MyColonDataAdenomaDetectionByEndoscopist,
        MyColonDataColonoscopiesByEndoscopist,
        by = Endo_Endoscopist
      )
    MyColonDataADR$PropAdenomas <-
      (MyColonDataADR$NumAdenomas / MyColonDataADR$NumColons) * 100
    
    # Adenocarcinomas (without adenomas) by endoscopist=====
    
    MyColonDataAdenoCarcinomaDetectionByEndoscopist <-
      dataframe[grepl(".*denoca.*", dataframe[, Histol]) &
                  !grepl(".*denom.*", dataframe[, Histol]),]
    MyColonDataAdenoCarcinomaDetectionByEndoscopist <-
      MyColonDataAdenoCarcinomaDetectionByEndoscopist %>%
      group_by_(Endo_Endoscopist) %>%
      do(data.frame(NumAdenocarcinomas = nrow(.)))
    
    MyColonDataAdenocarcinomas <-
      full_join(
        MyColonDataAdenoCarcinomaDetectionByEndoscopist,
        MyColonDataColonoscopiesByEndoscopist,
        by = Endo_Endoscopist
      )
    MyColonDataAdenocarcinomas$PropAdenocarcinomas <-
      (
        MyColonDataAdenocarcinomas$NumAdenocarcinomas /
          MyColonDataAdenocarcinomas$NumColons
      ) * 100
    
    # Dysplastic grade of adenomas by endoscopist (from whole dataset) =====
    MyColonData_HG_AdenomaDetectionByEndoscopist <-
      dataframe[grepl(".*denoma.*", dataframe[, Histol]) &
                  grepl(".*[Hh]igh [Gg]rade.*", dataframe[, Histol]),]
    MyColonData_HG_AdenomaDetectionByEndoscopist <-
      MyColonData_HG_AdenomaDetectionByEndoscopist %>%
      group_by_(Endo_Endoscopist) %>%
      do(data.frame(NumHighGradeAdenomas = nrow(.)))
    
    MyColonData_LG_AdenomaDetectionByEndoscopist <-
      dataframe[grepl(".*denoma.*", dataframe[, Histol]) &
                  grepl(".*[Ll]ow [Gg]rade.*", dataframe[, Histol]),]
    MyColonData_LG_AdenomaDetectionByEndoscopist <-
      MyColonData_LG_AdenomaDetectionByEndoscopist %>%
      group_by_(Endo_Endoscopist) %>%
      do(data.frame(NumLowGradeAdenomas = nrow(.)))
    
    MyColonDataHGD_Adenomas <-
      full_join(
        MyColonData_HG_AdenomaDetectionByEndoscopist,
        MyColonDataColonoscopiesByEndoscopist,
        by = Endo_Endoscopist
      )
    MyColonDataHGD_Adenomas$PropHGAdenomas <-
      (
        MyColonDataHGD_Adenomas$NumHighGradeAdenomas /
          MyColonDataHGD_Adenomas$NumColons
      ) * 100
    
    MyColonDataLGD_Adenomas <-
      full_join(
        MyColonData_LG_AdenomaDetectionByEndoscopist,
        MyColonDataColonoscopiesByEndoscopist,
        by = Endo_Endoscopist
      )
    MyColonDataLGD_Adenomas$PropLGAdenomas <-
      (
        MyColonDataLGD_Adenomas$NumLowGradeAdenomas /
          MyColonDataLGD_Adenomas$NumColons
      ) * 100
    
    MyColonData_Serr_AdenomaDetectionByEndoscopist <-
      dataframe[grepl(".*[Ss]errated.*", dataframe[, Histol]),]
    MyColonData_Serr_AdenomaDetectionByEndoscopist <-
      MyColonData_Serr_AdenomaDetectionByEndoscopist %>%
      group_by_(Endo_Endoscopist) %>%
      do(data.frame(NumSerrAdenomas = nrow(.)))
    
    MyColonDataSerr_Adenomas <-
      full_join(
        MyColonData_Serr_AdenomaDetectionByEndoscopist,
        MyColonDataColonoscopiesByEndoscopist,
        by = Endo_Endoscopist
      )
    MyColonDataSerr_Adenomas$PropSerrAdenomas <-
      (MyColonDataSerr_Adenomas$NumSerrAdenomas /
         MyColonDataSerr_Adenomas$NumColons) * 100
    
    # Hyperplastic detection rate by endoscopist (from whole dataset) ====
    MyColonDataHyperplasticDetectionByEndoscopist <-
      dataframe[grep(".*yperplastic.*", dataframe[, Dx]),] %>%
      group_by_(Endo_Endoscopist) %>%
      do(data.frame(NumHyperplastics = nrow(.)))
    
    MyColonDataColonoscopiesByEndoscopist <-
      dataframe %>% group_by_(Endo_Endoscopist) %>%
      do(data.frame(NumColons = nrow(.)))
    
    # Merge the two above by column to get proportion:
    MyColonDataHDR <-
      full_join(
        MyColonDataHyperplasticDetectionByEndoscopist,
        MyColonDataColonoscopiesByEndoscopist,
        by = Endo_Endoscopist
      )
    MyColonDataHDR$PropHyperplastic <-
      (MyColonDataHDR$NumHyperplastics / MyColonDataHDR$NumColons) * 100
    
    FinalTable <-
      full_join(MyColonDataADR, MyColonDataHDR, by = Endo_Endoscopist)
    FinalTable <-
      full_join(FinalTable, MyColonDataAdenocarcinomas, by = Endo_Endoscopist)
    FinalTable$HyperplasticToAdenomaRatio <-
      FinalTable$PropAdenomas / FinalTable$PropHyperplastic
    
    # Rename one column
    
    names(FinalTable)[names(FinalTable) == "NumColons.y"] <-
      "Colons(Count)"
    FinalTable<-data.frame(FinalTable)
    return(FinalTable)
  }

############# Endoscopist Quality ######

#' Determine overall number of procedures performed
#'
#' Determines the number of endoscopies done by an endoscopist
#' by type of endosopy and indication for a given timeframe
#' As per BSG recommendations for Upper GI minimum number of
#' gastroscopies in a year (although here the time frame is user
#' defined)
#' @param dataframe The dataframe
#' @param EndoscopistColumn The column containing the Endoscopists names
#' @param IndicationColumn The column containing the Indication for the examination
#' @keywords Withdrawal
#' @export
#' @examples Myendo<-Myendo[grepl('Gastroscopy',Myendo$ProcedurePerformed),]
#'  oo<-NumberPerformed(Myendo,'Endoscopist','Indications')
#'  rm(Myendo)

NumberPerformed <- function(dataframe, EndoscopistColumn, IndicationColumn) {
  dataframe<-data.frame(dataframe)
  NumByEndoscopist <- data.frame(table(dataframe[, EndoscopistColumn],
                                       dataframe[, IndicationColumn]))
  return(NumByEndoscopist)
}


############################# Coding ################################



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
#' @param EVENT the EVENT column
#' @keywords OPCS-4 codes extraction
#' @importFrom dplyr mutate case_when
#' @importFrom rlang sym
#' @importFrom stringr str_detect
#' @export
#' @examples # Need to run the HistolTypeSite and EndoscopyEvent functions first here
#' SelfOGD_Dunn$OPCS4w<-OPCS4Prep(SelfOGD_Dunn,"PROCEDUREPERFORMED","PathSite","EndoscopyEvent")


#Take the PathSite codes which should have been coded from PathSite using the HistolBiopsyIndex

# 
SelfOGD_Dunn<-read_excel("/home/rstudio/GenDev/DevFiles/EndoMineRFunctionDev/SelfOGD_Dunn.xlsx")
EndoscTree<-list("Patient Name","Date of Birth","General Practicioner","Hospital Number","Date of Procedure","Endoscopist","Second endoscopist","Trainee","Referring Physician","Nurses","Medications","Instrument","Extent of Exam","Complications","Co-morbidity","INDICATIONS FOR EXAMINATION","PROCEDURE PERFORMED","FINDINGS","ENDOSCOPIC DIAGNOSIS","RECOMMENDATIONS","COMMENTS","FOLLOW UP","OPCS4 Code")

SelfOGD_Dunn<-SelfOGD_Dunn%>%select(PatientID,Endo_ResultText,Histo_ResultText)
for(i in 1:(length(EndoscTree)-1)) {SelfOGD_Dunn<-Extractor2(SelfOGD_Dunn,'Endo_ResultText',as.character(EndoscTree[i]),as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))}
PathTree<-c("NATURE OF SPECIMEN","CLINICAL DETAILS","MACROSCOPICAL DESCRIPTION","HISTOLOGY","DIAGNOSIS")
for(i in 1:(length(PathTree)-1)) {SelfOGD_Dunn<-Extractor2(SelfOGD_Dunn,'Histo_ResultText',as.character(PathTree[i]),as.character(PathTree[i+1]),as.character(PathTree[i]))}

SelfOGD_Dunn$PathSite<-HistolTypeAndSite(SelfOGD_Dunn,"PROCEDUREPERFORMED","Histo_ResultText","MACROSCOPICALDESCRIPTION")
SelfOGD_Dunn$PathSiteIndex<-HistolBiopsyIndex(SelfOGD_Dunn,"PathSite")

SelfOGD_Dunn$EndoscopyEvent<-EndoscopyEvent(SelfOGD_Dunn,"FINDINGS")


#ForRules<-SelfOGD_Dunn%>%filter(grepl("Gastroscopy",PROCEDUREPERFORMED),grepl("OESOPHAGUS: Barrett's oesophagus C0 M1 ",FINDINGS))
#EndoscopyEvent(ForRules,"FINDINGS")

#ForRules<-SelfOGD_Dunn%>%filter(grepl("Gastroscopy",PROCEDUREPERFORMED))%>%select(INDICATIONSFOREXAMINATION,ExtentofExam,Histo_ResultText,FINDINGS,EndoscopyEvent,PathSite,PathSiteIndex)
#View(ForRules)
SelfOGD_Dunn<-OPCS4Prep(SelfOGD_Dunn,"PROCEDUREPERFORMED","PathSite","EndoscopyEvent")
write_xlsx(SelfOGD_Dunn, "/home/rstudio/EndoscopyEventToValidate.xlsx")



#Checking against actual coding data
ManualOPCS_4<-read_excel("/home/rstudio/GenDev/DevFiles/EndoMineRFunctionDev/TB_ALLPATID_Dunn_2013ToPresent.xls")
library(janitor)
selectedClean<-ManualOPCS_4%>%select("Prim Proc Code & Description","2nd Proc Code","Trust ID",
                                     "Consultant","Admission Date","Prim Diag Code & Description",
                                     "2nd Diagnosis Code",
                                     "3rd Diagnosis Code",
                                     "5th Diagnosis Code",
                                     "6th Diagnosis Code",
                                     "7th Diagnosis Code",
                                     "8th Diagnosis Code",
                                     "9th Diagnosis Code",
                                     "10th Diagnosis Code")
selectedClean<-clean_names(selectedClean,"snake")
names(selectedClean) <- gsub('admission_date', 'Endo_ResultEntered', names(selectedClean))
names(selectedClean) <- gsub('trust_id', 'PatientID', names(selectedClean))

#convert the date column names so can do a merge
SelfOGD_Dunn$Endo_ResultEntered<-as.Date(SelfOGD_Dunn$Endo_ResultEntered)
selectedClean$Endo_ResultEntered<-as.Date(selectedClean$Endo_ResultEntered)

mergedData <- merge(selectedClean,SelfOGD_Dunn,by=c("Endo_ResultEntered","PatientID"))


ForRules<-SelfOGD_Dunn%>%filter(grepl("Gastroscopy",PROCEDUREPERFORMED))%>%select(ExtentofExam,Histo_ResultText,FINDINGS,EndoscopyEvent,prim_proc_code_description,x2nd_proc_code,PathSite,PathSiteIndex)%>%sample_n(100)
View(ForRules)
ToSee<-ForRules%>%select(FINDINGS,EndoscopyEvent,PathSite)%>% filter(grepl("Error", EndoscopyEvent))


#For each event site:

OPCS4Prep <- function(dataframe, Procedure,PathSite,Event) {  
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
#' @param EVENT the EVENT column
#' @keywords OPCS-4 codes extraction
#' @importFrom dplyr mutate case_when
#' @importFrom rlang sym
#' @importFrom stringr str_detect
#' @export
#' @examples # Need to run the HistolTypeSite and EndoscopyEvent functions first here
#' SelfOGD_Dunn$OPCS4w<-OPCS4Prep(SelfOGD_Dunn,"PROCEDUREPERFORMED","PathSite","EndoscopyEvent")



#The plan: 
#1. Look at th ENDOSCOPICDIAGNOSIS column first and implement algorithm to look for keywords that map to diagnostic codes
#2. Need to establish an order of importance of diagnostic codes.
#3. Map all the diagnoses from the drop down box to primary diagnosis codes
EndoscopicICD10 <- function(dataframe, Procedure,PathSite,FINDINGS,ENDOSCOPICDIAGNOSIS,EndoscopyEvent) { 
  
  
  #Just get the uppers for now:
  SelfOGD_DunnOGD<-SelfOGD_Dunn[grepl("Gastroscopy",SelfOGD_Dunn$PROCEDUREPERFORMED),]
  
  #Merge the findings in with the myDx:
  SelfOGD_DunnOGD$FINDINGSmyDx<-paste(SelfOGD_DunnOGD$FINDINGS,SelfOGD_DunnOGD$ENDOSCOPICDIAGNOSIS)
  
  #Split into a string
  SelfOGD_DunnOGD$FINDINGSmyDx<-stringi::stri_split_lines(SelfOGD_DunnOGD$FINDINGSmyDx,omit_empty = TRUE)
  
  #Copy over to a new column to be sampled
  SelfOGD_DunnOGD$FindingsAfterProcessing<-SelfOGD_DunnOGD$FINDINGSmyDx
  
  
  #Use case when on nested list to generate ICD-10 codes and then remove from the list.
  
  #If it matches the grep then add the diagnosis, and then chop into list and remove that string
  
 
  
  

  SelfOGD_DunnOGD<-SelfOGD_DunnOGD %>%
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
  
  SelfOGD_DunnOGD$FINDINGSmyDx<-lapply(SelfOGD_DunnOGD$FINDINGSmyDx, function(x) x[!grepl("OESOPH.*","", x,ignore.case=TRUE,perl=TRUE)])
  
  SelfOGD_DunnOGD<-SelfOGD_DunnOGD %>%  FindingsAfterProcessing = map(
    FindingsAfterProcessing, ~ .x[!grepl("(mitotic|emr)", tolower(.x),ignore.case=TRUE,perl=TRUE)]
  )
  
  
  
  
  ######### Go through it again for the second codes:
  
  SelfOGD_DunnOGD<-SelfOGD_DunnOGD %>%
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
          grepl("(?=[^\\.]*(oesophageal))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K221  -  Oesophageal ulcer",
          grepl("(?=[^\\.]*(gastric|stomach|pylor))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K25  -  Gastric ulcer",
          grepl("gastritis",tolower(.x),ignore.case=TRUE) ~  "K297  -  Gastritis - unspecified",
          grepl("duodenitis",tolower(.x),ignore.case=TRUE) ~  "K298  -  Duodenitis",
          grepl("(?=[^\\.]*(gastric))(?=[^\\.]*polyp)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K317  -  Polyp of stomach and duodenum",
          grepl("candid",tolower(.x),ignore.case=TRUE) ~  "B387  -  Candidiasis of other sites",
          TRUE ~ "")
      ),
      FindingsAfterProcessing = map(
        FindingsAfterProcessing, ~ .x[!grepl("(mitotic|emr|tumour|dysplasia|hiatus|stricture|barrett|inlet patch|
                                         hiatus|esophagitis|duodenitis|gastritis)|(?:(?=[^\\.]*(duodenal|oesophageal|gastric|stomach|pylor))(?=[^\\.]*ulcer)[^\\.]*(\\.|$))|
                                             (?:(?=[^\\.]*(gastric))(?=[^\\.]*polyp)[^\\.]*(\\.|$))|(candid)", tolower(.x),ignore.case=TRUE,perl=TRUE)]
      )
    )
  
  
  SelfOGD_DunnOGD$OverallDiagnosisCode<-lapply(SelfOGD_DunnOGD$OverallDiagnosisCode,function(x) (unique(x)))
  SelfOGD_DunnOGD$OverallDiagnosisCode<-lapply(SelfOGD_DunnOGD$OverallDiagnosisCode, function(x) x[x != "" & x != "\n"])
  
  
  #Now go through the OverallCodes and determine the 
  
  
  ######### Go through it again for the third codes: 
  
  SelfOGD_DunnOGD<-SelfOGD_DunnOGD %>%
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
          grepl("(?=[^\\.]*(oesophageal))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K221  -  Oesophageal ulcer",
          grepl("(?=[^\\.]*(gastric|stomach|pylor))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K25  -  Gastric ulcer",
          grepl("gastritis",tolower(.x),ignore.case=TRUE) ~  "K297  -  Gastritis - unspecified",
          grepl("duodenitis",tolower(.x),ignore.case=TRUE) ~  "K298  -  Duodenitis",
          grepl("(?=[^\\.]*(gastric))(?=[^\\.]*polyp)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K317  -  Polyp of stomach and duodenum",
          grepl("candid",tolower(.x),ignore.case=TRUE) ~  "B387  -  Candidiasis of other sites",
          TRUE ~ "")
      ),
      FindingsAfterProcessing = map(
        FindingsAfterProcessing, ~ .x[!grepl("(mitotic|emr|tumour|dysplasia|hiatus|stricture|barrett|inlet patch|
                                         hiatus|esophagitis|duodenitis|gastritis)|(?:(?=[^\\.]*(duodenal|oesophageal|gastric|stomach|pylor))(?=[^\\.]*ulcer)[^\\.]*(\\.|$))|
                                             (?:(?=[^\\.]*(gastric))(?=[^\\.]*polyp)[^\\.]*(\\.|$))|(candid)", tolower(.x),ignore.case=TRUE,perl=TRUE)]
      )
    )
  
  #To Do: Also assess the findings column
  
  
  
  return(primDxVector)
}