


if (getRversion() >= "2.15.1")
  utils::globalVariables(
    c(
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
      "origin"
    )
  )

######## Surveillance functions ######

#' Extract the time difference between each test in days
#'
#' This determines the time difference between each test for a patient in days.
#'
#' @param x dataframe,
#' @param HospNum_Id Patient ID
#' @param Endo_ResultPerformed Date of the Endoscopy
#' @importFrom dplyr arrange group_by mutate lead
#' @importFrom magrittr '%>%'
#' @importFrom rlang sym
#' @keywords Surveillance
#' @export
#' @examples em<-SurveillanceTimeByRow(Myendo,'HospitalNumber',
#' 'Dateofprocedure')

SurveillanceTimeByRow <-
  function(x, HospNum_Id, Endo_ResultPerformed) {
    HospNum_Ida <- sym(HospNum_Id)
    Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
    x %>% arrange(!!HospNum_Ida, !!Endo_ResultPerformeda) %>% 
      group_by(!!HospNum_Ida) %>% 
      mutate(diffDate = difftime(as.Date(!!Endo_ResultPerformeda), lead(as.Date(
      !!Endo_ResultPerformeda
    ), 1), units = "days"))
  }

#' Extract the last test done by a patient in days and how long ago
#' 
#' This determines the last test done by that patient and the time
#' between now and that last test in days
#'
#' @param x dataframe
#' @param  HospNum_Id Patient ID
#' @param Endo_ResultPerformed Date of the Endoscopy
#' @importFrom magrittr '%>%'
#' @importFrom dplyr arrange group_by mutate lead
#' @importFrom rlang sym
#' @keywords Surveillance
#' @export
#' @examples em<-SurveillanceLastToNow(Myendo,'HospitalNumber',
#' 'Dateofprocedure')

SurveillanceLastToNow <-
  function(x, HospNum_Id, Endo_ResultPerformed) {
    HospNum_Ida <- sym(HospNum_Id)
    Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
    
    x %>% arrange(!!HospNum_Ida, !!Endo_ResultPerformeda) %>%
      group_by(!!HospNum_Ida) %>%
      mutate(diffDate = difftime(Sys.Date(), last(!!Endo_ResultPerformeda), 
                                 units = "days"))
  }


#' Extract the last test done by a patient only
#'
#' Extracts the last test only per patient
#' @param x dataframe
#' @param HospNum_Id Patient ID
#' @param Endo_ResultPerformed Date of the Endoscopy
#' @importFrom magrittr '%>%'
#' @importFrom dplyr arrange group_by filter row_number
#' @importFrom rlang sym
#' @keywords Surveillance
#' @export
#' @examples em<-SurveillanceLastTest(Myendo,'HospitalNumber','Dateofprocedure')



SurveillanceLastTest <-
  function(x, HospNum_Id, Endo_ResultPerformed) {
    HospNum_Ida <- sym(HospNum_Id)
    Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
    x %>% group_by(!!HospNum_Ida) %>%
      arrange(!!Endo_ResultPerformeda) %>%
      filter(row_number() == n())
  }


#' Extracts the first test only per patient
#'
#' Extracts the first test only per patient
#' @param x dataframe
#' @param HospNum_Id Patient ID
#' @param Endo_ResultPerformed Date of the Endoscopy
#' @importFrom dplyr arrange group_by filter
#' @importFrom magrittr '%>%'
#' @importFrom rlang sym
#' @keywords Surveillance
#' @export
#' @examples em<-SurveillanceFirstTest(Myendo,'HospitalNumber',
#' 'Dateofprocedure')


SurveillanceFirstTest <-
  function(x, HospNum_Id, Endo_ResultPerformed) {
    HospNum_Ida <- sym(HospNum_Id)
    Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
    x %>% group_by(!!HospNum_Ida) %>% arrange(!!Endo_ResultPerformeda) %>%
      filter(row_number() == 1)
  }



#' Number of tests done per month
#'
#' This determines the number of tests done per month
#' @param x dataframe
#' @param Endo_ResultPerformed Column with the date the Endoscopy was performed
#' @importFrom dplyr group_by summarise
#' @importFrom lubridate dmy month
#' @importFrom magrittr '%>%'
#' @importFrom rlang sym
#' @keywords cats
#' @export
#' @examples em<-SurveillanceCapacity(Myendo,'Dateofprocedure')

SurveillanceCapacity <- function(x, Endo_ResultPerformed) {
  Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
  x %>% mutate(month = format(as.Date(!!Endo_ResultPerformeda), "%m")) %>%
    group_by(month) %>% summarise(n = n())
}

#' Number of tests done per month and year by indication
#' 
#' Get an overall idea of how many endoscopies have been done for an indication 
#' by year and month. This is a more involved version of 
#' SurveillanceCapacity function. It takes string for 
#' the Indication for the test
#'
#' This returns a list which contains a plot (number of tests for that 
#' indication over time and a table with the same information broken down 
#' by month and year).
#' @param x dataframe
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
#' @keywords Tests number
#' @export
#' @examples # This takes the dataframe MyEndo (part of the package examples) and looks in 
#' # the column which holds the test indication (in this example it is called 
#' # 'Indication' The date of the procedure column(which can be date format or 
#' # POSIX format) is also necessary.  Finally the string which indicates the text 
#' # indication needs to be inpoutted. In this case we are looking for all 
#' # endoscopies done
#' # where the indication is surveillance (so grepping on 'Surv' will do fine) .
#' how<-HowManyTests(Myendo,'Indications','Dateofprocedure','Surv')


HowManyTests <-
  function(x,
           Indication,
           Endo_ResultPerformed,
           StringToSearch) {
    Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
    TestNumbers <-
      x %>% filter(str_detect(x[, Indication], StringToSearch)) %>%
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
    # # Then just plot it:
    
    
    Myplot <-
      ggplot(data = TestNumbers, aes(x = MonthYear, y = freq)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "loess") +
      scale_x_date(
        labels = function(x)
          format(x, "%d-%b")
      ) + theme_bw()
    functionResults <-
      list(Myplot = Myplot, TestNumbers = TestNumbers)
    return(functionResults)
  }




########################################## Patient flow functions#######

#' Create a Sankey plot for patient flow
#'
#' This creates a Sankey plot to see the order of tests for all patients:
#' dfw is the dataframe, y is the value of in this case
#' the procedure type (eg EMR,
#'  radiofrequency ablation for Barrett's but can be
#'  any description of a procedure you desire)
#'  Note the Hospital Number column MUST be called PatientID.
#' @param dfw the dataframe extracted using the standard cleanup scripts
#' @param y the column containing the test like ProcPerformed for example
#' @param PatientID the column containing the patients unique identifier 
#' eg hostpital number
#' @importFrom dplyr group_by
#' @importFrom magrittr '%>%'
#' @import data.table
#' @importfrom reshape2 'dcast'
#' @importFrom googleVis gvisSankey
#' @keywords Sankey
#' @export
#' @examples # The purpose of the function is to 
#' # provide a Sankey plot which allows the analyst to see the proportion 
#' # of patients moving from one state (in this case type of Procedure) to 
#' # another. This allows us to see for example how many EMRs are done after 
#' #RFA. For further patient flow examples see PatientFlow_CircosPlots
#' names(Myendo)[names(Myendo) == 'HospitalNumber'] <- 'PatientID'
#' SurveySankey(Myendo,"ProcedurePerformed","PatientID")

SurveySankey <- function(dfw, y,PatientID) {
  # Create the Sankey diagrams
  Sankey <-
    dcast(setDT(dfw)[, .SD, PatientID], 
          PatientID ~ rowid(PatientID),
          value.var = y)
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
                  grepl("[A-Z]", orders.plot$to), ]
  orders.plot <-
    orders.plot[!grepl("NA", orders.plot$from) &
                  !grepl("NA", orders.plot$to), ]
  plot(googleVis::gvisSankey(
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
    ))
}


#' Create a Circos plot for patient flow
#'
#' This allows us to look at the overall flow from one
#' type of procedure to another using circos plots.
#' @param x dataframe
#' @param Endo_ResultPerformed the column containing the date of the procedure
#' @param ProcPerformed The procedure that you want to plot (eg EMR,
#'  radiofrequency ablation for Barrett's but can be
#'  any dscription of a procedure you desire)
#' @param HospNum_Id Column with the patient's unique hospital number
#' @importFrom dplyr arrange group_by mutate select summarise lag ungroup rename
#' @importFrom reshape2 'dcast'
#' @importFrom tidyr separate
#' @import circlize
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
#' PatientFlow_CircosPlots(Myendo,"Dateofprocedure","PatientID","EndoEvent")
#' rm(Myendo)
#' rm(EndoEvent)


PatientFlow_CircosPlots <-
  function(x,
           Endo_ResultPerformed,
           HospNum_Id,
           ProcPerformed) {
    
    Endo_ResultPerformeda <- sym(Endo_ResultPerformed)
    HospNum_Ida <- sym(HospNum_Id)
    ProcPerformeda <- sym(ProcPerformed)
    
    mydf <-
      x %>% arrange(!!Endo_ResultPerformeda) %>% group_by(!!HospNum_Ida) %>%
      mutate(origin = lag(!!ProcPerformeda, 1),
              destination = !!ProcPerformeda) %>%
      select(origin, destination,PatientID) %>% 
      group_by(origin, destination,PatientID) %>%
      summarise(n = n()) %>% ungroup()
    
    mydf <- data.frame(dcast(mydf, origin ~ destination))
    
    # Get rid of NA's
    mydf <- mydf[complete.cases(mydf), ]
    
    V1 <- c("2", "7", "3", "10")
    V2 <- c("210,150,12", "110,255,233", "125,175,0", "255,219,0")
    
    mydf <- cbind(V1, V2, mydf)
    
    df_format <-
      mydf %>% select(1:3) %>% rename(order = V1,
                                      rgb = V2,
                                      region = origin) %>% 
      mutate(region = gsub("_", " ", region))
    # flow matrix. Need to add V1 and V2 to the matrix here
    
    matmydf <- as.matrix(mydf[, -(1:3)])
    dimnames(matmydf) <-
      list(orig = df_format$region, dest = df_format$region)
    # library('tidyr')
    df_format <-
      df_format %>% dplyr::arrange(order) %>% 
      separate(rgb, c("r", "g", "b")) %>%
      mutate(col = rgb(r, g, b, max = 255),
             max = rowSums(matmydf) + colSums(matmydf))
    
    
    # library('circlize')
    circos.clear()
    par(mar = rep(0, 4), cex = 0.9)
    circos.par(start.degree = 90, gap.degree = 4)
    par(cex = 0.8, mar = c(0, 0, 0, 0))
    chordDiagram(
      x = matmydf,
      directional = 1,
      order = df_format$region,
      grid.col = df_format$col,
      annotationTrack = "grid",
      transparency = 0.25,
      annotationTrackHeight = c(0.1, 0.1),
      diffHeight = -0.04
    )
    
    
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
      xlim <- get.cell.meta.data("xlim")
      ylim <- get.cell.meta.data("ylim")
      sector.index <- get.cell.meta.data("sector.index")
      circos.text(mean(xlim), mean(ylim), 
                  sector.index, col = "black", 
                  cex = 0.6, facing = "inside", 
                  niceFacing = TRUE)
    }, 
    bg.border = NA)
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
#' @param y the column of interest,
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
#' tt<-ListLookup(Myendo,'Findings',myNotableWords)

ListLookup <- function(theframe, y, myNotableWords) {
  jeopCorpus <- Corpus(VectorSource(theframe[, y]))
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
    sapply(myNotableWords, function(x)
      sum(d$Prop[grepl(x, d$word)]))
  d <- data.frame(X2 = names(d), Prop = as.vector(d))
  return(d)
}


############## Pathology Quality #####

# Groups anything by Endoscopist and returns the table and a ggplot


#' Plot a metric by endoscopist
#'
#' This takes any numerical metric in the dataset and plots it by endoscopist.
#' It of course relies on a Endoscopist column being present
#' @param x The dataframe
#' @param y The column (numeric data) of interest
#' @param z The endoscopist column
#' @importFrom ggplot2 ggplot
#' @importFrom tidyr drop_na
#' @importFrom dplyr group_by summarise
#' @importFrom rlang sym
#' @keywords Endoscopist
#' @export
#' @examples #The function plots any numeric metric by endoscopist
#' # and also gives a table with it. In this example we plot medication by
#' # endoscopist
#' Myendo<-EndoscChopperMeds(Myendo,'Medications')
#' Fent<-MetricByEndoscopist(Myendo,'Endoscopist','Fent')
#' rm(Myendo)


MetricByEndoscopist <- function(x, y, z) {
  group <- sym(y)
  variable <- sym(z)
  
  NumBxPlot <-
    x %>% tidyr::drop_na(!!variable) %>% group_by(!!group) %>% 
    summarise(avg = mean(!!variable))
  
  
  ggplot(NumBxPlot) + geom_point(aes(x = Endoscopist, y = avg),
                     colour = "red",
                     size = 3) + labs(title = "Average  by endoscopist") +
    theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    theme(axis.text.x = element_text(angle = -90)) +
    theme(axis.text.y = element_text(angle = -90)) +
    theme(legend.position = "top")
}



#' Standardise location of biopsies or tissue samples
#'
#' Standardises the location of biopsies by cleaning up the common typos and
#' abbreviations that are commonly used in free text of pathology reports
#'
#' @param x The dataframe
#' @param SampleLocation Column describing the Macroscopic sample from histology
#' @keywords Withdrawal
#' @export
#' @examples #Firstly we extract histology from the raw report
#' # using the extractor function
#' Histoltree <-list("Hospital Number:","Patient Name:",
#' "General Practitioner:","Date received:","Clinical Details",
#'  "Nature of specimen","Histology","Diagnosis",""
#')
#'
#' for (i in 1:(length(Histoltree) - 1)) {
#'  Mypath <-Extractor(
#'      Mypath, "PathReportWhole",as.character(Histoltree[i]),
#'      as.character(Histoltree[i + 1]),
#'      gsub(" ", "", as.character(Histoltree[i]))
#'    )
#' }
#' names(Mypath)[names(Mypath) == 'Datereceived'] <- 'Dateofprocedure'
#' Mypath$Dateofprocedure <- as.Date(Mypath$Dateofprocedure)
#' # The function then standardises the histology terms through a series of 
#' # regular expressions
#' f<-TermStandardLocation(Mypath,'Histology')
#' rm(Mypath)



TermStandardLocation <- function(x, SampleLocation) {
  x$SampleLocation <- tolower(x[, SampleLocation])
  x$SampleLocation <-
    gsub(
      "[Rr][Ii][Gg][Hh][Tt]|($| )[Rr] |[Aa]sce |[Aa]scending|[Aa]scend[^a-z]|
      [Cc]olon [Rr]|[Rr] [Cc]olon|[Aa]sc ",
      "Ascending ",
      x$SampleLocation
    )
  x$SampleLocation <-
    gsub(
      "[Ll][Ee][Ff][Tt]|lt |[Dd]escending|[Dd]escen[^a-z]|
      [Dd]esc[^a-z]|[Dd]es[^a-z]|[Cc]olon [Ll]|[Ll] [Cc]olon",
      "Descending ",
      x$SampleLocation
    )
  x$SampleLocation <-
    gsub("[Ss]igmoid|[Ss]igm[^a-z]|[Ss]igmo ",
         "Sigmoid ",
         x$SampleLocation)
  x$SampleLocation <-
    gsub("[Rr]ectal|[Rr]ectum|[Rr]ectum[a-z]|[Rr]ect ",
         "Rectum ",
         x$SampleLocation)
  x$SampleLocation <-
    gsub(
      "[Tt]ransverse|[Tt]ransv[^a-z]|[Tt]ranv |[Tt]rans ",
      "Transverse ",
      x$SampleLocation
    )
  x$SampleLocation <-
    gsub("[Cc]aecum|[Cc]aecal", "Caecum ", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Ss]plenic", "Splenic ", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Ii]leum|[Ii]leal", "Ileum ", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Rr]ectosigmoid", "Rectosigmoid ", x$SampleLocation)
  x$SampleLocation <-
    gsub(
      "[Ii]leocaecal\\s*|[Ii][Cc][Vv]|[Ii]leo-[Cc]aecum",
      "Ileocaecal ",
      x$SampleLocation
    )
  x$SampleLocation <-
    gsub("[Hh]ep[^a-z]|[Hh]epatic", "Hepatic ", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Cc]olonic|[Cc]olon |[Cc]ol[^a-z]",
         "Colon ",
         x$SampleLocation)
  x$SampleLocation <-
    gsub("[Tt]erm |[Tt]erminal", "Terminal ", x$SampleLocation)
  x$SampleLocation <-
    gsub("TI", "Terminal Ileum ", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Cc]aec ", "Caecum ", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Ss]ig ", "Sigmoid ", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Ii]leo\\s*-\\s*[Aa]nal ", "Ileoanal ", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Ii]leo\\s*[Aa]nal ", "Ileoanal ", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Pp]re\\s*pouch", "PrePouch ", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Pp]re-[Pp]ouch", "PrePouch ", x$SampleLocation)
  x$SampleLocation <- gsub("pouch", "Pouch ", x$SampleLocation)
  x$SampleLocation <-
    gsub("IleoAnal([a-zA-Z]+)", "Ileoanal \\1 ", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Aa]nastomosis", "Anastomosis", x$SampleLocation)
  x$SampleLocation <- gsub("[Xx]\\s*[1-9]|", "", x$SampleLocation)
  x$SampleLocation <- gsub("[1-9]\\s*[Xx]|", "", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Hh]yperplastic", "", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Dd]istal|[Pp]roximal|[Pp]rox ", "", x$SampleLocation)
  x$SampleLocation <- gsub("[Ss]essile", "", x$SampleLocation)
  x$SampleLocation <-
    gsub("\\d+[Mm]{2}|\\d+[Cc][Mm]", "", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Pp]edunculated|[Pp]seudo", "", x$SampleLocation)
  x$SampleLocation <- gsub("\\d", "", x$SampleLocation)
  x$SampleLocation <- gsub("  ", " ", x$SampleLocation)
  x$SampleLocation <- gsub(":", "", x$SampleLocation)
  # For upper GI
  x$SampleLocation <-
    gsub("[Gg]astric|[Ss]tomach", "Stomach", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Aa]ntrum|[Aa]ntral", "Antrum", x$SampleLocation)
  x$SampleLocation <-
    gsub("[Dd]uodenum|[Dd]2|[Dd]uodenal",
         "Duodenum",
         x$SampleLocation)
  x$SampleLocation <-
    gsub("[Oo]esophageal|[Oo]esophagus|esophag[^a-z]",
         "Oesophagus",
         x$SampleLocation)
  x$SampleLocation <-
    gsub("[Gg][Oo][Jj]|[Gg]astro[Oo]esophageal",
         "GOJ",
         x$SampleLocation)
  x$SampleLocation <-
    gsub("[Ff]undal|[Ff]undic|[Ff]undus", "GOJ", x$SampleLocation)
  return(x)
}


#' Locate where samples are taken from
#'
#' This assess where samples are taken from.
#' This should be used after the TermStandardLocation
#' as it relies on the presence of a SampleLocation column
#' which the TermStandardLocation produces.
#' It can be used after the PolypTidyUpLocator
#' although you could just run it after the TermStandardLocation
#' to get an overview of where samples were taken from.
#' It will tell you the sites sampled without duplication
#' @param x The dataframe
#' @param y SampleLocation from the TermStandardizer
#' @importFrom stringr str_match_all
#' @keywords Sample location
#' @export
#' @examples #Firstly we extract histology from the raw report
#' # using the extractor function
#' Histoltree <-list("Hospital Number:","Patient Name:",
#' "General Practitioner:","Date received:","Clinical Details",
#'  "Nature of specimen","Histology","Diagnosis",""
#')
#'
#' for (i in 1:(length(Histoltree) - 1)) {
#'  Mypath <-Extractor(
#'      Mypath, "PathReportWhole",as.character(Histoltree[i]),
#'      as.character(Histoltree[i + 1]),
#'      gsub(" ", "", as.character(Histoltree[i]))
#'    )
#' }
#' names(Mypath)[names(Mypath) == 'Datereceived'] <- 'Dateofprocedure'
#' Mypath$Dateofprocedure <- as.Date(Mypath$Dateofprocedure)
#' # The function needs all the terms to be standardised first so the 
#' # function TermStandardLocation needs to be run first
#' f<-TermStandardLocation(Mypath,'Histology')
#' # The sample locator then determines where the biopsies were taken from
#' # by assessing the SampleLocation column which comes from the
#' # TermStandardLocation function.
#' f<-SampleLocator(f,'SampleLocation')
#' rm(Mypath)

SampleLocator <- function(x, y) {
  x <- data.frame(x)
  tofind <-
    paste(
      c(
        "Ascending",
        "Descending",
        "Sigmoid",
        "Rectum",
        "Transverse",
        "Caecum",
        "Splenic",
        "Ileum",
        "Rectosigmoid",
        "Ileocaecal",
        "Hepatic",
        "Colon",
        "Terminal",
        "Terminal Ileum",
        "Ileoanal",
        "Prepouch",
        "Pouch",
        "Anastomosis",
        "Stomach",
        "Antrum",
        "Duodenum",
        "Oesophagus",
        "GOJ"
      ),
      collapse = "|"
    )
  x$AllSampleLocator <- str_match_all(x[, y], tofind)
  x$AllSampleLocator <-
    lapply(x$AllSampleLocator, function(p)
      unique(p))
  return(x)
}


#' Determine polyp location
#'
#' This should be used after the TermStandardizer
#' as it relies on the presence of a SampleLocation column
#' which the TermStandardLocation produces.
#' It should be used after the PolypTidyUpLocator
#' @param x The dataframe
#' @param y The column containing the SampleLocation from the 
#' TermStandardLocation
#' @keywords Withdrawal
#' @importFrom stringr str_match_all
#' @export
#' @examples Histoltree <-list("Hospital Number:","Patient Name:",
#' "General Practitioner:","Date received:","Clinical Details",
#'  "Nature of specimen","Histology","Diagnosis",""
#')
#'
#' for (i in 1:(length(Histoltree) - 1)) {
#'  Mypath <-Extractor(
#'      Mypath, "PathReportWhole",as.character(Histoltree[i]),
#'      as.character(Histoltree[i + 1]),
#'      gsub(" ", "", as.character(Histoltree[i]))
#'    )
#' }
#' names(Mypath)[names(Mypath) == 'Datereceived'] <- 'Dateofprocedure'
#' Mypath$Dateofprocedure <- as.Date(Mypath$Dateofprocedure)
#' # The polyp locator then determines where the biopsies were taken from
#' # by assessing the SampleLocation column which comes from the
#' # TermStandardLocation function.
#' f<-TermStandardLocation(Mypath,'Histology')
#' f<-PolypLocator(f,'SampleLocation')
#' rm(Mypath)



PolypLocator <- function(x, y) {
  x <- data.frame(x)
  tofind <-
    paste(
      c(
        "Ascending",
        "Descending",
        "Sigmoid",
        "Rectum",
        "Transverse",
        "Caecum",
        "Splenic",
        "Ileum",
        "Rectosigmoid",
        "Ileocaecal",
        "Hepatic",
        "Colon",
        "Terminal",
        "Terminal Ileum",
        "Ileoanal",
        "Prepouch",
        "Pouch",
        "Anastomosis",
        "Stomach",
        "Antrum",
        "Duodenum",
        "Oesophagus",
        "GOJ"
      ),
      collapse = "|"
    )
  x$PolypLocator <- str_match_all(x[, y], tofind)
  x$PolypLocator <- lapply(x$PolypLocator, function(p)
    unique(p))
  return(x)
}




#' Clean the polyp location
#'
#' This cleans up the polyps from the TermStandardLocation function
#' @param x The dataframe
#' @param SampleLocation The column containing the
#' SampleLocation from the Term Standardizer
#' @keywords Withdrawal
#' @export
#' @examples #' # The polyp locator then determines where the biopsies were taken from
#' # by assessing the SampleLocation column which comes from the
#' # TermStandardLocation function. This should also be run before the polyp 
#' # locator function
#' f<-TermStandardLocation(Mypath,'Histology')
#' f<-PolypTidyUpLocator(f,'SampleLocation')

PolypTidyUpLocator <- function(x, SampleLocation) {
  # Get all the polyps and tidy up the polyp data- Function 5
  x$Polyp <-
    str_match_all(x[, SampleLocation], ".*[Pp]olyp.*")
  x <- PolypLocator(x, "Polyp")
  
  return(x)
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
#' @param x The dataframe
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
#' EndoscTree <-
  #'   list("Hospital Number:","Patient Name:","General Practitioner:",
#'        "Date of procedure:","Endoscopist:","Second endoscopist:","Medications",
#'        "Instrument","Extent of Exam:","Indications:","Procedure Performed:",
#'        "Findings:","Endoscopic Diagnosis:")
#' for (i in 1:(length(EndoscTree) - 1)) {
  #'   MyendoColon <-
    #'     Extractor(MyendoColon,"OGDReportWhole",  as.character(EndoscTree[i]),
#'               as.character(EndoscTree[i + 1]),as.character(EndoscTree[i])
#'     )}
#' Histoltree <-
  #'   list(
    #'     "Hospital Number:","Patient Name:","General Practitioner:",
    #'     "Date received:","Clinical Details","Nature of specimen","Histology",
    #'     "Diagnosis","")
#' for (i in 1:(length(Histoltree) - 1)) {
  #' MypathColon <-Extractor(MypathColon,"PathReportWhole",  
#'                           as.character(Histoltree[i]),
#'                           as.character(Histoltree[i + 1]),
#'                           gsub(" ", "", as.character(Histoltree[i]))
#'   )}
#' names(MypathColon)[names(MypathColon) == 'Datereceived'] <- 'Dateofprocedure'
#' MypathColon$Dateofprocedure <- as.Date(MypathColon$Dateofprocedure)
#' vColon <-Endomerge2(MypathColon, "Dateofprocedure","HospitalNumber",
#'                     MyendoColon, "Dateofprocedure","HospitalNumber")
#' GRSTable<-GRS_Type_Assess_By_Unit(vColon,'ProcedurePerformed', 
#' 'Endoscopist','Diagnosis','Histology')
#' rm(vColon)
#' rm(MypathColon)
#' rm(MyendoColon)

GRS_Type_Assess_By_Unit <-
  function(x,
           ProcPerformed,
           Endo_Endoscopist,
           Dx,
           Histol) {
    x <- data.frame(x)
    
    # Adenomas by endoscopist ============
    
    x <- x[grepl("Colonoscopy", x[, ProcPerformed]), ]
    MyColonDataAdenomaDetectionByEndoscopist <-
      x[grep(".*[Aa]denom.*", x[, Dx]), ]
    MyColonDataAdenomaDetectionByEndoscopist <-
      MyColonDataAdenomaDetectionByEndoscopist %>%
      group_by_(Endo_Endoscopist) %>%
      do(data.frame(NumAdenomas = nrow(.)))
    
    MyColonDataColonoscopiesByEndoscopist <-
      x %>% group_by_(Endo_Endoscopist) %>%
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
      x[grepl(".*denoca.*", x[, Histol]) &
          !grepl(".*denom.*", x[, Histol]), ]
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
      x[grepl(".*denoma.*", x[, Histol]) &
          grepl(".*[Hh]igh [Gg]rade.*", x[, Histol]), ]
    MyColonData_HG_AdenomaDetectionByEndoscopist <-
      MyColonData_HG_AdenomaDetectionByEndoscopist %>%
      group_by_(Endo_Endoscopist) %>%
      do(data.frame(NumHighGradeAdenomas = nrow(.)))
    
    MyColonData_LG_AdenomaDetectionByEndoscopist <-
      x[grepl(".*denoma.*", x[, Histol]) &
          grepl(".*[Ll]ow [Gg]rade.*", x[, Histol]), ]
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
      x[grepl(".*[Ss]errated.*", x[, Histol]), ]
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
      x[grep(".*yperplastic.*", x[, Dx]), ] %>% group_by_(Endo_Endoscopist) %>%
      do(data.frame(NumHyperplastics = nrow(.)))
    
    MyColonDataColonoscopiesByEndoscopist <-
      x %>% group_by_(Endo_Endoscopist) %>%
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
#' @param x The dataframe
#' @param y The column containing the Endoscopists names
#' @param z The column containing the Indication for the examination
#' @keywords Withdrawal
#' @export
#' @examples Myendo<-Myendo[grepl('Gastroscopy',Myendo$ProcedurePerformed),]
#'  NumberPerformed(Myendo,'Endoscopist','Indications')

NumberPerformed <- function(x, y, z) {
  NumByEndoscopist <- data.frame(table(x[, y], x[, z]))
  return(NumByEndoscopist)
}
