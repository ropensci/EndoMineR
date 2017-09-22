
##### Fake Data Creation ##### 

# Endoscopies() creates a spreadsheet of Endoscopy data,Histop_df() creates a dataframe of
# Histopathology data. Endomerge() associates them together
# EndoRaw() creates an Endoscopy report (so the data has not already been extracted) for upper GI
# ColonEndoRaw() creates an Endoscopy report (so the data has not already been extracted) for lower GI
# pathRep() creates an Pathology report (so the data has not already been extracted) for upper GI
# ColonpathRep() creates an Pathology report (so the data has not already been extracted) for upper GI

#' FakeEndoCreator
#'
#' Creates fake endoscopic data to play with as a spreadsheet format. It assumes
#' that some of the data has already been separated out. EndoRaw() for upper GI
#' and ColonEndoRaw() for lower GI are better functions for the real
#'  scenario of just getting the report as a series of unextracted text files.
#' (Their histology equivalents are pathRep and ColonpathRep respectively)
#' @param x None needed
#' @keywords Fake endoscopy
#' @export
#' @examples Endoscopies()

Endoscopies <- function(x) {
  Endoscopist <- list(x1 = "Dr Jonny Begood", x2 = "Dr Elvis Presley",
                      x3 = "Dr Bilbo Baggins", x4 = "Dr Elmo Fudd",
                      x5 = "Dr Jimminey Cricket", x6 = "Dr Davy Jones",
                      x7 = "Dr Bugs Bunny", x8 = "Dr Rara Rasputin",
                      x9 = "Dr Chubby Checker", x10 = "Dr Frank Sinatra",
                      x11 = "Dr Charles Dickens", x12 = "Dr Joseph Conrad",
                      x13 = "Dr Florence Nightingale", x14 = "Dr Sal Addin",
                      x15 = "Dr King Richard III")
  Midazolam <- list(x = "1mg", x = "2mg", x = "3mg",
                    x = "4mg", x = "5mg", x = "6mg", x = "7mg",
                    x = "8mg")
  Fentanyl <- list(x = "12.5mcg", x = "25mcg", x = "50mcg",
                   x = "75mcg", x = "100mcg", x = "125mcg", x = "150mcg")
  Indication <- list(x1 = "Therapeutic- Dilatation",
                     x2 = "Other-", x3 = "Follow-up ULCER HEALING",
                     x4 = "Haematemesis or Melaena/Blood PR", x5 = "Previous OGD ? 8 months ago",
                     x6 = "Dysphagia/Odynophagia", x7 = "Surveillance-Barrett's",
                     x8 = "Nausea and/or Vomiting", x9 = "Weight Loss",
                     x10 = "Dysphagia/intermittent for a few months",
                     x11 = "Other-", x12 = "Small Bowel Biopsy",
                     x13 = "Dyspepsia", x14 = "Reflux-like Symptoms/Atypical Chest Pain",
                     x15 = "chronic abdo pain and constipaton",
                     x16 = "Oesophagus- Dysplasia", x17 = "Therapeutic- RFA")
  Diagnosis <- list(x1 = "Ulcer- Oesophageal. ",
                    x2 = "Post chemo-radiotherapy stricture ",
                    x3 = "Possible achalasia.", x4 = "Oesophagitis. ",
                    x5 = "Food bolus obstructing the oesophagus.",
                    x6 = "Hiatus Hernia. ", x7 = "Extensive neoplastic looking esophageal lesion. ",
                    x8 = "Esophageal candidiasis ", x9 = "Barretts oesophagus. ",
                    x10 = "Gastritis")
  Endodat <- sample(seq(as.Date("2013/01/01"), as.Date("2017/05/01"),
                        by = "day"), 1000)
  EndoHospNum <- sample(c("P433224", "P633443", "K522332",
                          "G244224", "S553322", "D0739033", "U873352",
                          "P223333", "Y763634", "I927282", "P223311",
                          "P029834", "U22415", "U234252", "S141141",
                          "O349253", "T622722", "J322909", "F630230",
                          "T432452"), 1000, replace = TRUE)
  # Yes I know... This was just easier..
  BarrettsLength <- c("C0M1", "C0M2", "C0M3", "C0M4",
                      "C0M5", "C0M6", "C0M7", "C0M8", "C0M9", "C0M10",
                      "C1M2", "C1M3", "C1M4", "C1M5", "C1M6", "C1M7",
                      "C1M8", "C1M9", "C1M10", "C2M3", "C2M4", "C2M5",
                      "C2M6", "C2M7", "C2M8", "C2M9", "C2M10", "C3M4",
                      "C3M5", "C3M6", "C3M7", "C3M8", "C3M9", "C3M10",
                      "C4M5", "C4M6", "C4M7", "C4M8", "C4M9", "C4M10",
                      "C5M6", "C5M7", "C5M8", "C5M9", "C5M10", "C6M7",
                      "C6M8", "C6M9")
  # Merge them all together into a dataframe
  Endoscopies <- data.frame(EndoHospNum, replicate(1000,
                                                   paste("Date of Procedure", sample(Endodat,
                                                                                     1, replace = F), " Endoscopist: ", sample(Endoscopist,
                                                                                                                               1, replace = F), "Midazolam: ", sample(Midazolam,
                                                                                                                                                                      1, replace = F), "Fentanyl: ", sample(Fentanyl,
                                                                                                                                                                                                            1, replace = F), "Indication:", sample(Indication,
                                                                                                                                                                                                                                                   1, replace = F), "Diagnosis:", stringr::str_c(sample(Diagnosis,
                                                                                                                                                                                                                                                                                                        sample(1:10, 1), replace = F), collapse = "."),
                                                         sample(c("", paste("Barrett's oesophagus length:",
                                                                            sample(BarrettsLength, 1))), 1))))
  # Lets rename the one column to something more
  # intelligent
  names(Endoscopies) <- c("HospNum_Id", "EndoReports")
  ######### Data accordionisation Convert into paragraphs so
  ######### can be more easily separated
  Endoscopies$Date <- stringr::str_extract(Endoscopies$EndoReports,
                                           "Date of Procedure.*Endoscopist")
  Endoscopies$Endoscopist <- stringr::str_extract(Endoscopies$EndoReports,
                                                  "Endoscopist:.*Midazolam")
  Endoscopies$Midazolam <- stringr::str_extract(Endoscopies$EndoReports,
                                                "Midazolam:.*Fentanyl")
  Endoscopies$Fentanyl <- stringr::str_extract(Endoscopies$EndoReports,
                                               "Fentanyl:.*Indication")
  Endoscopies$Indication <- stringr::str_extract(Endoscopies$EndoReports,
                                                 "Indication:.*Diagnosis")
  Endoscopies$Diagnosis <- stringr::str_extract(Endoscopies$EndoReports,
                                                "Diagnosis:.*")
  Endoscopies$BarrC <- stringr::str_extract(Endoscopies$EndoReports,
                                            " oesophagus length: C.*M.*")
  Endoscopies$BarrM <- stringr::str_extract(Endoscopies$BarrC,
                                            "M.*")
  
  ######### Data cleaning Endoscopy dataset and formatting
  ######### the columns
  Endoscopies$Date <- gsub("Date of Procedure", "",
                           Endoscopies$Date)
  # Note we are using the date conversion function
  # here
  Endoscopies$Date <- as.Date(gsub(" Endoscopist",
                                   "", Endoscopies$Date), format = "%Y-%m-%d")
  
  Endoscopies$Endoscopist <- gsub("Endoscopist:  Dr ",
                                  "", Endoscopies$Endoscopist)
  Endoscopies$Endoscopist <- gsub("Midazolam", "",
                                  Endoscopies$Endoscopist)
  
  Endoscopies$Midazolam <- gsub("Midazolam: ", "",
                                Endoscopies$Midazolam)
  # Also reformatting this column into a nueric
  # column at the same time
  Endoscopies$Midazolam <- as.numeric(gsub("mg Fentanyl",
                                           "", Endoscopies$Midazolam))
  
  Endoscopies$Fentanyl <- gsub("Fentanyl: ", "",
                               Endoscopies$Fentanyl)
  # Also reformatting this column into a nueric
  # column at the same time
  Endoscopies$Fentanyl <- as.numeric(gsub("mcg Indication",
                                          "", Endoscopies$Fentanyl))
  
  Endoscopies$Indication <- gsub("Indication: ",
                                 "", Endoscopies$Indication)
  Endoscopies$Indication <- gsub(" Diagnosis", "",
                                 Endoscopies$Indication)
  
  Endoscopies$Diagnosis <- gsub("Indication: ", "",
                                Endoscopies$Diagnosis)
  Endoscopies$Diagnosis <- gsub(" Diagnosis", "",
                                Endoscopies$Diagnosis)
  
  Endoscopies$BarrC <- gsub("oesophagus length: ",
                            "", Endoscopies$BarrC)
  # Also reformatting this column into a nueric
  # column at the same time
  Endoscopies$BarrC <- gsub("M.*", "", Endoscopies$BarrC)
  Endoscopies$BarrC <- as.numeric(gsub("C", "", Endoscopies$BarrC))
  # Also reformatting this column into a nueric
  # column at the same time
  Endoscopies$BarrM <- as.numeric(gsub("M", "", Endoscopies$BarrM))
  # load(file = "Endoscopies.rda")
  return(Endoscopies)
}



#' FakeHistolCreator
#'
#' Creates fake histology data to play with
#' @param x None needed
#' @keywords Fake histology spreadshet data
#' @export
#' @examples Histop_df()

Histop_df <- function(x) {
  Generate a load of strings
  line <- list(x1 = "Intestinal metaplasia is present.",
               x2 = "Basal hyperplasia is prominent", x3 = "There is no dysplasia or malignancy.",
               x4 = "No Helicobacter are seen.", x5 = "There is some ulceration.",
               x6 = "There is no intercellular oedema in the surface epithelium.",
               x7 = " PAS staining shows occasional spores, consistent with candida.",
               x8 = " No herpetic viral inclusions are seen.",
               x9 = " There is no dysplasia and no invasive carcinoma.",
               x10 = " There is mild regenerative epithelial change, but neither dysplasia nor malignancy is seen.",
               x11 = "The appearances are consistent with the endoscopic diagnosis of Barrett's oesophagus with active chronic inflammation.",
               x12 = "The biopsies of oesophageal squamous mucosa show surface erosion and active chronic inflammation.",
               x13 = "Numerous Candida spores and hyphae are present admixed with ulcer slough.",
               x14 = "There is reactive basal cell hyperplasia and mild inflammatory epithelial atypia.",
               x15 = "There is no significant increase in intraepithelial eosinophils.",
               x16 = "No granulomas or viral inclusions are seen.",
               x17 = "The appearances are those of Candida oesophagitis.",
               x18 = "Neither dysplasia nor malignancy is seen.",
               x19 = "The appearances are consistent with, but not specific for Barrett's (columnar lined) oesophagus.",
               x20 = "High grade dysplasia is present throughout this sample",
               x21 = "There is low grade dysplasia", x22 = "This is a dysplastic sample")
  list.of.samples <- replicate(1000, paste("Macrosopic description:",
                                           sample(1:10, 1), "specimens collected the largest measuring",
                                           sample(1:5, 1), "x", sample(1:5, 1), "x", sample(1:5,
                                                                                            1), "mm and the smallest", sample(1:5,
                                                                                                                              1), "x", sample(1:5, 1), "x", sample(1:5,
                                                                                                                                                                   1), "mm"), simplify = FALSE)
  
  # Merge the strings together randomly
  histop <- replicate(1000, paste(sample(list.of.samples,
                                         1, replace = F), paste("Diagnoses", stringr::str_c(sample(line,
                                                                                                   sample(3:10, 1), replace = F), collapse = "."))))
  
  # Because we eventually will merge histopath and
  # endoscopy together we are going to be crafty and
  # generate the histopath dates from the endoscopy
  # dates with 0-2 days difference
  dat <- Endoscopies$Date + sample(0:2, 1)
  dat <- sample(seq(as.Date("2013/01/01"), as.Date("2017/05/01"),
                    by = "day"), 1000)
  # Generate hospital numbers from the Endoscopies
  # report
  HospNum_Id <- Endoscopies$HospNum_Id
  
  Histop_df <- data.frame(HospNum_Id, dat, paste("Date received:",
                                                 dat, histop))
  names(Histop_df) <- c("HospNum_Id", "dat", "HistoReport")
  
  ######### Data accordionisation Convert into paragraphs so
  ######### can be more easily separated
  Histop_df$Date <- stringr::str_extract(Histop_df$HistoReport,
                                         "Date received:.*Macrosopic description:")
  Histop_df$Macro <- stringr::str_extract(Histop_df$HistoReport,
                                          "Macrosopic description:.*Diagnoses")
  Histop_df$Diagnoses <- stringr::str_extract(Histop_df$HistoReport,
                                              "Diagnoses.*")
  ######### Data cleaning Histopathology dataset and
  ######### formatting the columns
  Histop_df$Date <- gsub("Date received: ", "", Histop_df$Date)
  Histop_df$Date <- as.Date(gsub("Macrosopic description:",
                                 "", Histop_df$Date), format = "%Y-%m-%d")
  
  Histop_df$Macro <- gsub("Macrosopic description: ",
                          "", Histop_df$Macro)
  Histop_df$Macro <- gsub("Diagnoses", "", Histop_df$Macro)
  
  Histop_df$Diagnoses <- gsub("Diagnoses", "", Histop_df$Diagnoses)
  # Lets get rid of a column we don't need
  Histop_df$dat <- NULL
  # load(file = "Histop_df.rda")
  return(Histop_df)
}

######### Data merging We can merge straight away as we
######### have the same names for the columns date and
######### HospNum_Id so no need to mess around. We will use
######### the fuzzyjoin method as there is sometimes a gap
######### between the endoscopy date and the date that the
######### histopathology was received:






 samplenumber <- 2000
 HospitalNumberID <- paste("Hospital Number: ", sample(c(LETTERS)), 
     sample(1e+06:9999999, (samplenumber - 1900), replace = T), 
    sep = "")
# NHS_Trust <- replicate(samplenumber, c("Hospital: Random NHS Foundation Trust"))
# Patient_Name <- paste("Patient Name: ", randomNames::randomNames(samplenumber, 
#     "first", "last"))
# Date_of_Birth <- paste("DOB: ", generator::r_date_of_births(samplenumber, 
#     start = as.Date("1900-01-01"), end = as.Date("1999-01-01")))
# GeneralPractictioner <- paste("General Practitioner: Dr. ", 
#                               randomNames::randomNames(samplenumber, "first", "last"), sep = "")
Date_of_ProcedureAll <- generator::r_date_of_births(samplenumber,
                                                    start = as.Date("2001-01-01"), end = as.Date("2017-01-01"))

#' EndoRaw
#'
#' Generates fake Endoscopy date
#' @param x None needed
#' @keywords fake endoscopy data
#' @import randomNames
#' @import generator
#' @export
#' @examples EndoRaw(x)

EndoRaw2 <- function() {
  Date_of_Procedure<-Date_of_ProcedureAll
  Date <- paste("Date of procedure: ", Date_of_Procedure)
  EndoscopistList <- as.list(sample(randomNames::randomNames(samplenumber,
                                                             "first", "last"), 10, replace = T))
  Second_EndoscopistList <- as.list(sample(randomNames::randomNames(samplenumber,
                                                                    "first", "last"), 10, replace = T))
  Endoscopist <- replicate(samplenumber, paste("Endoscopist: Dr. ",
                                               sample(EndoscopistList, 1, replace = F), sep = ""))
  Second_Endoscopist <- replicate(samplenumber, paste("2nd Endoscopist: Dr. ",
                                                      sample(Second_EndoscopistList, 1, replace = F),
                                                      sep = ""))
  MedicationsFent <- replicate(samplenumber, paste("Medications: Fentanyl ",
                                                   sample(list(x = "12.5mcg", x = "25mcg", x = "50mcg",
                                                               x = "75mcg", x = "100mcg", x = "125mcg",
                                                               x = "150mcg"), 1, replace = F)))
  MedicationsMidaz <- replicate(samplenumber, paste("Midazolam ",
                                                    sample(list(x = "1mg", x = "2mg", x = "3mg",
                                                                x = "4mg", x = "5mg", x = "6mg", x = "7mg"),
                                                           1, replace = F)))
  Instrument <- replicate(samplenumber, paste("Instrument: ",
                                              sample(list(x = "FG1", x = "FG2", x = "FG3",
                                                          x = "FG4", x = "FG5", x = "FG6", x = "FG7"),
                                                     1, replace = F)))
  Extent_of_Exam <- replicate(samplenumber, paste("Extent of Exam: ",
                                                  sample(list(x = "Failed intubation", x = "Oesophagus",
                                                              x = "Stomach body", x = "D1", x = "D2",
                                                              x = "Pylorus", x = "GOJ"), 1, replace = F)))
  # Import the Findings text from data folder - but
  # how to get it there?
  INDICATIONS_FOR_EXAMINATION <- replicate(samplenumber,
                                           paste("Indications:", sample(list(x1 = "Therapeutic- Dilatation",
                                                                             x2 = "Other-", x3 = "Follow-up ULCER HEALING",
                                                                             x4 = "Haematemesis or Melaena/Blood PR",
                                                                             x5 = "Previous OGD ? 8 months ago", x6 = "Dysphagia/Odynophagia",
                                                                             x7 = "Surveillance-Barrett's", x8 = "Nausea and/or Vomiting",
                                                                             x9 = "Weight Loss", x10 = "Dysphagia/intermittent for a few months",
                                                                             x11 = "Other-", x12 = "Small Bowel Biopsy",
                                                                             x13 = "Dyspepsia", x14 = "Reflux-like Symptoms/Atypical Chest Pain",
                                                                             x15 = "chronic abdo pain and constipaton",
                                                                             x16 = "Oesophagus- Dysplasia", x17 = "Therapeutic- RFA"),
                                                                        1, replace = F)))
  PROCEDURE_PERFORMED <- "Procedure Performed: Gastroscopy (OGD)"
  FINDINGS <- read.table("/home/rstudio/EndoMineR/data-raw/data/FindingsText",
                         header = T, stringsAsFactors = F)
  FINDINGS <- replicate(samplenumber, paste("Findings: ",
                                            stringr::str_c(as.list(sample(FINDINGS$x, sample(1:10),
                                                                          replace = T)), collapse = ",")))
  ENDOSCOPIC_DIAGNOSIS <- data.frame(c("Ulcer- Oesophageal. ",
                                       "Post chemo-radiotherapy stricture ", "Possible achalasia.",
                                       "Oesophagitis. ", "Food bolus obstructing the oesophagus.",
                                       "Hiatus Hernia. ", "Extensive neoplastic looking esophageal lesion. ",
                                       "Esophageal candidiasis ", "Barretts oesophagus. ",
                                       "Gastritis"), stringsAsFactors = F)
  names(ENDOSCOPIC_DIAGNOSIS) <- "x"
  ENDOSCOPIC_DIAGNOSIS <- replicate(samplenumber,
                                    paste("Endoscopic Diagnosis: ", stringr::str_c(as.list(sample(ENDOSCOPIC_DIAGNOSIS$x,
                                                                                                  sample(1:3), replace = F)), collapse = ",")))
  # Now put it all together in one long text to
  # simulate a real Endoscopic report
  TheOGDReport <- data.frame(NHS_Trust, HospitalNumberID,
                             Patient_Name, GeneralPractictioner, Date, Endoscopist,
                             Second_Endoscopist, MedicationsFent, MedicationsMidaz,
                             Instrument, Extent_of_Exam, INDICATIONS_FOR_EXAMINATION,
                             PROCEDURE_PERFORMED, FINDINGS, ENDOSCOPIC_DIAGNOSIS)
  # Now paste the OGD report dataframe together to
  # make the fake report:
  TheOGDReportFinal <- tidyr::unite(TheOGDReport,
                                    cat(paste(colnames(TheOGDReport), collapse = "\n")),
                                    colnames(TheOGDReport), sep = "\n")
  names(TheOGDReportFinal) <- "OGDReportWhole"
  # load(file = "./data_raw/data/TheOGDReportFinal.rda")
  # return(TheOGDReportFinal)
}


#' pathRep
#'
#' Creates raw Pathology reports
#' @param x None needed
#' @import stringr
#' @import generator
#' @keywords Pathology reports
#' @export
#' @examples pathRep(x)

pathRep2 <- function() {
  Date <- Date_of_ProcedureAll+sample(1:12)
  Date <- paste("Date received: ", Date_of_Procedure +
                  replicate(samplenumber, as.numeric(sample(1:10)),
                            1))
  # Clinical Details
  ClinDet <- read.table("./data-raw/data/HistolClinDetText",
                        header = T, stringsAsFactors = F)
  ClinDet <- replicate(samplenumber, paste("Clinical Details: ",
                                           stringr::str_c(as.list(sample(ClinDet$x, sample(1:10),
                                                                         replace = T)), collapse = ",")))
  # Nature of the specimen
  NatureOfSpec <- read.table("./data-raw/data/HistolMacDescription.txt",
                             header = T, stringsAsFactors = F)
  NatureOfSpec <- replicate(samplenumber, paste(sample(1:10,1,replace=T),"specimen. Nature of specimen: ",
                                                stringr::str_c(as.list(sample(NatureOfSpec$x, sample(1:10), replace = T)), collapse = ",")))
  
  MacDescrip <- unlist(replicate(samplenumber, paste("Macroscopic description:",
                                                     sample(1:10, 1), "specimens collected the largest measuring",
                                                     sample(1:5, 1), "x", sample(1:5, 1), "x", sample(1:5,
                                                                                                      1), "mm and the smallest", sample(1:5,
                                                                                                                                        1), "x", sample(1:5, 1), "x", sample(1:5,
                                                                                                                                                                             1), "mm"), simplify = FALSE))
  # Merge the strings together randomly
  
  # Histol Details
  Histol <- read.table("./data-raw/data/HistolText",
                       header = T, stringsAsFactors = F)
  Histol <- replicate(samplenumber, paste("Histology: ",
                                          stringr::str_c(as.list(sample(Histol$x, sample(1:10),
                                                                        replace = T)), collapse = ",")))
  Diagnostic <- read.table("./data-raw/data/HistolDxText",
                           header = T, stringsAsFactors = F)
  
  Diagnostic <- replicate(samplenumber, paste("Diagnosis: ",
                                              stringr::str_c(as.list(sample(Diagnostic$x, sample(5:10),
                                                                            replace = T)), collapse = ",")))
  PathDataFrameReport <- data.frame(NHS_Trust, HospitalNumberID,
                                    Patient_Name, Date_of_Birth, GeneralPractictioner,
                                    Date, ClinDet, NatureOfSpec, MacDescrip, Histol,
                                    Diagnostic)
  PathDataFrameFinal <- tidyr::unite(PathDataFrameReport,
                                     cat(paste(colnames(PathDataFrameReport), collapse = "\n")),
                                     colnames(PathDataFrameReport), sep = "\n")
  names(PathDataFrameFinal) <- "PathReportWhole"
  # load(file = "./data_raw/data/PathDataFrameFinal.rda")
  return(PathDataFrameFinal)
}




#' ColonEndoRaw
#'
#' Creates raw Pathology reports
#' @param x None needed
#' @keywords Pathology reports
#' @import randomNames
#' @import generator
#' @export
#' @examples ColonEndoRaw(x)

ColonEndoRaw <- function(x) {
  Date_of_Procedure <- generator::r_date_of_births(samplenumber,
                                                   start = as.Date("2001-01-01"), end = as.Date("2017-01-01"))
  Date <- paste("Date of procedure: ", Date_of_Procedure)
  EndoscopistList <- as.list(sample(randomNames::randomNames(samplenumber,
                                                             "first", "last"), 10, replace = T))
  Second_EndoscopistList <- as.list(sample(randomNames::randomNames(samplenumber,
                                                                    "first", "last"), 10, replace = T))
  Endoscopist <- replicate(samplenumber, paste("Endoscopist: Dr. ",
                                               sample(EndoscopistList, 1, replace = F), sep = ""))
  Second_Endoscopist <- replicate(samplenumber, paste("2nd Endoscopist: Dr. ",
                                                      sample(Second_EndoscopistList, 1, replace = F),
                                                      sep = ""))
  MedicationsFent <- replicate(samplenumber, paste("Medications: Fentanyl ",
                                                   sample(list(x = "12.5mcg", x = "25mcg", x = "50mcg",
                                                               x = "75mcg", x = "100mcg", x = "125mcg",
                                                               x = "150mcg"), 1, replace = F)))
  MedicationsMidaz <- replicate(samplenumber, paste("Midazolam ",
                                                    sample(list(x = "1mg", x = "2mg", x = "3mg",
                                                                x = "4mg", x = "5mg", x = "6mg", x = "7mg"),
                                                           1, replace = F)))
  Instrument <- replicate(samplenumber, paste("Instrument: ",
                                              sample(list(x = "FC1", x = "FC2", x = "FC3",
                                                          x = "FC4", x = "FC5", x = "FC6", x = "FC7"),
                                                     1, replace = F)))
  Extent_of_Exam <- replicate(samplenumber, paste("Extent of Exam: ",
                                                  sample(list(x = "Failed intubation", x = "Recum",
                                                              x = "Sigmoid", x = "Descending Colon",
                                                              x = "Transverse Colon", x = "Ascending Colon",
                                                              x = "Caecum"), 1, replace = F)))
  # Import the Findings text from data folder - but
  # how to get it there?
  INDICATIONS_FOR_EXAMINATION <- replicate(samplenumber,
                                           paste("Indications:", sample(list(x1 = "Therapeutic- Dilatation",
                                                                             x2 = "Other-", x3 = "Diarrrhoea", x4 = "Weight loss",
                                                                             x5 = "IBD Surveillance", x6 = "PR Bleeding",
                                                                             x7 = "Family History CRC", x8 = "Nausea and/or Vomiting",
                                                                             x9 = "Abnormal Imaging", x10 = "Planned polypectomy",
                                                                             x11 = "Fe deficiency anaemia", x12 = "Chronic abdominal pain"),
                                                                        1, replace = F)))
  PROCEDURE_PERFORMED <- "Procedure Performed: Colonoscopy"
  FINDINGS <- read.table("./data_raw/data/FindingsTextColon",
                         header = T, stringsAsFactors = F)
  FINDINGS <- replicate(samplenumber, paste("Findings: ",
                                            stringr::str_c(as.list(sample(FINDINGS$x, sample(1:10),
                                                                          replace = T)), collapse = ",")))
  ENDOSCOPIC_DIAGNOSIS <- data.frame(c("Ulcer- Oesophageal. ",
                                       "Post chemo-radiotherapy stricture ", "Possible achalasia.",
                                       "Oesophagitis. ", "Food bolus obstructing the oesophagus.",
                                       "Hiatus Hernia. ", "Extensive neoplastic looking esophageal lesion. ",
                                       "Esophageal candidiasis ", "Barretts oesophagus. ",
                                       "Gastritis"), stringsAsFactors = F)
  names(ENDOSCOPIC_DIAGNOSIS) <- "x"
  ENDOSCOPIC_DIAGNOSIS <- replicate(samplenumber,
                                    paste("Endoscopic Diagnosis: ", stringr::str_c(as.list(sample(ENDOSCOPIC_DIAGNOSIS$x,
                                                                                                  sample(1:3), replace = F)), collapse = ",")))
  # Now put it all together in one long text to
  # simulate a real Endoscopic report
  TheOGDReport <- data.frame(NHS_Trust, HospitalNumberID,
                             Patient_Name, GeneralPractictioner, Date, Endoscopist,
                             Second_Endoscopist, MedicationsFent, MedicationsMidaz,
                             Instrument, Extent_of_Exam, INDICATIONS_FOR_EXAMINATION,
                             PROCEDURE_PERFORMED, FINDINGS, ENDOSCOPIC_DIAGNOSIS)
  # Now paste the OGD report dataframe together to
  # make the fake report:
  TheOGDReportFinal <- tidyr::unite(TheOGDReport,
                                    cat(paste(colnames(TheOGDReport), collapse = "\n")),
                                    colnames(TheOGDReport), sep = "\n")
  names(TheOGDReportFinal) <- "OGDReportWhole"
  ColonFinal<-TheOGDReportFinal
  # load(file = "ColonFinal.rda")
  return(TheOGDReportFinal)
}


#' ColonpathRep
#'
#' Creates raw Pathology reports
#' @param x None needed
#' @import stringr
#' @keywords Pathology reports
#' @export
#' @examples ColonpathRep(x)

ColonpathRep <- function(x) {
  Date_of_Procedure <- generator::r_date_of_births(samplenumber,
                                                   start = as.Date("2001-01-01"), end = as.Date("2017-01-01"))
  Date <- paste("Date received: ", Date_of_Procedure +
                  replicate(samplenumber, as.numeric(sample(1:10)),
                            1))
  # Clinical Details
  ClinDet <- read.table("./data_raw/data/Histopath_ClinDetPhrasesColon.txt",
                        header = F, stringsAsFactors = F)
  ClinDet <- replicate(samplenumber, paste("Clinical Details: ",
                                           stringr::str_c(as.list(sample(ClinDet$V1, sample(1:10),
                                                                         replace = T)), collapse = ",")))
  # Nature of the specimen
  NatureOfSpec <- read.table("./data_raw/data/Histopath_MacDescripPhrasesColon.txt",
                             header = F, stringsAsFactors = F)
  NatureOfSpec <- replicate(samplenumber, paste(sample(1:10,1,replace=T),"specimen. Nature of specimen: ",
                                                stringr::str_c(as.list(sample(NatureOfSpec$V1, sample(1:10), replace = T)), collapse = ",")))
  MacDescrip <- unlist(replicate(samplenumber, paste("Macroscopic description:",
                                                     sample(1:10, 1), "specimens collected the largest measuring",
                                                     sample(1:5, 1), "x", sample(1:5, 1), "x", sample(1:5,
                                                                                                      1), "mm and the smallest", sample(1:5,
                                                                                                                                        1), "x", sample(1:5, 1), "x", sample(1:5,
                                                                                                                                                                             1), "mm"), simplify = FALSE))
  # Merge the strings together randomly
  # MacDescrip<-replicate(1000,paste
  # (sample(list.of.samples,1,replace=F),paste('Diagnoses',stringr::stringr::str_c(sample(line,sample(3:10,1),replace=F),collapse='.'))))
  # Histol Details
  Histol <- read.table("./data_raw/data/HistolTextColon",
                       header = F, stringsAsFactors = F)
  Histol <- replicate(samplenumber, paste("Histology: ",
                                          stringr::str_c(as.list(sample(Histol$V1, sample(1:10),
                                                                        replace = T)), collapse = ",")))
  # Diagnostic details
  Diagnostic <- read.table("./data_raw/data/Histopath_DxRawColon.txt",
                           header = F, stringsAsFactors = F)
  Diagnostic <- replicate(samplenumber, paste("Diagnosis: ",
                                              stringr::str_c(as.list(sample(Diagnostic$V1, sample(5:10),
                                                                            replace = T)), collapse = ",")))
  PathDataFrameReport <- data.frame(NHS_Trust, HospitalNumberID,
                                    Patient_Name, Date_of_Birth, GeneralPractictioner,
                                    Date, ClinDet, NatureOfSpec, MacDescrip, Histol,
                                    Diagnostic)
  PathDataFrameFinalColon <- tidyr::unite(PathDataFrameReport,
                                          cat(paste(colnames(PathDataFrameReport), collapse = "\n")),
                                          colnames(PathDataFrameReport), sep = "\n")
  names(PathDataFrameFinalColon) <- "PathReportWhole"
  # load(file = "./data_raw/data/PathDataFrameFinalColon.rda")
  return(PathDataFrameFinalColon)
}
