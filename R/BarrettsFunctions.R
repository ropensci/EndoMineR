if (getRversion() >= "2.15.1") utils::globalVariables(c("PatientID", ".SD", "CStage", "NumbOfBx", 
    "Years", "Difference", "barplot", "head", "read.table", "eHospitalNum", "pHospitalNum", ".", 
    "EVENT", "MonthYear", "freq", "Endoscopist", "avg", "v", "destination", "dcast", "complete.cases", 
    "g", "gvisSankey", "head", "pHospitalNum", "par", "plot", "r", "read.table", "region", "rgb", 
    "setDT"))
###### Barretts Surveillance and Therapeutic Functions ######

#' BarrettsDataAccord_Prague
#' 
#' The aim is to extract a C and M stage (Prague score) for Barrett's samples.
#' This is done using a regex where C and M stages are explicitly mentioned in 
#' the free text
#' Specfically it extracts the Prague score
#' @param x dataframe with column of interest
#' @param y column of interest
#' @importFrom stringr str_extract
#' @keywords  Prague score
#' @export
#' @examples v<-BarrettsDataAccord_Prague(Myendo,'Findings')

BarrettsDataAccord_Prague <- function(x, y) {  
    x <- data.frame(x)
    x$CStage <- stringr::str_extract(x[, y], "(C(\\s|=)*\\d+)")
    x$CStage <- as.numeric(gsub("C", "", x$CStage))
    x$MStage <- stringr::str_extract(x[, y], "(M(\\s|=)*\\d+)")
    x$MStage <- as.numeric(gsub("M", "", x$MStage))
    return(x)
}


#' BarrettsDataAccord_PathStage
#' 
#' This extracts the pathological stage from the histopathology specimen. It is 
#' done using 'degradation' so that it will look for the worst overall grade 
#' in the histology specimen and if not found it will look for the next worst
#' and so on. It looks per report not per biopsy (it is more common
#' for histopathology reports tocontain the worst overall grade 
#' rather than individual biopsy grades).
#' Specfically it extracts the histopathology worst grade
#' Being the procedure done at the time and the follow-up timings
#' @param x dataframe with column of interest
#' @param y column of interest
#' @keywords Pathology extraction 
#' @export
#' @examples v<-HistolChopperAccessionNumber(Mypath,"Histology","SP-\\d{2}-\\d{7}")
#' v<-HistolChopperDx(v,"Diagnosis")
#' v<-HistolChopperExtrapolDx(v,"Diagnosis")
#' v<-HistolChopperNumbOfBx(v,"Macroscopicdescription","specimen")
#' v<-HistolChopperBxSize(v,"Macroscopicdescription")
#' b<-BarrettsDataAccord_PathStage(v,'Histology')

BarrettsDataAccord_PathStage <- function(x, y) {
    # Get the worst pathology for that sample
    x <- data.frame(x)
    x$IMorNoIM <- ifelse(grepl("[Ss][Mm]1", x[, y], perl = TRUE), "SM2", ifelse(grepl("[Ss][Mm]1", 
        x[, y], perl = TRUE), "SM1", ifelse(grepl("T1b", x[, y], perl = TRUE), "T1b_Unspec", 
        ifelse(grepl("T1a|ntramucosal", x[, y], perl = TRUE), "T1a", ifelse(grepl("[Hh]igh grade ", 
            x[, y], perl = TRUE), "HGD", ifelse(grepl("[Ll]ow grade", x[, y], perl = TRUE), "LGD", 
            ifelse(grepl("[Ii]ndef", x[, y], perl = TRUE), "IGD", ifelse(grepl("(?<!egative for |No evidence of |[Nn]o |either |or |and )[Ii]ntestinal [Mm]etaplasia", 
                x[, y], perl = TRUE), "IM", ifelse(grepl("(?<!egative for |No evidence of |[Nn]o |either |or |and )[Ii]ntestinal [Mm]etaplasia", 
                x[, y], perl = TRUE), "IM", ifelse(is.na(x[, y]), "No tissue", "No_IM"))))))))))
    return(x)
}


#' BarrettsDataAccord_Event
#' 
#' This function extracts the Event- usually a therapeutic event, from the text eg endoscopic
#' mucosal resection, radiofrequency ablation etc. It does not currently include stricture
#' dilatation.Specfically it extracts the event
#' 
#' @param x the dataframe
#' @param y The histology main text
#' @param z The Procedure Performed column
#' @param aa The endoscopic diagnosis column
#' @param bb The endoscopic findings column if different to the Diagnosis column
#' @keywords Event extraction
#' @export
#' @examples v<-HistolChopperAccessionNumber(Mypath,"Histology","SP-\\d{2}-\\d{7}")
#' v<-HistolChopperDx(v,"Diagnosis")
#' v<-HistolChopperExtrapolDx(v,"Diagnosis")
#' v<-HistolChopperNumbOfBx(v,"Macroscopicdescription","specimen")
#' v<-HistolChopperBxSize(v,"Macroscopicdescription")
#' v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,"Dateofprocedure","HospitalNumber")
#' b<-BarrettsDataAccord_Event(v,'Histology',
#' 'ProcedurePerformed','OGDReportWhole','Findings')
#'
BarrettsDataAccord_Event <- function(x, y, z, aa, bb) {
    x <- data.frame(x)
    # Get all the EVENTS in:
    x$EVENT <- ifelse(grepl("[Ee][Mm][Rr]", x[, y], perl = TRUE), "EMR", ifelse(grepl("[Ee]ndoscopic [Mm]ucosal [Rr]esection", 
        x[, y], perl = TRUE), "EMR", ifelse(grepl("ndomucosal", x[, y], perl = TRUE), "EMR", 
        ifelse(grepl("HALO|RFA", x[, z], perl = TRUE), "RFA", ifelse(grepl("APC", x[, z], perl = TRUE), 
            "APC", ifelse(grepl("HALO|RFA", x[, aa], perl = TRUE), "RFA", ifelse(grepl("APC", 
                x[, aa], perl = TRUE), "RFA", ifelse(grepl("HALO|RFA", x[, bb], perl = TRUE), 
                "RFA", ifelse(grepl("APC", x[, bb], perl = TRUE), "APC", "nothing")))))))))
    return(x)
}

#' BarrettsDataAccord_FUGroup
#' 
#' This determines the follow up rule a patient should fit in to (according to the 
#' British Society for Gastroenterology guidance on Barrett's oesophagus)
#' Specfically it combines the presence of intestinal metaplasia with 
#' Prague score so the follow-up group can be determined. It relies on the presence of a Prague
#' score. It should be run after BarrettsDAtaAccord_PathStage which looks for the worst stage of a 
#' specimen and which will determine the presence or absence of intestinal metaplasia
#' if the sample is non-dysplastic.
#' Being the procedure done at the time and the follow-up timings
#' @param x the dataframe(which has to have been processed by the BarrettsDataAccord_PathStage function first to get IMorNoIM), 
#' @param y The field to search (endoscopic findings)
#' @keywords Follow-Up
#' @export
#' @examples v<-HistolChopperDx(Mypath,"Diagnosis")
#' v<-HistolChopperExtrapolDx(v,"Diagnosis")
#' v<-HistolChopperNumbOfBx(v,"Macroscopicdescription","specimen")
#' v<-HistolChopperBxSize(v,"Macroscopicdescription")
#' v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,"Dateofprocedure","HospitalNumber")
#' b<-BarrettsDataAccord_PathStage(v,'Histology')
#' b2<-BarrettsDataAccord_Event(b,'Histology',
#' 'ProcedurePerformed','OGDReportWhole','Findings')
#' b3<-BarrettsDataAccord_FUGroup(b2,'Findings')

BarrettsDataAccord_FUGroup <- function(x, y) {
    x <- data.frame(x)
    # Do the follow-up groupings
    try(x$MStage <- ifelse(grepl("(C(\\s|=)*\\d+)", x[, y], perl = TRUE), stringr::str_extract(x[, 
        y], "(M(\\s|=)*\\d+)"), ifelse(grepl("[Ss]hort|[Tt]iny|[Tt]ongue|[Ss]mall", x[, y], perl = TRUE), 
        "0", ifelse(grepl("[1-3]{1}(\\s)*?[cC][mM]", x[, y], perl = TRUE), stringr::str_extract(x[, 
            y], "[1-3]{1}(\\s)*?[cC][mM]"), NA))))
    
    x$MStage <- gsub("M|cm|=", "", x$MStage)
    
    x$MStage <- as.integer(x$MStage)
    x$FU_Group <- ifelse(x$IMorNoIM == "No_IM" & x$MStage < 3, "Rule1", ifelse(x$IMorNoIM == 
        "IM" & x$MStage < 3, "Rule2", ifelse(x$MStage >= 3, "Rule3", "NoRules")))
    return(x)
    
}



############## Surveillance functions ########


#' BarrettsPatientTracking_Enrollment_Surveillance
#' 
#' This function graphs the patients who were not on surveillance programmes and sees how many then had an endoscopy.
#' This allows us to determine how many index Barrett's detections went on to undergo surveillance.
#' This should be run after the BarrettsDataAccord_Prague and BarrettsDataAccord_PathStage.
#' @param x dataframe
#' @param PatientID column of interest with unique hospital number in it
#' @param Endo_ResultPerformed column of interest with date endiscopy performed in it
#' @param IndicationsFroExamination column of interest with indications in it (usually 'Surveillance' or similar)
#' @keywords Patient Tracking
#' @importFrom dplyr group_by slice mutate filter
#' @importFrom magrittr '%>%'
#' @export
#' @examples Enroll<-BarrettsPatientTracking_Enrollment_Surveillance(Myendo,'HospitalNumber',
#' 'Dateofprocedure','Indications')

BarrettsPatientTracking_Enrollment_Surveillance <- function(x, PatientID, Endo_ResultPerformed, 
    IndicationsFroExamination) {
    x <- data.frame(x)
    PatientIDa <- rlang::sym(PatientID)
    Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)
    IndicationsFroExaminationa <- rlang::sym(IndicationsFroExamination)
    # So you want all those whose last endoscopy was non surveillance but who have a difftime
    # between now and the last of > 3years So get the last endoscopy for each patient Filter out
    # the endoscopies that were surveillance Get the difftime between now and the last endoscopy
    # Filter for those who have been waiting >3 years post non surveillance endoscopy
    
    t <- x %>% arrange(as.Date(!!Endo_ResultPerformeda)) %>% group_by(!!PatientIDa) %>% slice(n()) %>% filter(!grepl("Surv|Barr", 
        !!IndicationsFroExamination)) %>% mutate(Years = difftime(as.Date(Sys.Date()), as.Date(!!Endo_ResultPerformeda), 
        units = "weeks")/52) %>% filter(Years > 3)
    
    return(t)
    
}


#' BarrettsPatientTracking_UniqueHospNum
#' 
#' This function gets the unique patient ID's for each patient, 
#' for each rule. It lists the unique PatientIDs assocaited with a rule 
#' ('Rule1','Rule2','Rule3','NoRules'). This allows us to determine how many patients will need 
#' follow up at specific time intervals. This should be run after the BarrettsDataAccord_Prague 
#' and BarrettsDataAccord_PathStage.
#' @param x dataframe with column of interest
#' @param rule Rule of interest
#' @param PatientID Column containing patient numbers
#' @keywords Rule
#' @export
#' @examples v<-HistolChopperAccessionNumber(Mypath,"Histology","SP-\\d{2}-\\d{7}")
#' v<-HistolChopperDx(v,"Diagnosis")
#' v<-HistolChopperExtrapolDx(v,"Diagnosis")
#' v<-HistolChopperNumbOfBx(v,"Macroscopicdescription","specimen")
#' v<-HistolChopperBxSize(v,"Macroscopicdescription")
#' v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,"Dateofprocedure","HospitalNumber")
#' b1<-BarrettsDataAccord_Prague(v,'Findings')
#' b2<-BarrettsDataAccord_PathStage(b1,'Histology')
#' b3<-BarrettsDataAccord_Event(b2,'Histology',
#' 'ProcedurePerformed','OGDReportWhole','Findings')
#' b4<-BarrettsDataAccord_FUGroup(b3,'Findings')
#' colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#' Rule<-BarrettsPatientTracking_UniqueHospNum(b4,'Rule1','HospitalNumber')

BarrettsPatientTracking_UniqueHospNum <- function(x, rule, PatientID) {
    x <- data.frame(x)
    x <- subset(x, x$FU_Group == rule)
    x <- data.frame(unique(x[, PatientID]))
    names(x) <- c("x")
    return(x)
}

######## Endoscopic Performance Quality-#####


#' BarrettsQuality_AnalysisDocumentation
#' 
#' This function assesses the Barrett's documentation. This notes how many reports contain
#' the mandatory report fields as specified in the BSG standards on 
#' Barrett's endoscopic reporting. This should be run after the BarrettsDataAccord_Prague.
#' @param x dataframe
#' @param Findings column of interest- usually the main body of the endoscopic report
#' @import lattice
#' @keywords Documentation
#' @export
#' @examples v<-HistolChopperDx(Mypath,"Diagnosis")
#' v<-HistolChopperExtrapolDx(v,"Diagnosis")
#' v<-HistolChopperNumbOfBx(v,"Macroscopicdescription","specimen")
#' v<-HistolChopperBxSize(v,"Macroscopicdescription")
#' v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,"Dateofprocedure","HospitalNumber")
#' b<-BarrettsDataAccord_PathStage(v,'Histology')
#' BarrettsQuality_AnalysisDocumentation(b,"Findings")



BarrettsQuality_AnalysisDocumentation <- function(x, Findings) {
    x <- data.frame(x)
    # Is Surveillance documentation done properly?
    PragueSubsetx <- subset(x, !is.na(x$MStage))
    
    # Presence of islands
    IslandSubsetx <- x[grepl("[Ii]sland", x[, Findings]), ]
    
    # Hiatus hernia (top of gastric folds)
    HerniaSubsetx <- x[grep("[Hh]iat|astric fold|[Pp]inch", x[, Findings]), ]
    
    
    # Visible lesions- should also describe the absence of visible lesions explicitly
    LesionSubsetx <- x[grep("esion|odule|lcer", x[, Findings]), ]
    
    # Classification of lesions On surveillance vs not on surveillance Thie one is done as part
    # of the Therapeutic survey so a different dataset.
    
    # Biopsies (location and samples taken) Decided not to do this as no point as all biopsies
    # are labelled at time of pathology so don't see why they should be on the form. On
    # surveillance vs not on surveillance
    
    n = c(as.numeric(nrow(PragueSubsetx)/nrow(x)), as.numeric(nrow(IslandSubsetx)/nrow(x)), as.numeric(nrow(HerniaSubsetx)/nrow(x)), 
        as.numeric(nrow(LesionSubsetx))/nrow(x))
    s = c("On", "On", "On", "On")
    b = c("Prague", "Prague", "Island", "Island", "Hernia", "Hernia", "Lesion", "Lesion")
    EndoMinDataSet <- data.frame(s, b, n)
    
    t <- lattice::barchart(b ~ n, data = EndoMinDataSet, groups = s, scales = list(x = list(rot = 0, 
        cex = 2), y = list(cex = 2), main = list("Endoscopy documentation for Barrett's PRE DOI", 
        cex = 2.8)), key = list(space = "right", lines = list(col = c("pink", "lightblue"), lty = c(2, 
        2), lwd = 16), text = list(c("On", "Off"), cex = 2)))
    return(t)
    
}

############## Pathology Quality #############


#' BarrettsQuality_AnalysisBiopsyNumber
#' 
#' This function gets the biopsies taken per endoscopy and compares to the Prague score 
#' for that endoscopy.
#' @param x dataframe
#' @param Endo_ResultPerformed Date of the Endocscopy
#' @param PatientID Patient's unique identifier
#' @param Endoscopist name of the column with the Endoscopist names
#' @importFrom dplyr summarise group_by filter
#' @keywords Does something with data
#' @export
#' @examples v<-HistolChopperAccessionNumber(Mypath,"Histology","SP-\\d{2}-\\d{7}")
#' v<-HistolChopperDx(v,"Diagnosis")
#' v<-HistolChopperExtrapolDx(v,"Diagnosis")
#' v<-HistolChopperNumbOfBx(v,"Macroscopicdescription","specimen")
#' v<-HistolChopperBxSize(v,"Macroscopicdescription")
#' v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,"Dateofprocedure","HospitalNumber")
#' b1<-BarrettsDataAccord_Prague(v,'Findings')
#' b2<-BarrettsDataAccord_PathStage(b1,'Histology')
#' b3<-BarrettsDataAccord_Event(b2,'Histology',
#' 'ProcedurePerformed','OGDReportWhole','Findings')
#' b4<-BarrettsDataAccord_FUGroup(b3,'Findings')
#' colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#' BarrettsQuality_AnalysisBiopsyNumber(b4,'Endoscopist','Dateofprocedure','Endoscopist')

BarrettsQuality_AnalysisBiopsyNumber <- function(x, Endo_ResultPerformed, PatientID, Endoscopist) {
    x <- data.frame(x)
    PatientIDa <- rlang::sym(PatientID)
    Endo_ResultPerformeda <- rlang::sym(Endo_ResultPerformed)
    Endoscopista <- rlang::sym(Endoscopist)
    
    GroupedByEndoscopy <- x %>% dplyr::filter(!is.na(CStage), !is.na(NumbOfBx)) %>% dplyr::group_by(as.Date(!!Endo_ResultPerformeda), 
        !!PatientID, !!Endoscopista) %>% dplyr::summarise(Sum = sum(NumbOfBx), AvgC = mean(CStage))
    
    GroupedByEndoscopy$ExpectedNumber <- (GroupedByEndoscopy$AvgC + 1) * 2
    GroupedByEndoscopy$Difference <- GroupedByEndoscopy$Sum - GroupedByEndoscopy$ExpectedNumber
    
    # Now group the difference by endoscopist
    BxShortfallPre <- GroupedByEndoscopy %>% dplyr::group_by(!!Endoscopista) %>% dplyr::summarise(MeanDiff = mean(Difference))
    
    # e) Then show shortfall of number of biopsies on a graph
    t <- ggplot2::ggplot() + geom_point(aes(BxShortfallPre$Endoscopist, BxShortfallPre$MeanDiff), 
        size = 9) + geom_point(cex = 2) + labs(title = "Shortfall number of biopsies on Barrett's Surveillance list", 
        x = "", y = "") + theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) + theme(legend.position = "top") + 
        xlab("Endoscopist") + ylab("Shortfall(Obs-\nexpec number Bx)") + theme(axis.text.x = element_text(angle = -90, 
        size = 10)) + theme(axis.text.y = element_text(angle = -90, size = 10)) + theme(axis.title = element_text(size = 10)) + 
        theme(title = element_text(size = 10)) + theme(legend.position = "top")
    
    functionResults <- list(BxShortfallPre = BxShortfallPre, t = t)
    return(functionResults)
    
}


############## Diagnostic yield functions #######

#' BarrettsSurveillance_PathDetection
#' 
#' This function assesses pathology of specimens taken at surveillance per year.
#' It outputs a plot which determines the the overall number of pathologies (
#' low/high grade dysplasia and cancer) for patients on surveillance
#' 
#' @param x the dataframe
#' @param titlePlot The plot title
#' @keywords dysplasia and cancer detection
#' @import ggplot2
#' @importFrom magrittr '%>%'
#' @export
#' @examples v<-HistolChopperDx(Mypath,"Diagnosis")
#' v<-HistolChopperExtrapolDx(v,"Diagnosis")
#' v<-HistolChopperNumbOfBx(v,"Macroscopicdescription","specimen")
#' v<-HistolChopperBxSize(v,"Macroscopicdescription")
#' v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,"Dateofprocedure","HospitalNumber")
#' b1<-BarrettsDataAccord_Prague(v,'Findings')
#' b2<-BarrettsDataAccord_PathStage(b1,'Histology')
#' b3<-BarrettsDataAccord_Event(b2,'Histology',
#' 'ProcedurePerformed','OGDReportWhole','Findings')
#' b4<-BarrettsDataAccord_FUGroup(b3,'Findings')
#' colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#' BarrettsSurveillance_PathDetection(b4,'Myplot')


BarrettsSurveillance_PathDetection <- function(x, titlePlot) {
    x <- data.frame(x)
    
    LGD <- x[grepl("LGD", x$IMorNoIM), ]
    HGD <- x[grepl("HGD", x$IMorNoIM), ]
    OAC <- x[grepl("SM1|SM2|T1b|T1a", x$IMorNoIM), ]
    
    n = c(nrow(LGD), nrow(HGD), nrow(OAC))
    b = c("LGD", "HGD", "OAC")
    EndoMinDataSet <- data.frame(b, n)
    
    ggplot2::ggplot(EndoMinDataSet, aes(x = b, y = n)) + geom_bar(stat = "identity") + xlab("Pathology Grade") + 
        ylab("Total Number") + theme(axis.text.x = element_text(angle = -90)) + labs(title = titlePlot) + 
        theme(legend.position = "top")
}




#' BarrettsSurveillanceDDR
#' 
#' This function assesses the dysplasia detection rate per endoscopist.
#' @param x dataframe, 
#' @param y column with the Endoscopist names
#' @param IMorNoIM extracted with the function BarrettsDataAccord_PathStage()
#' @keywords dysplasia detection rate
#' @export
#' @examples v<-HistolChopperDx(Mypath,"Diagnosis")
#' v<-HistolChopperExtrapolDx(v,"Diagnosis")
#' v<-HistolChopperNumbOfBx(v,"Macroscopicdescription","specimen")
#' v<-HistolChopperBxSize(v,"Macroscopicdescription")
#' v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,"Dateofprocedure","HospitalNumber")
#' b1<-BarrettsDataAccord_Prague(v,'Findings')
#' b2<-BarrettsDataAccord_PathStage(b1,'Histology')
#' b3<-BarrettsDataAccord_Event(b2,'Histology',
#' 'ProcedurePerformed','OGDReportWhole','Findings')
#' b4<-BarrettsDataAccord_FUGroup(b3,'Findings')
#' colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
#' BarrettsSurveillanceDDR(b4,'Endoscopist','IMorNoIM')



BarrettsSurveillanceDDR <- function(x, y, IMorNoIM) {
    x <- data.frame(x)
    
    # Need to include indefinite for dysplasia
    myDDR <- prop.table(table(x[, y], x[, IMorNoIM]))
    return(myDDR)
}




#### Therapeutics Functions ####

#' BarrettsTherapeuticsOnly
#' 
#' This function makes all therapeutics unique. This should only be run after the 
#' BarrettsDataAccord_EventFunction as this provides an EVENT column which this 
#' function uses.
#' @param x dataframe
#' @keywords Therapeutic 
#' @export
#' @examples 



BarrettsTherapeuticsOnly <- function(x) {
    x <- data.frame(x)
    x <- x[x$EVENT != "nothing", ]
    return(x)
    
}


#' BarrettsTherapy_Numbers_EMRsByGrade
#' 
#' Plots all the pathological grades of the EMRs.This should only be run after all the 
#' BarrettsDataAccord functions.
#' @param EndoSubsetEMR The dataset. 
#' @keywords EMR chart
#' @export
#' @examples 

BarrettsTherapy_Numbers_EMRsByGrade <- function(EndoSubsetEMR) {
    
    EndoSubsetEMR <- EndoSubsetEMR[EndoSubsetEMR$EVENT == "EMR", ]
    AllEMRs <- nrow(EndoSubsetEMR)
    SM2 <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "SM2", ])
    SM1 <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "SM1", ])
    T1b <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "T1b", ])
    T1a <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "T1a", ])
    HGD <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "HGD", ])
    LGD <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "LGD", ])
    IM <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "IM", ])
    NoIM <- nrow(EndoSubsetEMR[EndoSubsetEMR$IMorNoIM == "No_IM", ])
    
    n = c(as.numeric(AllEMRs), as.numeric(SM2), as.numeric(SM1), as.numeric(T1b), as.numeric(T1a), 
        as.numeric(HGD), as.numeric(LGD), as.numeric(IM), as.numeric(NoIM))
    s = c("AllEMRs", "SM2", "SM1", "T1b_Unspecified", "T1a", "HGD", "LGD", "IM", "No IM")
    EMRResult <- data.frame(s, n)
    # axis(1, at=mids)
    barplot(EMRResult$n, names.arg = c("AllEMRs", "SM2", "SM1", "T1b_Unspec", "T1a", "HGD", "LGD", 
        "IM", "No IM"), xlab = "Tissue grade", ylab = "Number of EMRs", cex.lab = 2, cex.axis = 2.5, 
        cex.main = 2.5, cex.names = 2.5, main = "EMR Tissue pathology results")
}




#' BarrettsBasicNumbers
#' 
#' This looks at the basic numbers of all the therapeutic endoscopies over time. 
#' This should only be run after all the BarrettsDataAccord functions.
#' @param x the dataframe
#' @param Endo_ResultPerformed the date the endoscopy was performed
#' @keywords Number of therapies
#' @importFrom dplyr group_by mutate summarise
#' @importFrom magrittr '%>%'
#' @import ggplot2
#' @export
#' @examples 

BarrettsBasicNumbers <- function(x, Endo_ResultPerformed) {
    x <- data.frame(x)
    xNum <- x %>% filter(EVENT != "nothing") %>% mutate(year = year(Endo_ResultPerformed)) %>% 
        group_by(EVENT, year) %>% summarise(n = n())
    xNumPlot <- ggplot(xNum, aes(x = year, y = n, group = EVENT, colour = EVENT)) + geom_line()
    functionResults <- list(ProcNumbers = xNum, ProcNumbersPlot = xNumPlot)
    return(functionResults)
}

#' BarrettsTherapeuticsRFA_ByCatheter
#' 
#' This looks at the basic numbers of RFA by catheter type used.
#' This should only be run after all the BarrettsDataAccord functions.
#' @param EndoSubsetRFA The dataframe
#' @param y report field of interest
#' @param z Another endoscopy report field of interest
#' @keywords RFA, Radiofrequency ablation
#' @export
#' @examples 

BarrettsTherapeuticsRFA_ByCatheter <- function(EndoSubsetRFA, y, z) {
    
    EndoSubsetRFA <- EndoSubsetRFA[EndoSubsetRFA$EVENT == "RFA", ]
    
    HALO90a <- EndoSubsetRFA[grepl("90", EndoSubsetRFA[, y], perl = TRUE), ]
    HALO90b <- EndoSubsetRFA[grepl("90", EndoSubsetRFA[, z], perl = TRUE), ]
    HALO90c <- rbind(HALO90a, HALO90b)
    
    HALO360a <- EndoSubsetRFA[grepl("360", EndoSubsetRFA[, y], perl = TRUE), ]
    HALO360b <- EndoSubsetRFA[grepl("360", EndoSubsetRFA[, z], perl = TRUE), ]
    HALO360c <- rbind(HALO360a, HALO360b)
    
    HALO60a <- EndoSubsetRFA[grepl("HALO60| 60", EndoSubsetRFA[, y], perl = TRUE), ]
    HALO60b <- EndoSubsetRFA[grepl("HALO60| 60", EndoSubsetRFA[, z], perl = TRUE), ]
    HALO60c <- rbind(HALO60a, HALO60b)
    
    HALOTTSa <- EndoSubsetRFA[grepl("TTS|[Cc]hannel", EndoSubsetRFA[, y], perl = TRUE), ]
    HALOTTSb <- EndoSubsetRFA[grepl("TTS|[Cc]hannel", EndoSubsetRFA[, z], perl = TRUE), ]
    HALOTTSc <- rbind(HALOTTSa, HALOTTSb)
    
    HALOAPCa <- EndoSubsetRFA[grepl("APC", EndoSubsetRFA[, y], perl = TRUE), ]
    HALOAPCb <- EndoSubsetRFA[grepl("APC", EndoSubsetRFA[, z], perl = TRUE), ]
    HALOAPCc <- rbind(HALOAPCa, HALOAPCb)
    
    
    n = c(nrow(HALO90c), nrow(HALO360c), nrow(HALO60c), nrow(HALOTTSc), nrow(HALOAPCc))
    s = c("HALO 90", "HALO 360", "HALO 60", "HALO TTS", "APC")
    EMRResult <- data.frame(s, n)
    # axis(1, at=mids, labels=EMRResult%s) axis(1, at=mids)
    barplot(EMRResult$n, names.arg = c("HALO 90", "HALO 360", "HALO 60", "HALO TTS", "APC"), 
        xlab = "Catheter type", ylab = "Number of RFA's", cex.lab = 2, cex.axis = 2.5, cex.main = 2.5, 
        cex.names = 2.5, main = "RFA Catheter type usage")
}




#' Barretts_LesionRecognitionEMR
#' 
#' This looks at the Paris grades of each EMR and then creates a heatmap of pathological grade vs 
#' endoscopic Paris grade.This should only be run after all the BarrettsDataAccord functions.
#' @param EndoSubsetEMR The dataframe
#' @param y Endoscopy report field of interest
#' @param z Another endoscopy report field of interest
#' @keywords Does something with data
#' @import gplots
#' @export
#' @examples 

Barretts_LesionRecognitionEMR <- function(EndoSubsetEMR, y, z) {
    
    EndoSubsetEMR <- EndoSubsetEMR[EndoSubsetEMR$EVENT == "EMR", ]
    
    EndoSubsetEMR$ParisClass <- ifelse(grepl("11a_c|2a_c|[Ii][Ii]a_c", EndoSubsetEMR[, y], perl = TRUE) | 
        grepl("11a_c|2a_c|[Ii][Ii]a_c", EndoSubsetEMR[, z], perl = TRUE), "2a_c", ifelse(grepl("[Ii][Ii]a|2a|11a", 
        EndoSubsetEMR[, y], perl = TRUE) | grepl("[Ii][Ii]a|2a|11a", EndoSubsetEMR[, z], perl = TRUE), 
        "2a", ifelse(grepl("[Ii][Ii]b|2b|11b", EndoSubsetEMR[, y], perl = TRUE) | grepl("[Ii][Ii]b|2b|11b", 
            EndoSubsetEMR[, z], perl = TRUE), "2b", ifelse(grepl("[Ii][Ii]c|2c|11c", EndoSubsetEMR[, 
            y], perl = TRUE) | grepl("[Ii][Ii]c|2c|11c", EndoSubsetEMR[, z], perl = TRUE), "2c", 
            ifelse(grepl("[Ii][Ii][Ii]|III", EndoSubsetEMR[, y], perl = TRUE) | grepl("[Ii][Ii][Ii]|III", 
                EndoSubsetEMR[, z], perl = TRUE), "3", ifelse(grepl("Paris [Tt]ype [Ii]s|1s ", 
                EndoSubsetEMR[, y], perl = TRUE) | grepl("Paris [Tt]ype [Ii]s|1s", EndoSubsetEMR[, 
                z], perl = TRUE), "1s", ifelse(grepl("[Ii]p|1p", EndoSubsetEMR[, y], perl = TRUE) | 
                grepl("[Ii]p|1p", EndoSubsetEMR[, z], perl = TRUE), "1p", "No_Paris")))))))
    
    # Create the matrix
    df3 <- data.frame(EndoSubsetEMR$ParisClass, EndoSubsetEMR$IMorNoIM)
    # Reorganise the column names and rows Get rid of no Paris EMR's
    dfy <- df3[!df3$EndoSubsetEMR.ParisClass == "No_Paris", ]
    # Get the histology proportions by the Paris grade
    tr4 <- as.data.frame.matrix(prop.table(table(dfy), 1))
    
    tr5 <- as.matrix(tr4)
    tr5 <- head(tr5, -1)
    # Create the heatmap par(oma = c(4, 0, 0, 4))
    
    colors = c(seq(-1, 0.2, length = 100), seq(0.21, 0.8, length = 100), seq(0.81, 1, length = 100))
    
    gplots::heatmap.2(tr5, trace = "none", breaks = colors, density.info = "none", dendrogram = "none", 
        Rowv = FALSE, Colv = FALSE, cexRow = 3.5, cexCol = 1.5)
    
}
