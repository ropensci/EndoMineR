


#' Endomerge
#'
#' Merges endoscopy and histology data. This takes the endoscopy dataset date performed and the hospital number column
#' and merges with the equivalent column in the pathology dataset
#' @param x Endoscopy dataframe
#' @param EndoDate The date the endoscopy was performed
#' @param EndoHospNumber The unique hospital number in the endoscopy dataset
#' @param y Histopathology dataframe
#' @param PathDate The date the endoscopy was performed
#' @param PathHospNumber The unique hospital number in the endoscopy dataset
#' @keywords merge endoscopy and histology
#' @export
#' @examples v<-TheOGDReportFinal
#' Myendo<-TheOGDReportFinal
#' Myendo$OGDReportWhole<-gsub("2nd Endoscopist:","Second endoscopist:",Myendo$OGDReportWhole)
#' EndoscTree<-list("Hospital Number:","Patient Name:","General Practitioner:",
#' "Date of procedure:","Endoscopist:","Second Endoscopist:","Medications",
#' "Instrument","Extent of Exam:","Indications:","Procedure Performed:","Findings:",
#' "Endoscopic Diagnosis:")
#' for(i in 1:(length(EndoscTree)-1)) {
#'  Myendo<-Extractor(Myendo,"OGDReportWhole",as.character(EndoscTree[i]),
#'  as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
#' }
#' Myendo$Dateofprocedure<-as.Date(Myendo$Dateofprocedure)
#' Mypath<-PathDataFrameFinalColon
#' HistolTree<-list("Hospital Number","Patient Name","DOB:","General Practitioner:",
#' "Date of procedure:","Clinical Details:","Macroscopic description:","Histology:","Diagnosis:","")
#' for(i in 1:(length(HistolTree)-1)) {
#'  Mypath<-Extractor(Mypath,"PathReportWhole",as.character(HistolTree[i]),
#'  as.character(HistolTree[i+1]),as.character(HistolTree[i]))
#' }
#' Mypath$Dateofprocedure<-as.Date(Mypath$Dateofprocedure)
#' v<-HistolChopperDx(Mypath,"Diagnosis")
#' v<-HistolChopperExtrapolDx(v,"Diagnosis")
#' v<-HistolChopperNumbOfBx(v,"Macroscopicdescription","specimen")
#' v<-HistolChopperBxSize(v,"Macroscopicdescription")
#' v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",v,"Dateofprocedure","HospitalNumber")

Endomerge2 <- function(x,EndoDate,EndoHospNumber,y,PathDate,PathHospNumber) {
    # Rename the columns so can do the join Extract the date from both reports
    #x$Date <- str_match(x$PathReportWhole, "Date received:  .*")
    #x$Date <- as.Date(gsub("Date received:  ", "", x$Date))
    #x$pHospitalNum <- str_match(x$PathReportWhole, "Hospital Number: .*")
    #x$pHospitalNum <- as.character(gsub("Hospital Number: ", "", x$pHospitalNum))
  colnames(x)[which(names(x) == EndoDate)]<-"Date"
  colnames(x)[which(names(x) == EndoHospNumber)]<-"eHospitalNum"
  colnames(y)[which(names(y) == PathDate)]<-"Date"
  colnames(y)[which(names(y) == PathHospNumber)]<-"pHospitalNum"
    #y$Date <- str_match(y$OGDReportWhole, "Date of procedure.*")
    #y$Date <- as.Date(gsub("Date of procedure:  ", "", y$Date))
    #y$eHospitalNum <- str_match(y$OGDReportWhole, "Hospital Number.*")
    #y$eHospitalNum <- as.character(gsub("Hospital Number: ", "", y$eHospitalNum))
    
    
    EndoHistoMerge <- fuzzyjoin::difference_full_join(y, x, by = "Date", max_dist = 2, distance_col = "Days") %>% 
        dplyr::filter(eHospitalNum == pHospitalNum)
    return(EndoHistoMerge)
}
