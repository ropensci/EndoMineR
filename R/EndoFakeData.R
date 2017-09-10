


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
#' @examples 
#' EndoscopyData<-Endoscopies()
#' HistopathologyData<-Histop_df()
#' Endomerge(EndoscopyData,HistopathologyData)

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
