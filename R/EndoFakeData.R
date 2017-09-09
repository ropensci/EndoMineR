


#' Endomerge
#'
#' Merges endoscopy and histology data
#' @param x Endoscopy dataframe
#' @param y Histopathology dataframe
#' @keywords merge endoscopy and histology
#' @export
#' @examples 
#' EndoscopyData<-Endoscopies()
#' HistopathologyData<-Histop_df()
#' Endomerge(EndoscopyData,HistopathologyData)

Endomerge2 <- function(x, y) {
    # Rename the columns so can do the join Extract the
    # date from both reports
    x$Date <- str_match(x$PathReportWhole, "Date received:  .*")
    x$Date <- as.Date(gsub("Date received:  ", "", 
        x$Date))
    x$pHospitalNum <- str_match(x$PathReportWhole, 
        "Hospital Number: .*")
    x$pHospitalNum <- as.character(gsub("Hospital Number: ", 
        "", x$pHospitalNum))
    
    y$Date <- str_match(y$OGDReportWhole, "Date of procedure.*")
    y$Date <- as.Date(gsub("Date of procedure:  ", 
        "", y$Date))
    y$eHospitalNum <- str_match(y$OGDReportWhole, "Hospital Number.*")
    y$eHospitalNum <- as.character(gsub("Hospital Number: ", 
        "", y$eHospitalNum))
    
    
    EndoHistoMerge <- fuzzyjoin::difference_full_join(y, 
        x, by = "Date", max_dist = 2, distance_col = "Days") %>% 
        dplyr::filter(eHospitalNum == pHospitalNum)
    return(EndoHistoMerge)
}
