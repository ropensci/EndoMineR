if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "DayDiff"
    )
  )
}



#' Merge endoscopy and histology data.
#'
#'  This takes the endoscopy dataset date
#' performed and the hospital number column
#' and merges with the equivalent column in the pathology dataset. This is
#' merged within a 7 day time frame as pathology is often reported after
#' endoscopic
#' @param x Endoscopy dataframe
#' @param EndoDate The date the endoscopy was performed
#' @param EndoHospNumber The unique hospital number in the endoscopy dataset
#' @param y Histopathology dataframe
#' @param PathDate The date the endoscopy was performed
#' @param PathHospNumber The unique hospital number in the endoscopy dataset
#' @importFrom magrittr '%>%'
#' @importFrom dplyr filter
#' @importFrom fuzzyjoin difference_full_join
#' @importFrom rlang sym
#' @keywords merge endoscopy and histology
#' @export
#' @examples
#' v <- Endomerge2(
#'   Myendo, "Dateofprocedure", "HospitalNumber",
#'   Mypath, "Dateofprocedure", "HospitalNumber"
#' )
Endomerge2 <-
  function(x,
           EndoDate,
           EndoHospNumber,
           y,
           PathDate,
           PathHospNumber) {

    # Rename the columns so can do the join Extract the date from both reports
    colnames(x)[which(names(x) == EndoDate)] <- "Date"
    colnames(x)[which(names(x) == EndoHospNumber)] <- "eHospitalNum"
    x$Date <- gsub("\n", "", x$Date)
    x$Date <- as.Date(x$Date)
    
    colnames(x)[which(names(x) == EndoDate)] <- "Date"
    colnames(x)[which(names(x) == EndoHospNumber)] <- "eHospitalNum"
    x$eHospitalNum <- trimws(gsub("\n", "", x$eHospitalNum))
    
    colnames(y)[which(names(y) == PathDate)] <- "Date"
    colnames(y)[which(names(y) == PathHospNumber)] <- "pHospitalNum"
    names(y)<-gsub("[.]","",names(y))
    y$Date <- gsub("\n", "", y$Date)
    y$Date <- as.Date(y$Date)
    y$pHospitalNum <- trimws(y$pHospitalNum)
    

    
    #Merge by hospital number first as a full join
    EndoHistoMerge<-merge(x, y, by.x=c( "eHospitalNum"),
                          by.y=c( "pHospitalNum"),
                          all.x=TRUE)

    
    #Then create date diff
    EndoHistoMerge$DayDiff<-difftime(EndoHistoMerge$Date.x, EndoHistoMerge$Date.y,units="days")
    
    #Then filter by datediff to get the final list including when no biopsies were taken
    EndoHistoMerge<-EndoHistoMerge%>%filter((DayDiff<7&DayDiff>-1)|is.na(DayDiff))

    return(EndoHistoMerge)
  }
