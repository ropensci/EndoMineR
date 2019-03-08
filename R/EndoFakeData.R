



#' Endomerge
#'
#' Merges endoscopy and histology data. This takes the endoscopy dataset date
#' performed and the hospital number column
#' and merges with the equivalent column in the pathology dataset
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
#' @examples v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',
#' Mypath,'Dateofprocedure','HospitalNumber')

Endomerge2 <-
  function(x,
           EndoDate,
           EndoHospNumber,
           y,
           PathDate,
           PathHospNumber)
  {
    # Rename the columns so can do the join Extract the date from both reports
    colnames(x)[which(names(x) == EndoDate)] <- "Date"
    colnames(x)[which(names(x) == EndoHospNumber)] <- "eHospitalNum"
    x$Date <- gsub("\n", "", x$Date)
    x$Date <- as.Date(x$Date)
    
    colnames(x)[which(names(x) == EndoDate)] <- "Date"
    colnames(x)[which(names(x) == EndoHospNumber)] <- "eHospitalNum"
    colnames(y)[which(names(y) == PathDate)] <- "Date"
    colnames(y)[which(names(y) == PathHospNumber)] <- "pHospitalNum"
    y$Date <- gsub("\n", "", y$Date)
    y$Date <- as.Date(y$Date)
    
    EndoHistoMerge <-
      difference_full_join(y,
                                      x,
                                      by = "Date",
                                      max_dist = 8,
                                      distance_col = "Days")
    eHospitalNuma <- rlang::sym("eHospitalNum")
    pHospitalNuma <- rlang::sym("pHospitalNum")
    
    EndoHistoMerge1<-EndoHistoMerge%>%filter(!!eHospitalNuma == !!pHospitalNuma)
    
    return(EndoHistoMerge1)
  }
