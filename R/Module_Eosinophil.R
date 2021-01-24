if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "EosinophilYN"
    )
  )
}
###### Eosinophilic-specific extrapolation Functions ######

#' Extract the Prague score
#'
#' The aim is to extract a C and M stage (Prague score) for Barrett's samples.
#' This is done using a regex where C and M stages are explicitly mentioned in
#' the free text
#' Specfically it extracts the Prague score
#' @param dataframe dataframe with column of interest
#' @param findings column of interest
#' @param histol second column of interest
#' @param IndicationsFroExamination second column of interest
#' @importFrom stringr str_extract str_replace str_extract_all
#' @keywords  Eosinophilic Oesophagitis EoE
#' @export 
#' @family Disease Specific Analysis - Eosinophilic Data
#' @examples
#' # Firstly relevant columns are extrapolated from the
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' # Mypath demo dataset. These functions are all part of Histology data
#' # cleaning as part of the package.
#' v <- Mypath
#' v$NumBx <- HistolNumbOfBx(v$Macroscopicdescription, "specimen")
#' v$BxSize <- HistolBxSize(v$Macroscopicdescription)
#' # The histology is then merged with the Endoscopy dataset. The merge occurs
#' # according to date and Hospital number
#' v <- Endomerge2(
#'   Myendo, "Dateofprocedure", "HospitalNumber", v, "Dateofprocedure",
#'   "HospitalNumber"
#' )
#' 
#' 
#' aa <- Eosinophilics(v, "Findings", "Histology","Indications")


Eosinophilics<-function(dataframe,findings,histol,IndicationsFroExamination){
  
  dataframe$Eosinoph<-str_extract_all(dataframe[,histol],"[Ee]osinophil")
  dataframe$EoE_EndoFindings<-str_extract_all(dataframe[,findings],"[Tt]racheal|[Rr]idg|[Ff]urrow|[Nn]arrow|[Pp]ipe|[Pp]laqu|[Ww]hit|[Ss]trict|[Rr]ing|[Ee]xud|edema|[Ss]chat|[Ss]hat|\\b(?=\\w*tz)\\w+\\b|[Cc]andi|[Dd]ebris|[Oo]esophagit")
  dataframe$HPF<-as.numeric(sapply(str_extract_all(dataframe[,histol], "[0-9]+(?=(\\s+[Ii]ntraepithelial)?(\\s+[Ee]osinophils)?\\s+(per|in one|in a|\\/)?.+([Hh][Pp][Ff]|[Hh]igh.+power.+field|[Hh]ighpower\\s+field))"), 
                                   function(x) x[which.max(as.numeric(x))][1]))
  dataframe$EoE_Histol<-str_extract_all(dataframe[,histol],".*edema.*|[Mm]icroabscesses|[Mm]icro-abscesses|[Ss]pongiosi.*|[Hh]yperpl.*|[Ff]ibro|[Nn]eutrophil|[Ll]ymphocyte")
  
  #Some cleaning again
  dataframe$EoE_Histol<-unlist(lapply(dataframe$EoE_Histol,function(x) paste0(x,collapse=" ; ")))
  dataframe$EoE_EndoFindings<-unlist(lapply(dataframe$EoE_EndoFindings,function(x) paste0(x,collapse=" ; ")))
  dataframe$Eosinoph<-unlist(lapply(dataframe$Eosinoph,function(x) paste0(x,collapse=" ; ")))
  
  dataframe$Eosinoph<-ifelse(is.na(dataframe$Eosinoph),  "N", "Y")
  dataframe$Hiatus<-str_extract(dataframe[,findings],"hiatu")
  dataframe$Hiatus<-ifelse(is.na(dataframe$Hiatus),  "N", "Y")
  dataframe$Schatzki<-str_extract(dataframe$EoE_EndoFindings,"schat")
  dataframe$Schatzki<-ifelse(is.na(dataframe$Schatzki),  "N", "Y")
  dataframe$Furrow<-str_extract(dataframe$EoE_EndoFindings,"furrow")
  dataframe$Furrow<-ifelse(is.na(dataframe$Furrow),  "N", "Y")
  dataframe$Edema<-str_extract(dataframe$EoE_EndoFindings,"edema")
  dataframe$Edema<-ifelse(is.na(dataframe$Edema),  "N", "Y")
  dataframe$Stricture<-str_extract(dataframe$EoE_EndoFindings,"strict|narrow")
  dataframe$Stricture<-ifelse(is.na(dataframe$Stricture),  "N", "Y")
  dataframe$Tracheal<-str_extract(dataframe$EoE_EndoFindings,"ring|tracheal")
  dataframe$Tracheal<-ifelse(is.na(dataframe$Tracheal),  "N", "Y")
  dataframe$WhiteSpots<-str_extract(dataframe$EoE_EndoFindings,"exud|whit")
  dataframe$WhiteSpots<-ifelse(is.na(dataframe$WhiteSpots),  "N", "Y")
  dataframe$Normal<-ifelse(dataframe$EoE_EndoFindings=="","Normal","Abnormal")
  dataframe$Normal<-ifelse(is.na(dataframe$Normal), "N", "Y")
  
  #Indications extraction:
  dataframe$DysphagiaIndication<-ifelse(grepl("dysphagi|odyno|bolus",dataframe[,IndicationsFroExamination]),"Dysphagia","No dysphagia")
  dataframe$RefluxIndication<-ifelse(grepl("reflux|atypi|gord|dyspepsia|barrett",dataframe[,IndicationsFroExamination]),"Reflux","No reflux")
  
  
  
  #Create labels:
  attr(dataframe$Eosinoph, "label") <- "Eosinophilic Oesophagitis"
  attr(dataframe$Hiatus, "label") <- "Hiatus"
  attr(dataframe$Schatzki, "label") <- "Schatzki ring"
  attr(dataframe$Furrow, "label") <- "Furrow"
  attr(dataframe$Edema, "label") <- "Oedema"
  attr(dataframe$Stricture, "label") <- "Stricture"
  attr(dataframe$Tracheal, "label") <- "Trachealisation"
  attr(dataframe$WhiteSpots, "label") <- "White spots"
  attr(dataframe$Normal, "label") <- "Normal endoscopy"
  attr(dataframe$DysphagiaIndication, "label") <- "Dysphagia"
  attr(dataframe$RefluxIndication, "label") <- "Reflux"
  
  return(dataframe)
}


