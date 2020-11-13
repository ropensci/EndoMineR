#IBD Functions

#' Cleans medication column if present
#'
#' This extracts all of the relevant IBD scores where present
#' from the medical text.
#' @param inputColumn1 column of interest as a string vector
#' @keywords IBD scores
#' @return This returns a dataframe with all the scores in it
#' @importFrom stringr str_extract str_match
#' @export
#' @examples
#'  df<-read.csv("/Users/sebastianzeki/Documents/AnonKCHendo6m.csv",stringsAsFactors = FALSE)
#' IBD_Scores(df$ER_FINDINGS_STR)
#' MydfNew <- cbind(IBD_Scores(df$ER_FINDINGS_STR), df)
IBD_Scores <- function(inputColumn1) {
  df <- data.frame(inputColumn1, stringsAsFactors = FALSE)
  inputColumn1<-tolower(inputColumn1)
  df$UCEIS<-str_extract(inputColumn1,"uceis.*?(?<![a-z])[0-9]")
  df$SCCAI<-str_match(inputColumn1,"sccai.*")
  df$UCEIS<-str_match(inputColumn1,"uceis.*?(?<![a-z])([0-9])")
  df$Mayo<-str_match(inputColumn1,"mayo.*?(\\d)")
  df$Rutgerts<-str_match(inputColumn1,"rutg.*?(\\d)")
  
  #Regex to search for:
  df$SES<-stringr::str_extract(IBD_AllAssess$SearchTarget,"ses([:punct:]| )+")
  
  #Standardise it:
  df$SES<-(gsub("ses.*","ses",df$SES))
  
  #Extract the sesScore where possible:
  df$SES_Score<-stringr::str_extract(df$SearchTarget,"total.*?= *\\d+")
  
  #Extract the SES CD when only followed by a number but not as an arithmetic + + + situation
  df$SES_Score2<-stringr::str_extract(df$SearchTarget,"ses *- *cd.*?\\d(?!\\+)")
  df$SES_Score3<-stringr::str_extract(df$SearchTarget,"ses.*?cd.*?\\d(?!\\+)")
  df$SES_Score4<-stringr::str_extract(df$SearchTarget,"sescd.*?\\d(?! \\+)")
  
  df$FinalScore<-ifelse(is.na(df$SES_Score),df$SES_Score2,df$SES_Score)
  df$FinalScore<-ifelse(is.na(df$FinalScore),df$SES_Score3,df$FinalScore)
  df$FinalScore<-ifelse(is.na(df$FinalScore),df$SES_Score4,df$FinalScore)
  
  
  #Now clean up the final score:
  df$FinalScore<-gsub(".*=","",df$FinalScore)
  df$FinalScore<-gsub(".*cd","",df$FinalScore)
  df$FinalScore<-ifelse(grepl("[a-z]+",df$FinalScore),NA,df$FinalScore)
  return(df)
  
  
}

