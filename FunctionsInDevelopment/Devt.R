# Functions to develop
# Need to figure out how to extract all the polyps per endoscopy(don't need to know how many removed, just which locations they came from)

# Determine the location of biopsies


#' PouchTidyUpLocator
#' Standardises the location of biopsies for pouchitis assessment
#' @param The dataframe=x, SampleLocation
#' @keywords Pouch
#' @export
#' @examples
#' PouchTidyUpLocator()
PouchTidyUpLocator <- function(x,SampleLocation) {
  # Get all the polyps and tidy up the polyp data- Function 5
  x$Pouch <- gsub("[Bb]iops.*?[A-Z]", "", x[,SampleLocation])
  x$Pouch <- gsub("[Bb]iops.*(\\s+)?", "", x[,SampleLocation])
  x$Pouch <- gsub("=", "", x$Pouch)
  x$Pouch <- gsub("\\.", "", x$Pouch)
  x$Pouch <- gsub("[Cc][Mm]", "", x$Pouch)
  x$Pouch <- gsub("[Mm]{2}", "", x$Pouch)
  x$Pouch <- gsub("[Bb]x", "", x$Pouch)
  x$Pouch <- gsub("[Bb]iopsies|[Bb]iopsy|[Bb]io.*?(y|ies)", "", x$Pouch)
  x$Pouch <- gsub("[Aa]ll |[Aa]bove", "", x$Pouch)
  x$Pouch <- gsub("[Aa]nal", "Anal", x$Pouch)
  x$Pouch <- gsub("[Ss]ite", "", x$Pouch)
  x$Pouch <- gsub("[Rr]andom", "", x$Pouch)
  x$Pouch <- gsub("[Tt]erminal [Ii]leum", "", x$Pouch)
  x$Pouch <- gsub("(b$|b\\s)", "", x$Pouch)
  x$Pouch <- gsub("-", "", x$Pouch, fixed = T)
  x$Pouch <- gsub("+", "", x$Pouch, fixed = T)
  x$Pouch <- gsub("[Pp].*?ch", "Pouch", x$Pouch)
  x$Pouch <- gsub("([a-z])[Pp]ouch", "\\1 Pouch", x$Pouch)
  x$Pouch <- gsub(".*[Aa]nastomosis.*", "Anastomosis", x$Pouch)
  x$Pouch <- gsub(".*[Aa]fferent.*", "Afferent", x$Pouch)
  x$Pouch <- gsub("  ", " ", x$Pouch)
  x$Pouch <- gsub("[Ii]leum Anal", "[Ii]leoAnal", x$Pouch, fixed = T)
  x$Pouch <- gsub("[Pp]ouch Ileum", "[Ii]leum [Pp]ouch", x$Pouch, fixed = T)
  x$Pouch <- gsub("[Ii]leoAnal [Pp]ouch", "[Ii]leoAnal", x$Pouch, fixed = T)
  x$Pouch <- gsub("[Ii]leo ", "[Ii]leoAnal", x$Pouch, fixed = T)
  x$Pouch <- gsub("[Ii]leum [Pp]ouch", "[Ii]leum", x$Pouch, fixed = T)
  x$Pouch <- gsub("smbowel", "[Ii]leum", x$Pouch, fixed = T)
  x$Pouch <- gsub("[Pp]ouch [Cc]olon", "[Pp]ouch", x$Pouch, fixed = T)
  # This mops up whatever is left
  x$Pouch <- gsub("[Pp]ouch .*", "Pouch", x$Pouch)
  x$Pouch <- gsub("[Pp][Rr][Ee]\\s*[Pp][Oo][Uu][Cc][Hh] .*", "Pre-pouch", x$Pouch)
  x$Pouch <- gsub("[Ii]leum.*", "[Ii]leum", x$Pouch)
  x$Pouch <- gsub(".*[Ii]leo[Aa]nal.*", "IleoAnal", x$Pouch)
  x$Pouch <- gsub(".*[Rr]ectum.*", "Rectum", x$Pouch)
  x$Pouch <- trimws(x$Pouch, which = c("both"))
  x<-x%>%filter(x$Pouch!="character(0)")
  xForPouch <- x[!is.na(x$Pouch), ]
}
