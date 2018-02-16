% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BarrettsFunctions.R
\name{BarrettsBxQual}
\alias{BarrettsBxQual}
\title{Barrett's number of biopsies}
\usage{
BarrettsBxQual(dataframe, Endo_ResultPerformed, PatientID, Endoscopist)
}
\arguments{
\item{dataframe}{dataframe}

\item{Endo_ResultPerformed}{Date of the Endocscopy}

\item{PatientID}{Patient's unique identifier}

\item{Endoscopist}{name of the column with the Endoscopist names}
}
\description{
This function gets the biopsies taken per endoscopy and compares to the
Prague score for that endoscopy.
}
\examples{
# Firstly relevant columns are extrapolated from the
# Mypath demo dataset. These functions are all part of Histology data
# cleaning as part of the package.
v<-HistolDx(Mypath,'Diagnosis')
v<-HistolExtrapolDx(v,'Diagnosis')
v<-HistolNumbOfBx(v,'Macroscopicdescription','specimen')
v<-HistolBxSize(v,'Macroscopicdescription')
# The histology is then merged with the Endoscopy dataset. The merge occurs
# according to date and Hospital number
v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',v,'Dateofprocedure',
'HospitalNumber')
# The function relies on the other Barrett's functions being run as well:
b1<-Barretts_PragueScore(v,'Findings')
b2<-Barretts_PathStage(b1,'Histology')
b3<-Barretts_EventType(b2,'Histology','ProcedurePerformed',
'Indications','Findings')
# The follow-up group depends on the histology and the Prague score for a
# patient so it takes the processed Barrett's data and then looks in the
# Findings column for permutations of the Prague score.
b4<-Barretts_FUType(b3,'Findings')
colnames(b4)[colnames(b4) == 'pHospitalNum'] <- 'HospitalNumber'
# The number of average number of biopsies is then calculated and
# compared to the average Prague C score so that those who are taking
# too few biopsies can be determined
BarrettsBxQual(b4,'Date.x','HospitalNumber',
                                     'Endoscopist')
rm(v)
}
\keyword{Does}
\keyword{data}
\keyword{something}
\keyword{with}