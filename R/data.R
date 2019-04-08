#' Fake Upper GI Endoscopy Set
#'
#' A dataset containing fake endoscopy reports. The report field is provided as a whole report
#' without any fields having been already extracted
#' 
#'
#' @format A data frame with 2000 rows and 1 variables:
#' \describe{
#'   \item{OGDReportWhole}{The whole report, in text}
#' }
"TheOGDReportFinal"

#' Fake Upper GI Pathology Set
#'
#' A dataset containing fake pathology reports for upper GI endoscopy tissue specimens.
#' The report field is provided as a whole report
#' without any fields having been already extracted
#' 
#'
#' @format A data frame with 2000 rows and 1 variables:
#' \describe{
#'   \item{PathReportWhole}{The whole report, in text}
#' }
"PathDataFrameFinal"


#' Fake Lower GI Endoscopy Set
#'
#' A dataset containing fake lower GI endoscopy reports. The report field is provided as a whole report
#' without any fields having been already extracted
#' 
#'
#' @format A data frame with 2000 rows and 1 variables:
#' \describe{
#'   \item{OGDReportWhole}{The whole report, in text}
#' }
"ColonFinal"

#' Fake Lower GI Endoscopy Set including Pathology
#'
#' A dataset containing fake lower GI endoscopy reports and pathology reports all
#' pre-extracted
#' 
#'
#' @format A data frame with 2000 rows and 26 variables:
#' \describe{
#'   \item{pHospitalNum}{The HospitalNum, in text}
#'   \item{PatientName.x}{The PatientName, in text}
#'   \item{GeneralPractitioner.x}{The GeneralPractitioner report, in text}
#'   \item{Date.x}{The Date, in date}
#'   \item{Endoscopist}{The Endoscopist report, in text}
#'   \item{Secondendoscopist}{The Secondendoscopist report, in text}
#'   \item{Medications}{The Medications report, in text}
#'   \item{Instrument}{The Instrument report, in text}
#'   \item{ExtentofExam}{The ExtentofExam report, in text}
#'   \item{Indications}{The Indications report, in text}
#'   \item{ProcedurePerformed}{The ProcedurePerformed report, in text}
#'   \item{Findings}{The Findings report, in text}
#'   \item{EndoscopicDiagnosis}{The EndoscopicDiagnosis report, in text}
#'   \item{Original.x}{The Original endosocpy report, in text}
#'   \item{eHospitalNum}{The HospitalNum, in text}
#'   \item{PatientName.y}{The PatientName, in text}
#'   \item{DOB}{The DOB, in date}
#'   \item{GeneralPractitioner.y}{The GeneralPractitioner report, in text}
#'   \item{Date.y}{The Date.y , in date}
#'   \item{ClinicalDetails}{The ClinicalDetails report, in text}
#'   \item{Natureofspecimen}{The Natureofspecimen report, in text}
#'   \item{Macroscopicdescription}{The Macroscopicdescription report, in text}
#'   \item{Histology}{The Histology report, in text}
#'   \item{Diagnosis}{The Diagnosis report, in text}
#'   \item{Original.y}{The whole report, in text}
#'   \item{Days}{Days, in numbers}
#' }
"vColon"

#' Fake Lower GI Pathology Set
#'
#' A dataset containing fake pathology reports for lower GI endoscopy tissue specimens.
#' The report field is provided as a whole report
#' without any fields having been already extracted
#' 
#'
#' @format A data frame with 2000 rows and 1 variables:
#' \describe{
#'   \item{PathReportWhole}{The whole report, in text}
#' }
"PathDataFrameFinalColon"

#' Fake Endoscopies
#'
#' A dataset containing fake endoscopy reports. The report fields have already been
#' The report field is derived from the whole report as follows:
#' Myendo<-TheOGDReportFinal
#' Myendo$OGDReportWhole<-gsub('2nd Endoscopist:','Second endoscopist:',Myendo$OGDReportWhole)
#' EndoscTree<-list('Hospital Number:','Patient Name:','General Practitioner:',
#' 'Date of procedure:','Endoscopist:','Second endoscopist:','Medications',
#' 'Instrument','Extent of Exam:','Indications:','Procedure Performed:','Findings:',
#' 'Endoscopic Diagnosis:')
#' for(i in 1:(length(EndoscTree)-1)) {
#'  Myendo<-Extractor(Myendo,'OGDReportWhole',as.character(EndoscTree[i]),
#'  as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
#' }
#' Myendo$Dateofprocedure<-as.Date(Myendo$Dateofprocedure)
#'
#' @format A data frame with 2000 rows and 1 variables:
#' \describe{
#'   \item{OGDReportWhole}{The whole report, in text}
#'   \item{HospitalNumber}{Hospital Number, in text}
#'   \item{PatientName}{Patient Name, in text}
#'   \item{GeneralPractitioner}{General Practitioner, in text}
#'   \item{Dateofprocedure}{Date of the procedure, as date}
#'   \item{Endoscopist}{Endoscopist, in text}
#'   \item{Secondendoscopist}{Secondendoscopist, in text}
#'   \item{Medications}{Medications, in text}
#'   \item{Instrument}{Instrument, in text}
#'   \item{ExtentofExam}{ExtentofExam, in text}
#'   \item{Indications}{Indications, in text}
#'   \item{ProcedurePerformed}{Procedure Performed, in text}
#'   \item{Findings}{Endoscopic findings, in text}
#' }
"Myendo"

#' Fake Pathology report
#'
#' A dataset containing fake pathology reports.
#' The report field is derived from the whole report as follows:
#' Mypath<-PathDataFrameFinalColon
#' HistolTree<-list('Hospital Number','Patient Name','DOB:','General Practitioner:',
#' 'Date of procedure:','Clinical Details:','Macroscopic description:','Histology:','Diagnosis:','')
#' for(i in 1:(length(HistolTree)-1)) {
#'  Mypath<-Extractor(Mypath,'PathReportWhole',as.character(HistolTree[i]),
#'  as.character(HistolTree[i+1]),as.character(HistolTree[i]))
#' }
#' Mypath$Dateofprocedure<-as.Date(Mypath$Dateofprocedure)
#'
#' @format A data frame with 2000 rows and 1 variables:
#' \describe{
#'   \item{PathReportWhole}{The whole report, in text}
#'   \item{HospitalNumber}{Hospital Number, in text}
#'   \item{PatientName}{Patient Name, in text}
#'   \item{DOB}{Date of Birth, in text}
#'   \item{GeneralPractitioner}{General Practitioner, in text}
#'   \item{Dateofprocedure}{Date of the procedure, as date}
#'   \item{ClinicalDetails}{Clinical Details, in text}
#'   \item{Macroscopicdescription}{Macroscopic description of the report, in text}
#'   \item{Histology}{Histology, in text}
#'   \item{Diagnosis}{Diagnosis, in text}
#' }
"Mypath"
