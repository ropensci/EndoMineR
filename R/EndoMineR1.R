#' EndoMineR: A package for analysis of endoscopic and related pathology
#'
#' The goal of EndoMineR is to extract as much information as possible from 
#' endoscopy reports and their associated pathology specimens. The package 
#' is intended for use by gastroenterologists, pathologists and anyone 
#' interested in the analysis of endoscopic and ppathological datasets
#' Gastroenterology now has many standards against which practice is measured 
#' although many reporting systems do not include the reporting capability to 
#' give anything more than basic analysis. Much of the data is locked in 
#' semi-structured text.However the nature of semi-structured text means that 
#' data can be extracted in a standardised way- it just requires more 
#' manipulation. This package provides that manipulation so that complex 
#' endoscopic-pathological analyses, in line with recognised standards for 
#' these analyses, can be done.The package is basically in three parts/
#' 
#' \itemize{
#' \item The extraction- This is really when the data is provided as full text 
#' reports. You may already have the data in a spreadsheet in which case 
#' this part isn't necessary.
#' 
#' \item Cleaning- These are a group of functions 
#' that allow the user to extract and clean data commonly found in endoscopic 
#' and pathology reports. The cleaning functions usually remove common typos or 
#' extraneous information and do some reformatting.
#' 
#' \item  Analyses- The analyses provide graphing function as well as analyses 
#' according to the cornerstone questions in gastroenterology- namely 
#' surveillance, patient tracking, quality of endoscopy and pathology 
#' reporting and diagnostic yield questions.
#' }
#' 
#' To learn more about EndoMineR, start with the vignettes:
#' `browseVignettes(package = "EndoMineR")`
#'
#' @docType package
#' @name EndoMineR
NULL