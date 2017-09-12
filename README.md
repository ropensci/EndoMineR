
<!-- README.md is generated from README.Rmd. Please edit that file -->
EndoMineR
=========

The goal of EndoMineR is to extract as much information as possible from endoscopy reports and their associated pathology specimens. Gastroenterology now has many standards against which practice is measured although many reporting systems do not include the reporting capability to give anything more than basic analysis. Much of the data is locked in semi-structured text. However the nature of semi-structured text means that data can be extracted in a standardised way- it just requires more manipulation. This package provides that manipulation so that complex endoscopic-pathological analyses, in line with recognised standards for these analyses, can be done.

The package is basically in three parts

1.  **The extraction**- This is really when the data is provided as full text reports. You may already have the data in a spreadsheet in which case this part isn't necessary

2.  **Cleaning**- These are a group of functions that allow the user to extract and clean data commonly found in endoscopic and pathology reports. The cleaning functions usually remove common typos or extraneous information and do some reformatting

3.  **Analyses**- The analyses provide graphing function as well as analyses according to the cornerstone questions in gastroenterology- namely surveillance, patient tracking, quality of endoscopy and pathology reporting and diagnostic yield questions.

Installation
------------

You can install EndoMineR from github with:

``` r
# install.packages("devtools")
devtools::install_github("sebastiz/EndoMineR")
```

The extractor function
----------------------

One of the most useful functions in the package is the Extractor. Different hospitals will use different software with different headings for endoscopic reports. The extractor allows the user to define the separations in a report so that all reports can be automatically placed into a meaningful dataframe for further cleaning. This is analogous to tokenization in natural language processing. Here we use the in built datasets as part of the package:

``` r
Mypath<-data(PathDataFrameFinalColon)
HistolTree<-list("Hospital Number","Patient Name","DOB:","General Practitioner:",
"Date of procedure:","Clinical Details:","Macroscopic description:","Histology:","Diagnosis:","")
for(i in 1:(length(HistolTree)-1)) {
Mypath<-Extractor(Mypath,"PathReportWhole",as.character(HistolTree[i]),
as.character(HistolTree[i+1]),as.character(HistolTree[i]))
}
```

This function can be used for both histology and pathology datasets.

The cleaning function
---------------------

Data cleaning is tricky so that the cleaning functions have been developed on the basis of recurring patterns of cleaning that needs to be done for these kinds of reports. For example, when cleaning the endoscopist name the following function can be used:

``` r
EndoscChopperEndoscopist(Myendo,'Endoscopist')
```

Many such functions for both endoscopy and histology are provided

The analyses
------------

The overall aim is to provide functions that allow the user to perform complex analyses on the endoscopic-pathological datasets. As far as possible the analyses are based on guidelines developed by the British Society of Gastroenterology. These analyses will expand in further iterations. Generic analyses functions are provided for example, as various numeric analyses plotted by endoscopist.

``` r
Myendo<-EndoscChopperMeds(Myendo,'Medications')
Fent<-MetricByEndoscopist(Myendo,'Endoscopist','Fent')
```

More specific analyses, ie those relating to a specific guidelines are also provided. For example in the case of Barrett's oesophagus, the follow-up timing for the next endoscopy in those who have non dysplastic mucosa, can be determined as follows:

``` r
#' v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",Mypath,"Dateofprocedure","HospitalNumber")
#' b<-BarrettsDataAccord_PathStage(v,'Histology')
#' b2<-BarrettsDataAccord_Event(b,'Histology',
#' 'ProcedurePerformed','OGDReportWhole','Findings')
#' b3<-BarrettsDataAccord_FUGroup(b2,'Findings')
```

Further more detailed examples are provided in the associated vignette for this package
