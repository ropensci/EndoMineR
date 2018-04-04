
[![Build Status](https://travis-ci.org/sebastiz/EndoMineR.svg?branch=master)](https://travis-ci.org/sebastiz/EndoMineR) [![ropensci](https://badges.ropensci.org/153_status.svg)](https://github.com/ropensci/onboarding/issues/153) [![Coverage status](https://codecov.io/gh/sebastiz/EndoMineR/branch/master/graph/badge.svg)](https://codecov.io/github/sebastiz/EndoMineR?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
<img src="vignettes/img/EndoMineRLogo.png" style="display: block; margin: auto;" />

EndoMineR
=========

The goal of EndoMineR is to extract as much information as possible from endoscopy reports and their associated pathology specimens. The package is intended for use by gastroenterologists, pathologists and anyone interested in the analysis of endoscopic and ppathological datasets

<br>

Gastroenterology now has many standards against which practice is measured although many reporting systems do not include the reporting capability to give anything more than basic analysis. Much of the data is locked in semi-structured text.

<br>

However the nature of semi-structured text means that data can be extracted in a standardised way- it just requires more manipulation. This package provides that manipulation so that complex endoscopic-pathological analyses, in line with recognised standards for these analyses, can be done.

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

Getting started
---------------

<br>

**The data input**

Most datasets will either be raw text so that the entire report contents is one text file. Other datasets will be spreadsheets where the pertinent columns reflecting eg Medication, performing endoscopis etc., will already be separated out. Functions are available for both situations. If the input is a series of raw text files organised as a series of rows (one row per report), then the first function to use, once the data has been inputted, is the extractor function

### The extractor function

One of the most useful functions in the package is the Extractor. Different hospitals will use different software with different headings for endoscopic reports. The extractor allows the user to define the separations in a report so that all reports can be automatically placed into a meaningful dataframe for further cleaning. This is analogous to tokenization in natural language processing. Here we use the in built datasets as part of the package:

``` r
mywords<-c("Hospital Number","Patient Name:","DOB:","General Practitioner:",
"Date received:","Clinical Details:","Macroscopic description:",
"Histology:","Diagnosis:")
Mypath<-Extractor(Mypath,"PathReportWhole",mywords)
```

<br>

This function should be used for both histology and pathology datasets separately.

<br>

### The cleaning function

<br>

Individual cleaning functions are provided for individual columns (for the most likely columns that you might want to analyse- eg Medications, Endoscopist, the Procedure performed etc.). For histopathology similar cleaning functions can be found.

For example, when cleaning the endoscopist name the following function can be used:

``` r
EndoscEndoscopist(Myendo,'Endoscopist')
```

Many such functions for both endoscopy and histology are provided

<br>

Both endoscopy and histology functions can also be found as part of a respective convenience parent function (for endoscopy it is called EndoscAll and for histology it is called HistolAll).

<br>

### The merging function

Once the histology and endoscopy datasets have been cleaned, if you wish (and want to run some of the analysis functions later in the packaed) you can merge the endoscopy and pathology datasets. This has been provided as a convenience function EndoMerge2 and merges the datasets based on the date performed (with some flexibility given pathology received is not always the same as the endoscopy date).

``` r
v<-Endomerge2(Myendo,'Dateofprocedure','HospitalNumber',Mypath,'Dateofprocedure','HospitalNumber')
```

### The analyses

The overall aim is to provide functions that allow the user to perform complex analyses on the endoscopic-pathological datasets. As far as possible the analyses are based on guidelines developed by the British Society of Gastroenterology. These analyses will expand in further iterations. Generic analyses functions are provided for example, as various numeric analyses plotted by endoscopist.

``` r
Myendo<-EndoscMeds(Myendo,'Medications')
Fent<-MetricByEndoscopist(Myendo,'Endoscopist','Fent')
```

More specific analyses, ie those relating to a specific guidelines are also provided. For example in the case of Barrett's oesophagus, the follow-up timing for the next endoscopy in those who have non dysplastic mucosa, can be determined as follows:

``` r
v<-Endomerge2(Myendo,"Dateofprocedure","HospitalNumber",Mypath,"Dateofprocedure","HospitalNumber")
b<-Barretts_PathStage(v,'Histology')
b2<-Barretts_Event(b,'Histology','ProcedurePerformed','OGDReportWhole','Findings') b3<-Barretts_FUGroup(b2,'Findings')
```

Further more detailed examples are provided in the associated vignette for this package
