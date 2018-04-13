---
title: 'EndoMineR for the extraction of endoscopic and associated pathology data from medical reports'
tags:
- example
- tags
- for the paper
authors:
- name: Sebastian S Zeki
  orcid: 0000-0003-1673-2663
  affiliation: "1"

affiliations:
- name: Department of Gastroenterology, St Thomas' Hospital, 
Westminster Bridge Bridge Road, London SE1 7EH
  index: 1
date: 14th April 2018
bibliography: paper.bib
---

# Summary

- A summary describing the high-level functionality and purpose of the software
for a diverse, non-specialist audience

Medical data is increasingly kept in an electronic format worldwide (REF). This serves many purposes including more efficient storage, distribution and accessibility of patient-focussed data. As important is the ability to analyse healthcare data for to optimize resource deployment and usage.  The tools for the analysis are often statistical and rely on the provision of ‘clean’ datasets before this can be done. ‘Cleaning’ a dataset is often the most difficult aspect of any data analysis and involves the provision of meaningful and well-formatted data so that the interpretation of the analysis is not subject to the doubts of the data quality. 

The British Society of Gastroenterology recommends that all endoscopic data is kept in an electronic format particularly to facilitate audit and maintain standards through the Global Rating Scale (GRS) (REF). The endoscopic dataset is however only part of the patient’s story as many aspects of a patient’s gastroenterological care depend on the results of histopathological analysis of tissue taken during the examination. Pathology results are often available many days after the endoscopic result and usually stored in a separate data repository, although this may change with the arrival of an encompassing electronic patient record. 
Regardless of the method of storage, it is often difficult to associate the particular  histopathological result with an endoscopic result. Further, even if the two data sets can be merged, a problem occurs in the isolation of various parts of each report such that each part can be individually analysed.  Examples include the isolation of who the endoscopist was or the presence of dysplasia within a histopathology report. This is all the more difficult if the report is unstructured or partially structured free text. 

However if this can be done then many downstream analyses which benefit individual patients as well as the department, can be automated and include more complex analyses to determine follow-up regimes or endoscopic –pathologic lesion recognition performance.



- A clear statement of need that illustrates the purpose of the software

 It is the purpose of this paper to demonstrate a methodology to merge endoscopy with pathology reports and extract units of data from both. The paper also demonstrates how crucial questions that are asked of datasets repeatedly in endoscopy can be answered in an automated way as long as the dataset is prepared well.
 
- A list of key references including a link to the software archive
- Mentions (if applicable) of any ongoing research projects using the software
or recent scholarly publications enabled by it

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

This is an example citation [@figshare_archive].

Figures can be included like this: ![Fidgit deposited in figshare.](figshare_article.png)

# References