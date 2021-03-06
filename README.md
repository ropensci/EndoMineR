
[![Build
Status](https://travis-ci.org/sebastiz/EndoMineR.svg?branch=master)](https://travis-ci.org/sebastiz/EndoMineR)
[![ropensci](https://badges.ropensci.org/153_status.svg)](https://github.com/ropensci/onboarding/issues/153)
[![Coverage
status](https://codecov.io/gh/sebastiz/EndoMineR/branch/master/graph/badge.svg)](https://codecov.io/github/sebastiz/EndoMineR?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->

    ## here() starts at /home/rstudio/EndoMineR

This package has undergone a major revision to make it much more user
friendly. THe documentation has been updated to reflect this. I am
always happy to hear of any feedback, positive and negative.

## **Aims of EndoMineR**

The goal of EndoMineR is to extract as much information as possible from
free or semi-structured endoscopy reports and their associated pathology
specimens. A full tutorial can be found
[here](https://docs.ropensci.org/EndoMineR/articles/EndoMineR.html)

## Installation

You can install EndoMineR from github with:

``` r
# install.packages("devtools")
devtools::install_github("ropenSci/EndoMineR")
```

If you dont have access to github, then download the zip and change the
working dirctory to the place you have downloaded it, then do

``` r
setwd("C:/Users/Desktop/")

#On windows you cand cd to change the directory or us pushd to create a temporary directory indtead of cd and then setwd to the temporary directory
unzip("EndoMineR.zip")
file.rename("EndoMineR.zip-master", "EndoMineR.zip")
shell("R CMD build EndoMineR.zip")

#Then install the resulting tarball with:

install.packages("EndoMineR_0.2.0.9000.tar.gz", repos = NULL)
```

### How to contribute

Contributions to this project are most welcome. There are just a few
small guidelines you need to follow.

#### Submitting a patch

It’s generally best to start by opening a new issue describing the bug
or feature you’re intending to fix. Even if you think it’s relatively
minor, it’s helpful to know what people are working on. Mention in the
initial issue that you are planning to work on that bug or feature so
that it can be assigned to you.

Follow the normal process of forking the project, and setup a new branch
to work in. It’s important that each group of changes be done in
separate branches in order to ensure that a pull request only includes
the commits related to that bug or feature.

The best way to ensure your code is properly formatted is to use lint.
Various packages in R provide this.

Any significant changes should almost always be accompanied by tests.
The project already has good test coverage, so look at some of the
existing tests if you’re unsure how to go about it.

Do your best to have well-formed commit messages for each change. This
provides consistency throughout the project, and ensures that commit
messages are able to be formatted properly by various git tools.

Finally, push the commits to your fork and submit a pull request.
Please, remember to rebase properly in order to maintain a clean, linear
git
history.

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
