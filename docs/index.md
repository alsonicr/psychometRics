# PsychometRics

PsychometRics is an R package providing functions for psychometric
analysis. The package wraps several helper functions to offer efficient,
fast methods for common psychometric analyses.

## How it helps psychometric analysis

The package offers wrapper functions focused on Classical Test Theory
(CTT) and factor analysis (FA). These functions help identify
potentially weak items by producing diagnostic warnings. Interpret these
warnings and decisions to remove items carefully, as they require
subject-matter judgement.

## Install

``` r
install.packages("devtools")
library(devtools)
install_github("alsonicr/psychometRics")
```

## To do

In order:

1.  Add references to function descriptions and evaluation norms
2.  Add missing-data options (imputation, MICE) for alpha and parallel
    functions
3.  Reformat RAR function to optimize parameters
    - Create helper for multiple selection
    - Test for unused distractors
4.  Create a detect method to provide warnings
5.  Add remaining kwargs and examples
