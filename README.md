#dmo: Don't Miss Out

This package contains functions and scripts to help work and deal with missing data. Most parts were made for the Don't Miss Out! project that focussed on the valuable information missing data can hold. In this project methods to deal with missing data, in mainly questionnaire data, were investigated. 

##Installation

```{r}
install.packages("devtools")
devtools::install_github(repo = "iriseekout/dmo")
```

##Overview

The `dmo` package contains:

* functions to generate missing data according the MCAR, MAR or MNAR mechanisms.
* a shiny application to demonstrate the effect of different mssing data mechanisms.
* a demonstration on how to apply passive multiple imputation in `mice`.

##Main functions

