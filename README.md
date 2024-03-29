<!-- README.md is generated from README.Rmd. Please edit that file -->

\#dmo: Don’t Miss Out

This package contains functions and scripts to help work and deal with
missing data. Most parts were made for the Don’t Miss Out! project that
focussed on the valuable information incomplete data can hold. In this
project methods to deal with missing data, in mainly questionnaire data,
were investigated.

\#\#Installation

``` r
install.packages("devtools")
devtools::install_github(repo = "iriseekhout/dmo")
```

\#\#Overview

The `dmo` package contains:

-   functions to generate missing data according the MCAR, MAR or MNAR
    mechanisms.
-   a shiny application to demonstrate the effect of different mssing
    data mechanisms.
-   a demonstration on how to apply passive multiple imputation in
    `mice`.

\#\#Main functions The main functions in the `dmo` package are:

| Function name | Description                                 |
|---------------|---------------------------------------------|
| `MCAR()`      | Generate missing observations that are MCAR |
| `MAR()`       | Generate missing observations that are MAR  |
| `MNAR()`      | Generate missing observations that are MNAR |
