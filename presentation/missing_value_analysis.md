---
title: "Missing value analysis"
author: dr. Iris Eekhout
date: March 5, 2022
output: powerpoint_presentation
---

Missing value analysis
========================================================
author: Dr. Iris Eekhout
date: 5-3-2021
autosize: true 

What are missing values
========================================================

Missing observations are defined as `NA` in R. 

Missing data can have different implications for data summaries, analyses and conclusions based on the data with missing values. 

Amount of missing data
========================================================

* Matrix perspective
* Variable perspective
* Case perspective

Example data
========================================================

A data example is used to demonstrate the methods to explore missing values. 
The example data has 25 rows and 5 columns. 



```
            X1          X2          X3          X4         X5
1   0.41230116          NA -1.45787137 -1.74871275         NA
2   0.14760256  1.21263618  1.22460855  1.16144105  0.8304089
3   1.88943395  1.02193984  1.12186053  1.11436550  1.3265324
4           NA  1.72322066          NA          NA  0.4076068
5  -0.67977599 -0.90319245 -1.00828926 -1.12338735 -1.6435517
6           NA  0.25753152          NA          NA  0.3686427
7   0.32937242  0.53467589  0.35070126  0.47028218 -0.1468478
8  -0.61880057          NA -0.81618849 -0.25146137         NA
9   1.14089485          NA  0.68133003 -0.42495368         NA
10 -0.63665683 -1.27311812  0.70974783 -0.01803123  0.2778070
11 -1.38289659 -1.71690078 -1.78794127 -1.32038202 -1.5665607
12  0.13374468 -0.56895184 -0.02332994 -0.55840361 -0.1340712
13          NA -2.01522814          NA          NA -1.2982912
14 -0.25518586          NA -0.59051561  0.71916078         NA
15  0.03578176          NA  0.25466956 -0.07273832         NA
16 -0.21832163  0.70896950  0.74757649  0.71458257 -0.9769466
17 -0.21516743  0.64185444  0.88095910  0.47813841  1.2712774
18 -0.16622630 -1.15644533 -1.87425292 -0.42087001 -0.8841403
19          NA -0.62467564          NA          NA  0.3624683
20  0.16951236 -0.84708241  0.73033372 -0.28787058 -0.7053414
21  0.67410706 -0.09297161  0.16765174  0.79405342  1.2267761
22 -0.38259181          NA  0.23612307  0.94023662         NA
23  1.28316812  0.78034507  0.30292133  0.39507234  1.0357680
24 -0.82342295 -0.57840871 -0.78624998 -2.28822419 -0.6507763
25  1.13415482 -0.18570873 -0.34605117  0.47172670  0.1097295
```


Matrix perspective
========================================================

**Matrix perspective:** the number of missing entries in the data matrix. 

The `is.na` function returns `TRUE` if a cell is missing (`NA`) and `FALSE` is a cell is observed.

In the example there are 24 missing data entries. The data frame contains 5 variables for 25 subjects, which makes a total of 125 data entries. So, 19.2% of the data entries are missing.  


```r
sum(is.na(datm))
```

```
[1] 24
```

```r
sum(is.na(datm))/length(is.na(datm))
```

```
[1] 0.192
```


Variables perspective
=======================================================

**Variables perspective:** the number of missing values per variable.

For each variable we can count the number of missing observations (n) and calculate the proportion (p).



```r
datm %>%
  is.na %>%
  data.frame() %>%
  summarise_all(list(n = sum, p = mean)) %>%
  pivot_longer(everything(), 
               names_to = c("variable", ".value"),
               names_pattern = "(.*)_(.)")
```

```
# A tibble: 5 x 3
  variable     n     p
  <chr>    <int> <dbl>
1 X1           4  0.16
2 X2           6  0.24
3 X3           4  0.16
4 X4           4  0.16
5 X5           6  0.24
```

Case perspective
========================================================

**Case perspective:** the number of rows, i.e. cases, with missing values.

Many analysis methods only use the rows that are fully observed: complete-case analysis. 

The data are then *listwise* deleted. 


```r
datm %>% 
  is.na %>%
  data.frame() %>%
  mutate(n_miss = rowSums(.),
         missing = ifelse(n_miss > 0, "rows with misings", "rows without missing")) %>%
  group_by(missing) %>%
  summarise(n = n(),
            p = n/ 25)
```

```
# A tibble: 2 x 3
  missing                  n     p
  <chr>                <int> <dbl>
1 rows with misings       10   0.4
2 rows without missing    15   0.6
```


Case perspective - `mice`
===========================================================

The `cci` function in the `mice` package creates and indicator for the number of fully observed rows. 


```r
mice::cci(datm)
```

```
 [1] FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE
[13] FALSE FALSE FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE
[25]  TRUE
```

The `nic` function in the `mice` package counts the number of incomplete cases, i.e. cases with missing values. 


```r
mice::nic(datm)
```

```
[1] 10
```

The `ncc` function in the `mice` package counts the number of complete cases, i.e. cases full fully observed rows.

```r
mice::ncc(datm)
```

```
[1] 15
```


Missing data patterns
===========================================================

**Missing data pattern**: the combination of observed an unobserved values that occur together in a row. 
Generally notated as having a 0 for a missing value and a 1 for an observed value. 

Data often contains multiple different missing data patterns. The example shows three missing data patterns:

1 All variables are observed, so a row of only ones. 
2 Three variables observed ant two missing.
3 Three missing values and two observed. 


```r
mice::md.pattern(datm, plot= F)
```

```
   X1 X3 X4 X2 X5   
15  1  1  1  1  1  0
6   1  1  1  0  0  2
4   0  0  0  1  1  3
    4  4  4  6  6 24
```


The row-names show the number of times the pattern occurs in the data. 
The final column shows the number missing values the missing data pattern holds. 


Missing data pairs
============================================================

**Missing data pair:** the number of times two variables are either missing together or observed together.

This can inform us about how many cases we can actually use for imputation. The `md.pair` function from the `mice` package shows four matrices. Each matrix gives us information about combinations of missing values in our data. 

* `rr`: response-response, the count of how often two variables are both observed.
* `rm`: response-missing, the count of how often the row-variable is observed and the column-variable is missing.
* `mr`: missing-response, the count of how often the row-variable is missing and the column-variable is observed.
* `mm`: missing-missing, the count of how often two variables are both missing. 





```r
pat <- mice::md.pairs(datm)
```


Response-response
===========================================================


```r
pat$rr
```

```
   X1 X2 X3 X4 X5
X1 21 15 21 21 15
X2 15 19 15 15 19
X3 21 15 21 21 15
X4 21 15 21 21 15
X5 15 19 15 15 19
```

Response-missing
===========================================================


```r
pat$rm
```

```
   X1 X2 X3 X4 X5
X1  0  6  0  0  6
X2  4  0  4  4  0
X3  0  6  0  0  6
X4  0  6  0  0  6
X5  4  0  4  4  0
```

Missing-response
===========================================================


```r
pat$mr
```

```
   X1 X2 X3 X4 X5
X1  0  4  0  0  4
X2  6  0  6  6  0
X3  0  4  0  0  4
X4  0  4  0  0  4
X5  6  0  6  6  0
```

Missing-missing
===========================================================


```r
pat$mm
```

```
   X1 X2 X3 X4 X5
X1  4  0  4  4  0
X2  0  6  0  0  6
X3  4  0  4  4  0
X4  4  0  4  4  0
X5  0  6  0  0  6
```


Information for imputation
===========================================================

The proportion missing-response from the sum of the missing-response and missing-missing matrices shows how many usable cases the data have to impute the row variable from the column variable. 


```r
round(100 * pat$mr / (pat$mr + pat$mm))
```

```
    X1  X2  X3  X4  X5
X1   0 100   0   0 100
X2 100   0 100 100   0
X3   0 100   0   0 100
X4   0 100   0   0 100
X5 100   0 100 100   0
```

