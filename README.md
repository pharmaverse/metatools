
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metatools <a href='https://github.com/pharmaverse/metatools'><img src="man/figures/metatools.png" align="right" style="height:139px;"/></a>

<!-- badges: start -->

[<img src="http://pharmaverse.org/shields/metatools.svg">](https://pharmaverse.org)
[![R-CMD-check](https://github.com/pharmaverse/metatools/workflows/R-CMD-check/badge.svg)](https://github.com/pharmaverse/metatools/actions)
[![codecov](https://codecov.io/gh/pharmaverse/metatools/branch/main/graph/badge.svg?token=55N5APFLPA)](https://codecov.io/gh/pharmaverse/metatools)
[<img src="https://img.shields.io/badge/License-MIT-blue.svg">](https://github.com/pharmaverse/metatools/blob/main/LICENSE)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of metatools is to enable the use of metacore objects.
Metatools can be used to build datasets or enhance columns in existing
datasets as well as checking datasets against the metadata in metacore.

## Installation

You can install the released version of metatools from
[GitHub](https://github.com/) with:

``` r
install.packages("metatools")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gsk-tech/metatools")
```

## Example

Here is a basic example of some functions to build datasets and create
new columns.

``` r
library(metatools)
library(metacore)
library(haven)
library(magrittr)
metacore <- define_to_metacore(metacore_example("ADaM_define.xml"), quiet = TRUE) %>%
  select_dataset("ADSL")
#> 
#>  Metadata successfully imported
#> Loading in metacore object with suppressed warnings
ds_list <- list(DM = read_xpt(metatools_example("dm.xpt")))

build_from_derived(metacore, ds_list) %>% # To pull in columns from DM to be in ADSL
   create_cat_var(metacore, AGE, AGEGR1, AGEGR1N) %>% #Add an AGEGR1 and AGEGR1N column
   convert_var_to_fct(metacore, ETHNIC) # Change ETHNIC to as factor 
#> # A tibble: 306 x 15
#>    STUDYID  USUBJID  SUBJID SITEID TRT01P    AGE AGEU  RACE  SEX   ETHNIC  DTHFL
#>    <chr>    <chr>    <chr>  <chr>  <chr>   <dbl> <chr> <chr> <chr> <fct>   <chr>
#>  1 CDISCPI~ 01-701-~ 1015   701    Placebo    63 YEARS WHITE F     HISPAN~ ""   
#>  2 CDISCPI~ 01-701-~ 1023   701    Placebo    64 YEARS WHITE M     HISPAN~ ""   
#>  3 CDISCPI~ 01-701-~ 1028   701    Xanome~    71 YEARS WHITE M     NOT HI~ ""   
#>  4 CDISCPI~ 01-701-~ 1033   701    Xanome~    74 YEARS WHITE M     NOT HI~ ""   
#>  5 CDISCPI~ 01-701-~ 1034   701    Xanome~    77 YEARS WHITE F     NOT HI~ ""   
#>  6 CDISCPI~ 01-701-~ 1047   701    Placebo    85 YEARS WHITE F     NOT HI~ ""   
#>  7 CDISCPI~ 01-701-~ 1057   701    Screen~    59 YEARS WHITE F     HISPAN~ ""   
#>  8 CDISCPI~ 01-701-~ 1097   701    Xanome~    68 YEARS WHITE M     NOT HI~ ""   
#>  9 CDISCPI~ 01-701-~ 1111   701    Xanome~    81 YEARS WHITE F     NOT HI~ ""   
#> 10 CDISCPI~ 01-701-~ 1115   701    Xanome~    84 YEARS WHITE M     NOT HI~ ""   
#> # ... with 296 more rows, and 4 more variables: RFSTDTC <chr>, RFENDTC <chr>,
#> #   AGEGR1 <chr>, AGEGR1N <chr>
```

Metatools can also be used to run checks

``` r
data <- read_xpt(metatools_example("adsl.xpt"))
#Checking all values are in the control terminology for just TRT01PN
check_ct_col(data, metacore, TRT01PN)
#> [1] TRUE
#Checking all values are in the control terminology for all variables 
check_ct_data(data, metacore, TRUE)
#> [1] TRUE
# Check all variables in the metadata are in the dataset and there aren't any extra columns 
check_variables(data, metacore)
#> No missing or extra variables
#> [1] TRUE
```
