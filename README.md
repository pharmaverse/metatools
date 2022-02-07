
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
devtools::install_github("pharmaverse/metatools")
```

## Example

Here is a basic example of some functions to build datasets and create
new columns.

``` r
library(metatools)
library(metacore)
library(haven)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
load(metacore_example("pilot_ADaM.rda"))
metacore <- metacore %>%
  select_dataset("ADSL")
ds_list <- list(DM = read_xpt(metatools_example("dm.xpt")))

build_from_derived(metacore, ds_list) %>% # To pull in columns from DM to be in ADSL
   select(USUBJID, AGE, ETHNIC) %>% 
   create_cat_var(metacore, AGE, AGEGR1, AGEGR1N) %>% #Add an AGEGR1 and AGEGR1N column
   convert_var_to_fct(metacore, ETHNIC) # Change ETHNIC to as factor 
#> # A tibble: 306 x 5
#>    USUBJID       AGE ETHNIC                 AGEGR1 AGEGR1N
#>    <chr>       <dbl> <fct>                  <chr>  <chr>  
#>  1 01-701-1015    63 HISPANIC OR LATINO     <65    1      
#>  2 01-701-1023    64 HISPANIC OR LATINO     <65    1      
#>  3 01-701-1028    71 NOT HISPANIC OR LATINO 65-80  2      
#>  4 01-701-1033    74 NOT HISPANIC OR LATINO 65-80  2      
#>  5 01-701-1034    77 NOT HISPANIC OR LATINO 65-80  2      
#>  6 01-701-1047    85 NOT HISPANIC OR LATINO >80    3      
#>  7 01-701-1057    59 HISPANIC OR LATINO     <65    1      
#>  8 01-701-1097    68 NOT HISPANIC OR LATINO 65-80  2      
#>  9 01-701-1111    81 NOT HISPANIC OR LATINO >80    3      
#> 10 01-701-1115    84 NOT HISPANIC OR LATINO >80    3      
#> # ... with 296 more rows
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
