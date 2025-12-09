# Remove labels to multiple variables on a data frame

This function allows a user to removes all labels to a dataframe at
once.

## Usage

``` r
remove_labels(data)
```

## Arguments

- data:

  A data.frame or tibble

## Value

data with variable labels applied

## Examples

``` r
library(haven)
data <- read_xpt(metatools_example("adsl.xpt"))
remove_labels(data)
#> # A tibble: 254 × 51
#>    STUDYID     USUBJID SUBJID SITEID SITEGR1 ARM   TRT01P TRT01PN TRT01A TRT01AN
#>    <chr>       <chr>   <chr>  <chr>  <chr>   <chr> <chr>    <dbl> <chr>    <dbl>
#>  1 CDISCPILOT… 01-701… 1015   701    701     Plac… Place…       0 Place…       0
#>  2 CDISCPILOT… 01-701… 1023   701    701     Plac… Place…       0 Place…       0
#>  3 CDISCPILOT… 01-701… 1028   701    701     Xano… Xanom…      81 Xanom…      81
#>  4 CDISCPILOT… 01-701… 1033   701    701     Xano… Xanom…      54 Xanom…      54
#>  5 CDISCPILOT… 01-701… 1034   701    701     Xano… Xanom…      81 Xanom…      81
#>  6 CDISCPILOT… 01-701… 1047   701    701     Plac… Place…       0 Place…       0
#>  7 CDISCPILOT… 01-701… 1097   701    701     Xano… Xanom…      54 Xanom…      54
#>  8 CDISCPILOT… 01-701… 1111   701    701     Xano… Xanom…      54 Xanom…      54
#>  9 CDISCPILOT… 01-701… 1115   701    701     Xano… Xanom…      54 Xanom…      54
#> 10 CDISCPILOT… 01-701… 1118   701    701     Plac… Place…       0 Place…       0
#> # ℹ 244 more rows
#> # ℹ 41 more variables: TRTSDT <date>, TRTEDT <date>, TRTDURD <dbl>,
#> #   AVGDD <dbl>, CUMDOSE <dbl>, AGE <dbl>, AGEGR1 <chr>, AGEGR1N <dbl>,
#> #   AGEGR2 <chr>, AGEGR2N <dbl>, AGEU <chr>, RACE <chr>, RACEN <dbl>,
#> #   SEX <chr>, ETHNIC <chr>, SAFFL <chr>, ITTFL <chr>, EFFFL <chr>,
#> #   COMP8FL <chr>, COMP16FL <chr>, COMP24FL <chr>, DISCONFL <chr>,
#> #   DSRAEFL <chr>, DTHFL <chr>, BMIBL <dbl>, BMIBLGR1 <chr>, HEIGHTBL <dbl>, …
```
