# Check Control Terminology for a Single Column

This function checks the column in the dataset only contains the control
terminology as defined by the metacore specification

## Usage

``` r
check_ct_col(data, metacore, var, na_acceptable = NULL)
```

## Arguments

- data:

  Data to check

- metacore:

  A metacore object to get the codelist from. If the variable has
  different codelists for different datasets the metacore object will
  need to be subsetted using `select_dataset` from the metacore package.

- var:

  Name of variable to check

- na_acceptable:

  Logical value, set to `NULL` by default, so the acceptability of
  missing values is based on if the core for the variable is "Required"
  in the `metacore` object. If set to `TRUE` then will pass check if
  values are in the control terminology or are missing. If set to
  `FALSE`then NA will not be acceptable.

## Value

Given data if column only contains control terms. If not, will error
given the values which should not be in the column

## Examples

``` r
library(metacore)
library(haven)
library(magrittr)
load(metacore_example("pilot_ADaM.rda"))
spec <- metacore %>% select_dataset("ADSL")
#> Warning: `core` from the `ds_vars` table only contains missing values.
#> Warning: `supp_flag` from the `ds_vars` table only contains missing values.
#> Warning: `common` from the `var_spec` table only contains missing values.
#> Warning: `where` from the `value_spec` table only contains missing values.
#> Warning: `dataset` from the `supp` table only contains missing values.
#> Warning: `variable` from the `supp` table only contains missing values.
#> Warning: `idvar` from the `supp` table only contains missing values.
#> Warning: `qeval` from the `supp` table only contains missing values.
#> ✔ ADSL dataset successfully selected
#> 
data <- read_xpt(metatools_example("adsl.xpt"))
check_ct_col(data, spec, TRT01PN)
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
check_ct_col(data, spec, "TRT01PN")
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
