# Check Control Terminology for a Dataset

This function checks that all columns in the dataset only contains the
control terminology as defined by the metacore specification

## Usage

``` r
check_ct_data(data, metacore, na_acceptable = NULL, omit_vars = NULL)
```

## Arguments

- data:

  Dataset to check

- metacore:

  metacore object that contains the specifications for the dataset of
  interest. If any variable has different codelists for different
  datasets the metacore object will need to be subsetted using
  `select_dataset` from the metacore package.

- na_acceptable:

  `logical` value or `character` vector, set to `NULL` by default.
  `NULL` sets the acceptability of missing values based on if the core
  for the variable is "Required" in the `metacore` object. If set to
  `TRUE` then will pass check if values are in the control terminology
  or are missing. If set to `FALSE` then NA will not be acceptable. If
  set to a `character` vector then only the specified variables may
  contain NA values.

- omit_vars:

  `character` vector indicating which variables should be skipped when
  doing the controlled terminology checks. Internally, `omit_vars` is
  evaluated before `na_acceptable`.

## Value

Given data if all columns pass. It will error otherwise

## Examples

``` r
library(haven)
library(metacore)
library(magrittr)
load(metacore_example("pilot_ADaM.rda"))
spec <- metacore %>% select_dataset("ADSL", quiet = TRUE)
#> ✔ ADSL dataset successfully selected
#> ℹ Dataset metadata specification subsetted with suppressed warnings
#> 
data <- read_xpt(metatools_example("adsl.xpt"))

check_ct_data(data, spec, omit_vars = c("AGEGR2", "AGEGR2N"))
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
if (FALSE) { # \dontrun{
# These examples produce errors:
check_ct_data(data, spec, na_acceptable = FALSE)
check_ct_data(data, spec, na_acceptable = FALSE, omit_vars = "DISCONFL")
check_ct_data(data, spec, na_acceptable = c("DSRAEFL", "DCSREAS"), omit_vars = "DISCONFL")
} # }
```
