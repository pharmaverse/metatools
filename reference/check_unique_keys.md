# Check Uniqueness of Records by Key

This function checks the uniqueness of records in the dataset by key
using `get_keys` from the metacore package. If the key uniquely
identifies each record the function will print a message stating
everything is as expected. If records are not uniquely identified an
error will explain the duplicates.

## Usage

``` r
check_unique_keys(data, metacore, dataset_name = deprecated())
```

## Arguments

- data:

  Dataset to check

- metacore:

  metacore object that only contains the specifications for the dataset
  of interest.

- dataset_name:

  **\[deprecated\]** Optional string to specify the dataset that is
  being built. This is only needed if the metacore object provided
  hasn't already been subsetted.  
  Note: Deprecated in version 0.2.0. The `dataset_name` argument will be
  removed in a future release. Please use
  [`metacore::select_dataset`](https://atorus-research.github.io/metacore/reference/select_dataset.html)
  to subset the `metacore` object to obtain metadata for a single
  dataset.

## Value

message if the key uniquely identifies each dataset record, and error
otherwise

## Examples

``` r
library(haven)
library(metacore)
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
check_unique_keys(data, spec)
#> Keys uniquely identify records
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
