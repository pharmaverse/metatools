# Add Missing Variables

This function adds in missing columns according to the type set in the
metacore object. All values in the new columns will be missing, but
typed correctly. If unable to recognize the type in the metacore object
will return a logical type.

## Usage

``` r
add_variables(dataset, metacore, dataset_name = deprecated())
```

## Arguments

- dataset:

  Dataset to add columns to. If all variables are present no columns
  will be added.

- metacore:

  metacore object that only contains the specifications for the dataset
  of interest.

- dataset_name:

  Optional string to specify the dataset. This is only needed if the
  metacore object provided hasn't already been subsetted.  
  Note: Deprecated in version 0.2.0. The `dataset_name` argument will be
  removed in a future release. Please use
  [`metacore::select_dataset`](https://atorus-research.github.io/metacore/reference/select_dataset.html)
  to subset the `metacore` object to obtain metadata for a single
  dataset.

## Value

The given dataset with any additional columns added

## Examples

``` r
library(metacore)
library(haven)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
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
data <- read_xpt(metatools_example("adsl.xpt")) %>%
   select(-TRTSDT, -TRT01P, -TRT01PN)
add_variables(data, spec)
#> # A tibble: 254 × 51
#>    STUDYID USUBJID SUBJID SITEID SITEGR1 ARM   TRT01A TRT01AN TRTEDT     TRTDURD
#>    <chr>   <chr>   <chr>  <chr>  <chr>   <chr> <chr>    <dbl> <date>       <dbl>
#>  1 CDISCP… 01-701… 1015   701    701     Plac… Place…       0 2014-07-02     182
#>  2 CDISCP… 01-701… 1023   701    701     Plac… Place…       0 2012-09-01      28
#>  3 CDISCP… 01-701… 1028   701    701     Xano… Xanom…      81 2014-01-14     180
#>  4 CDISCP… 01-701… 1033   701    701     Xano… Xanom…      54 2014-03-31      14
#>  5 CDISCP… 01-701… 1034   701    701     Xano… Xanom…      81 2014-12-30     183
#>  6 CDISCP… 01-701… 1047   701    701     Plac… Place…       0 2013-03-09      26
#>  7 CDISCP… 01-701… 1097   701    701     Xano… Xanom…      54 2014-07-09     190
#>  8 CDISCP… 01-701… 1111   701    701     Xano… Xanom…      54 2012-09-16      10
#>  9 CDISCP… 01-701… 1115   701    701     Xano… Xanom…      54 2013-01-23      55
#> 10 CDISCP… 01-701… 1118   701    701     Plac… Place…       0 2014-09-09     182
#> # ℹ 244 more rows
#> # ℹ 41 more variables: AVGDD <dbl>, CUMDOSE <dbl>, AGE <dbl>, AGEGR1 <chr>,
#> #   AGEGR1N <dbl>, AGEGR2 <chr>, AGEGR2N <dbl>, AGEU <chr>, RACE <chr>,
#> #   RACEN <dbl>, SEX <chr>, ETHNIC <chr>, SAFFL <chr>, ITTFL <chr>,
#> #   EFFFL <chr>, COMP8FL <chr>, COMP16FL <chr>, COMP24FL <chr>, DISCONFL <chr>,
#> #   DSRAEFL <chr>, DTHFL <chr>, BMIBL <dbl>, BMIBLGR1 <chr>, HEIGHTBL <dbl>,
#> #   WEIGHTBL <dbl>, EDUCLVL <dbl>, DISONSDT <date>, DURDIS <dbl>, …
```
