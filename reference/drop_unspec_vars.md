# Drop Unspecified Variables

This function drops all unspecified variables.

## Usage

``` r
drop_unspec_vars(dataset, metacore, dataset_name = deprecated())
```

## Arguments

- dataset:

  Dataset to change

- metacore:

  metacore object that only contains the specifications for the dataset
  of interest.

- dataset_name:

  **\[deprecated\]** Optional string to specify the dataset. This is
  only needed if the metacore object provided hasn't already been
  subsetted.  
  Note: Deprecated in version 0.2.0. The `dataset_name` argument will be
  removed in a future release. Please use
  [`metacore::select_dataset`](https://atorus-research.github.io/metacore/reference/select_dataset.html)
  to subset the `metacore` object to obtain metadata for a single
  dataset.

## Value

Dataset with only specified columns

## Examples

``` r
library(metacore)
library(haven)
library(dplyr)
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
  select(USUBJID, SITEID) %>%
  mutate(foo = "Hello")
drop_unspec_vars(data, spec)
#> The following variable(s) were dropped:
#>   foo
#> # A tibble: 254 × 2
#>    USUBJID     SITEID
#>    <chr>       <chr> 
#>  1 01-701-1015 701   
#>  2 01-701-1023 701   
#>  3 01-701-1028 701   
#>  4 01-701-1033 701   
#>  5 01-701-1034 701   
#>  6 01-701-1047 701   
#>  7 01-701-1097 701   
#>  8 01-701-1111 701   
#>  9 01-701-1115 701   
#> 10 01-701-1118 701   
#> # ℹ 244 more rows
```
