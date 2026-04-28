# Drop Unspecified Variables

This function drops all unspecified variables.

## Usage

``` r
drop_unspec_vars(
  dataset,
  metacore,
  dataset_name = deprecated(),
  verbose = c("message", "warn", "silent")
)
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

- verbose:

  Character string controlling message verbosity. One of:

  `"message"`

  :   Show both warnings and messages (default)

  `"warn"`

  :   Show warnings but suppress messages

  `"silent"`

  :   Suppress all warnings and messages

## Value

Dataset with only specified columns

## Examples

``` r
library(metacore)
library(haven)
library(dplyr)
load(metacore_example("pilot_ADaM.rda"))
spec <- metacore %>% select_dataset("ADSL")
#> Warning: 'ds_vars' has incorrect column names. It should be: dataset, variable, key_seq,
#> order, mandatory, core, supp_flag
#> Warning: Other checks were not performed, because column names were incorrect
#> ✔ ADSL dataset successfully selected
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
