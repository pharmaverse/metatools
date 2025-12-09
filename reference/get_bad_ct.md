# Gets vector of control terminology which should be there

This function checks the column in the dataset only contains the control
terminology as defined by the metacore specification. It will return all
values not found in the control terminology

## Usage

``` r
get_bad_ct(data, metacore, var, na_acceptable = NULL)
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
  `FALSE` then NA will not be acceptable.

## Value

vector

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
get_bad_ct(data, spec, "DCSREAS")
#> character(0)
get_bad_ct(data, spec, "DCSREAS", na_acceptable = FALSE)
#> [1] ""
```
