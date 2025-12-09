# Create Categorical Variable from Codelist

Using the grouping from either the `decode_var` or `code_var` and a
reference variable (`ref_var`) it will create a categorical variable and
the numeric version of that categorical variable.

## Usage

``` r
create_cat_var(
  data,
  metacore,
  ref_var,
  grp_var,
  num_grp_var = NULL,
  create_from_decode = FALSE,
  strict = TRUE
)
```

## Arguments

- data:

  Dataset with reference variable in it

- metacore:

  A metacore object to get the codelist from. If the variable has
  different codelists for different datasets the metacore object will
  need to be subsetted using `select_dataset` from the metacore package.

- ref_var:

  Name of variable to be used as the reference i.e AGE when creating
  AGEGR1

- grp_var:

  Name of the new grouped variable

- num_grp_var:

  Name of the new numeric decode for the grouped variable. This is
  optional if no value given no variable will be created

- create_from_decode:

  Sets the `decode` column of the codelist as the column from which the
  variable will be created. By default the column is `code`.

- strict:

  A logical value indicating whether to perform strict checking against
  the codelist. If `TRUE` will issue a warning if values in the
  `ref_var` column do not fit into the group definitions for the
  codelist in `grp_var`. If `FALSE` no warning is issued and values not
  defined by the codelist will likely result in `NA` results.

## Value

dataset with new column added

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
dm <- read_xpt(metatools_example("dm.xpt")) %>%
  select(USUBJID, AGE)
# Grouping Column Only
create_cat_var(dm, spec, AGE, AGEGR1)
#> # A tibble: 306 × 3
#>    USUBJID       AGE AGEGR1
#>    <chr>       <dbl> <chr> 
#>  1 01-701-1015    63 <65   
#>  2 01-701-1023    64 <65   
#>  3 01-701-1028    71 65-80 
#>  4 01-701-1033    74 65-80 
#>  5 01-701-1034    77 65-80 
#>  6 01-701-1047    85 >80   
#>  7 01-701-1057    59 <65   
#>  8 01-701-1097    68 65-80 
#>  9 01-701-1111    81 >80   
#> 10 01-701-1115    84 >80   
#> # ℹ 296 more rows
# Grouping Column and Numeric Decode
create_cat_var(dm, spec, AGE, AGEGR1, AGEGR1N)
#> # A tibble: 306 × 4
#>    USUBJID       AGE AGEGR1 AGEGR1N
#>    <chr>       <dbl> <chr>    <dbl>
#>  1 01-701-1015    63 <65          1
#>  2 01-701-1023    64 <65          1
#>  3 01-701-1028    71 65-80        2
#>  4 01-701-1033    74 65-80        2
#>  5 01-701-1034    77 65-80        2
#>  6 01-701-1047    85 >80          3
#>  7 01-701-1057    59 <65          1
#>  8 01-701-1097    68 65-80        2
#>  9 01-701-1111    81 >80          3
#> 10 01-701-1115    84 >80          3
#> # ℹ 296 more rows
```
