# Convert Variable to Factor with Levels Set by Control Terms

This functions takes a dataset, a metacore object and a variable name.
Then looks at the metacore object for the control terms for the given
variable and uses that to convert the variable to a factor with those
levels. If the control terminology is a code list, the code column will
be used. The function fails if the control terminology is an external
library

## Usage

``` r
convert_var_to_fct(data, metacore, var)
```

## Arguments

- data:

  A dataset containing the variable to be modified

- metacore:

  A metacore object to get the codelist from. If the variable has
  different codelists for different datasets the metacore object will
  need to be subsetted using `select_dataset` from the metacore package

- var:

  Name of variable to change

## Value

Dataset with variable changed to a factor

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
  select(USUBJID, SEX, ARM)
# Variable with codelist control terms
convert_var_to_fct(dm, spec, SEX)
#> # A tibble: 306 × 3
#>    USUBJID     SEX   ARM                 
#>    <chr>       <fct> <chr>               
#>  1 01-701-1015 F     Placebo             
#>  2 01-701-1023 M     Placebo             
#>  3 01-701-1028 M     Xanomeline High Dose
#>  4 01-701-1033 M     Xanomeline Low Dose 
#>  5 01-701-1034 F     Xanomeline High Dose
#>  6 01-701-1047 F     Placebo             
#>  7 01-701-1057 F     Screen Failure      
#>  8 01-701-1097 M     Xanomeline Low Dose 
#>  9 01-701-1111 F     Xanomeline Low Dose 
#> 10 01-701-1115 M     Xanomeline Low Dose 
#> # ℹ 296 more rows
# Variable with permitted value control terms
convert_var_to_fct(dm, spec, ARM)
#> # A tibble: 306 × 3
#>    USUBJID     SEX   ARM                 
#>    <chr>       <chr> <fct>               
#>  1 01-701-1015 F     Placebo             
#>  2 01-701-1023 M     Placebo             
#>  3 01-701-1028 M     Xanomeline High Dose
#>  4 01-701-1033 M     Xanomeline Low Dose 
#>  5 01-701-1034 F     Xanomeline High Dose
#>  6 01-701-1047 F     Placebo             
#>  7 01-701-1057 F     NA                  
#>  8 01-701-1097 M     Xanomeline Low Dose 
#>  9 01-701-1111 F     Xanomeline Low Dose 
#> 10 01-701-1115 M     Xanomeline Low Dose 
#> # ℹ 296 more rows
```
