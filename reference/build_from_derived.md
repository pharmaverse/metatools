# Build a dataset from derived

This function builds a dataset out of the columns that just need to be
pulled through. So any variable that has a derivation in the format of
'dataset.variable' will be pulled through to create the new dataset.
When there are multiple datasets present, they will be joined by the
shared `key_seq` variables. These columns are often called
'Predecessors' in ADaM, but this is not universal so that is optional to
specify.

## Usage

``` r
build_from_derived(
  metacore,
  ds_list,
  dataset_name = deprecated(),
  predecessor_only = TRUE,
  keep = FALSE
)
```

## Arguments

- metacore:

  metacore object that contains the specifications for the dataset of
  interest.

- ds_list:

  Named list of datasets that are needed to build the from. If the list
  is unnamed,then it will use the names of the objects.

- dataset_name:

  **\[deprecated\]** Optional string to specify the dataset that is
  being built. This is only needed if the metacore object provided
  hasn't already been subsetted.  
  Note: Deprecated in version 0.2.0. The `dataset_name` argument will be
  removed in a future release. Please use
  [`metacore::select_dataset`](https://atorus-research.github.io/metacore/reference/select_dataset.html)
  to subset the `metacore` object to obtain metadata for a single
  dataset.

- predecessor_only:

  By default `TRUE`, so only variables with the origin of 'Predecessor'
  will be used. If `FALSE` any derivation matching the dataset.variable
  will be used.

- keep:

  String to determine which columns from the original datasets should be
  kept

  - "FALSE" (default): only columns that are also present in the ADaM
    specification are kept in the output.

  - "ALL": all original columns are carried through to the ADaM,
    including those that have been renamed. e.g. if DM.ARM is a
    predecessor to DM.TRT01P, both ARM and TRT01P will be present as
    columns in the ADaM output.

  - "PREREQUISITE": columns are retained if they are required for future
    derivations in the specification. Additional prerequisite columns
    are identified as columns that appear in the 'derivation' column of
    the metacore object in the format "DATASET.VARIABLE", but not as
    direct predecessors. Predecessors are defined as columns where the
    derivation is a 1:1 copy of a column in a source dataset. e.g.
    derivation = "VS.VSTESTCD" is a predecessor, while derivation =
    "Value of VS.VSSTRESN where VS.VSTESTCD == 'Heart Rate'" contains
    both VS.VSTESTCD and VS.VSSTRESN as prerequisites, and these columns
    will be kept through to the ADaM.

## Value

dataset

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
ds_list <- list(DM = read_xpt(metatools_example("dm.xpt")))
build_from_derived(spec, ds_list, predecessor_only = FALSE)
#> # A tibble: 306 × 13
#>    USUBJID     SUBJID SITEID ARM     TRT01P   AGE AGEU  RACE  SEX   ETHNIC DTHFL
#>    <chr>       <chr>  <chr>  <chr>   <chr>  <dbl> <chr> <chr> <chr> <chr>  <chr>
#>  1 01-701-1015 1015   701    Placebo Place…    63 YEARS WHITE F     HISPA… ""   
#>  2 01-701-1023 1023   701    Placebo Place…    64 YEARS WHITE M     HISPA… ""   
#>  3 01-701-1028 1028   701    Xanome… Xanom…    71 YEARS WHITE M     NOT H… ""   
#>  4 01-701-1033 1033   701    Xanome… Xanom…    74 YEARS WHITE M     NOT H… ""   
#>  5 01-701-1034 1034   701    Xanome… Xanom…    77 YEARS WHITE F     NOT H… ""   
#>  6 01-701-1047 1047   701    Placebo Place…    85 YEARS WHITE F     NOT H… ""   
#>  7 01-701-1057 1057   701    Screen… Scree…    59 YEARS WHITE F     HISPA… ""   
#>  8 01-701-1097 1097   701    Xanome… Xanom…    68 YEARS WHITE M     NOT H… ""   
#>  9 01-701-1111 1111   701    Xanome… Xanom…    81 YEARS WHITE F     NOT H… ""   
#> 10 01-701-1115 1115   701    Xanome… Xanom…    84 YEARS WHITE M     NOT H… ""   
#> # ℹ 296 more rows
#> # ℹ 2 more variables: RFSTDTC <chr>, RFENDTC <chr>
```
