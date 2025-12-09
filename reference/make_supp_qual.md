# Make Supplemental Qualifier

Make Supplemental Qualifier

## Usage

``` r
make_supp_qual(dataset, metacore, dataset_name = deprecated())
```

## Arguments

- dataset:

  dataset the supp will be pulled from

- metacore:

  A subsetted metacore object to get the supp information from. If not
  already subsetted then a `dataset_name` will need to be provided

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

a CDISC formatted SUPP dataset

## Examples

``` r
library(metacore)
library(safetyData)
library(tibble)
load(metacore_example("pilot_SDTM.rda"))
spec <- metacore %>% select_dataset("AE")
#> Warning: `core` from the `ds_vars` table only contains missing values.
#> Warning: `format` from the `var_spec` table only contains missing values.
#> Warning: `common` from the `var_spec` table only contains missing values.
#> Warning: `sig_dig` from the `value_spec` table only contains missing values.
#> Warning: `where` from the `value_spec` table only contains missing values.
#> ✔ AE dataset successfully selected
#> 
ae <- combine_supp(sdtm_ae, sdtm_suppae)
make_supp_qual(ae, spec) %>% as_tibble()
#> # A tibble: 1,191 × 10
#>    STUDYID      RDOMAIN USUBJID    IDVAR IDVARVAL QNAM  QLABEL QVAL  QORIG QEVAL
#>    <chr>        <chr>   <chr>      <chr>    <int> <chr> <chr>  <chr> <chr> <chr>
#>  1 CDISCPILOT01 AE      01-701-10… AESEQ        1 AETR… TREAT… Y     deri… CLIN…
#>  2 CDISCPILOT01 AE      01-701-10… AESEQ        2 AETR… TREAT… Y     deri… CLIN…
#>  3 CDISCPILOT01 AE      01-701-10… AESEQ        3 AETR… TREAT… Y     deri… CLIN…
#>  4 CDISCPILOT01 AE      01-701-10… AESEQ        1 AETR… TREAT… Y     deri… CLIN…
#>  5 CDISCPILOT01 AE      01-701-10… AESEQ        2 AETR… TREAT… Y     deri… CLIN…
#>  6 CDISCPILOT01 AE      01-701-10… AESEQ        3 AETR… TREAT… Y     deri… CLIN…
#>  7 CDISCPILOT01 AE      01-701-10… AESEQ        4 AETR… TREAT… Y     deri… CLIN…
#>  8 CDISCPILOT01 AE      01-701-10… AESEQ        1 AETR… TREAT… Y     deri… CLIN…
#>  9 CDISCPILOT01 AE      01-701-10… AESEQ        2 AETR… TREAT… Y     deri… CLIN…
#> 10 CDISCPILOT01 AE      01-701-10… AESEQ        1 AETR… TREAT… Y     deri… CLIN…
#> # ℹ 1,181 more rows
```
