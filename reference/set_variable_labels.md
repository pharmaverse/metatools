# Apply labels to a data frame using a metacore object

This function leverages metadata available in a metacore object to apply
labels to a data frame.

## Usage

``` r
set_variable_labels(
  data,
  metacore,
  dataset_name = deprecated(),
  verbose = c("message", "warn", "silent")
)
```

## Arguments

- data:

  A dataframe or tibble upon which labels will be applied

- metacore:

  metacore object that contains the specifications for the dataset of
  interest.

- dataset_name:

  **\[deprecated\]** Optional string to specify the dataset that is
  being built. This is only needed if the metacore object provided
  hasn't already been subsetted.  
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

Dataframe with labels applied

## Examples

``` r
mc <- metacore::spec_to_metacore(
  metacore::metacore_example("p21_mock.xlsx"),
  quiet = TRUE
)
#> ✔ Metadata successfully imported
#> ℹ Dataset metadata imported with suppressed warnings
#> ℹ To use the Metacore object with metatools package, first subset a dataset
#>   using `metacore::select_dataset()`
#> 
dm <- haven::read_xpt(metatools_example("dm.xpt"))
set_variable_labels(dm, mc, dataset_name = "DM")
#> ℹ The `dataset_name` argument will be removed in a future release. Please use
#>   `metacore::select_dataset()` to subset the metacore object to obtain metadata
#>   for a single dataset.
#> Warning: The `dataset_name` argument of `check_unique_keys()` is deprecated as of
#> metatools 0.2.0.
#> Warning: `core` from the `ds_vars` table only contains missing values.
#> Warning: `supp_flag` from the `ds_vars` table only contains missing values.
#> Warning: `format` from the `var_spec` table only contains missing values.
#> Warning: `sig_dig` from the `value_spec` table only contains missing values.
#> Warning: `where` from the `value_spec` table only contains missing values.
#> Warning: `dataset` from the `supp` table only contains missing values.
#> Warning: `variable` from the `supp` table only contains missing values.
#> Warning: `idvar` from the `supp` table only contains missing values.
#> Warning: `qeval` from the `supp` table only contains missing values.
#> ✔ DM dataset successfully selected
#> 
#> # A tibble: 306 × 25
#>    STUDYID      DOMAIN USUBJID  SUBJID RFSTDTC RFENDTC RFXSTDTC RFXENDTC RFICDTC
#>    <chr>        <chr>  <chr>    <chr>  <chr>   <chr>   <chr>    <chr>    <chr>  
#>  1 CDISCPILOT01 DM     01-701-… 1015   "2014-… "2014-… "2014-0… "2014-0… ""     
#>  2 CDISCPILOT01 DM     01-701-… 1023   "2012-… "2012-… "2012-0… "2012-0… ""     
#>  3 CDISCPILOT01 DM     01-701-… 1028   "2013-… "2014-… "2013-0… "2014-0… ""     
#>  4 CDISCPILOT01 DM     01-701-… 1033   "2014-… "2014-… "2014-0… "2014-0… ""     
#>  5 CDISCPILOT01 DM     01-701-… 1034   "2014-… "2014-… "2014-0… "2014-1… ""     
#>  6 CDISCPILOT01 DM     01-701-… 1047   "2013-… "2013-… "2013-0… "2013-0… ""     
#>  7 CDISCPILOT01 DM     01-701-… 1057   ""      ""      ""       ""       ""     
#>  8 CDISCPILOT01 DM     01-701-… 1097   "2014-… "2014-… "2014-0… "2014-0… ""     
#>  9 CDISCPILOT01 DM     01-701-… 1111   "2012-… "2012-… "2012-0… "2012-0… ""     
#> 10 CDISCPILOT01 DM     01-701-… 1115   "2012-… "2013-… "2012-1… "2013-0… ""     
#> # ℹ 296 more rows
#> # ℹ 16 more variables: RFPENDTC <chr>, DTHDTC <chr>, DTHFL <chr>, SITEID <chr>,
#> #   AGE <dbl>, AGEU <chr>, SEX <chr>, RACE <chr>, ETHNIC <chr>, ARMCD <chr>,
#> #   ARM <chr>, ACTARMCD <chr>, ACTARM <chr>, COUNTRY <chr>, DMDTC <chr>,
#> #   DMDY <dbl>
```
