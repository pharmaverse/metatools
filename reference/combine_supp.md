# Combine the Domain and Supplemental Qualifier

Combine the Domain and Supplemental Qualifier

## Usage

``` r
combine_supp(dataset, supp)
```

## Arguments

- dataset:

  Domain dataset

- supp:

  Supplemental Qualifier dataset

## Value

a dataset with the supp variables added to it

## Examples

``` r
library(safetyData)
library(tibble)
combine_supp(sdtm_ae, sdtm_suppae) %>% as_tibble()
#> # A tibble: 1,191 × 36
#>    STUDYID DOMAIN USUBJID AESEQ AESPID AETERM AELLT AELLTCD AEDECOD AEPTCD AEHLT
#>    <chr>   <chr>  <chr>   <int> <chr>  <chr>  <chr> <lgl>   <chr>   <lgl>  <chr>
#>  1 CDISCP… AE     01-701…     1 E07    APPLI… APPL… NA      APPLIC… NA     HLT_…
#>  2 CDISCP… AE     01-701…     2 E08    APPLI… APPL… NA      APPLIC… NA     HLT_…
#>  3 CDISCP… AE     01-701…     3 E06    DIARR… DIAR… NA      DIARRH… NA     HLT_…
#>  4 CDISCP… AE     01-701…     3 E10    ATRIO… AV B… NA      ATRIOV… NA     HLT_…
#>  5 CDISCP… AE     01-701…     1 E08    ERYTH… ERYT… NA      ERYTHE… NA     HLT_…
#>  6 CDISCP… AE     01-701…     2 E09    ERYTH… LOCA… NA      ERYTHE… NA     HLT_…
#>  7 CDISCP… AE     01-701…     4 E08    ERYTH… ERYT… NA      ERYTHE… NA     HLT_…
#>  8 CDISCP… AE     01-701…     1 E04    APPLI… APPL… NA      APPLIC… NA     HLT_…
#>  9 CDISCP… AE     01-701…     2 E05    APPLI… APPL… NA      APPLIC… NA     HLT_…
#> 10 CDISCP… AE     01-701…     1 E08    APPLI… APPL… NA      APPLIC… NA     HLT_…
#> # ℹ 1,181 more rows
#> # ℹ 25 more variables: AEHLTCD <lgl>, AEHLGT <chr>, AEHLGTCD <lgl>,
#> #   AEBODSYS <chr>, AEBDSYCD <lgl>, AESOC <chr>, AESOCCD <lgl>, AESEV <chr>,
#> #   AESER <chr>, AEACN <lgl>, AEREL <chr>, AEOUT <chr>, AESCAN <chr>,
#> #   AESCONG <chr>, AESDISAB <chr>, AESDTH <chr>, AESHOSP <chr>, AESLIFE <chr>,
#> #   AESOD <chr>, AEDTC <chr>, AESTDTC <chr>, AEENDTC <chr>, AESTDY <int>,
#> #   AEENDY <int>, AETRTEM <chr>
```
