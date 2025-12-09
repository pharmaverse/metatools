# Create Variable from Codelist

This functions uses code/decode pairs from a metacore object to create
new variables in the data

## Usage

``` r
create_var_from_codelist(
  data,
  metacore,
  input_var,
  out_var,
  codelist = NULL,
  decode_to_code = TRUE,
  strict = TRUE
)
```

## Arguments

- data:

  Dataset that contains the input variable

- metacore:

  A metacore object to get the codelist from. This should be a subsetted
  metacore object (of subclass `DatasetMeta`) created using
  [`metacore::select_dataset`](https://atorus-research.github.io/metacore/reference/select_dataset.html).

- input_var:

  Name of the variable that will be translated for the new column

- out_var:

  Name of the output variable. Note: Unless a codelist is provided the
  grouping will always be from the code of the codelist associates with
  `out_var`.

- codelist:

  Optional argument to supply a codelist. Must be a data.frame with
  `code` and `decode` columns such as those created by the function
  [`metacore::get_control_term`](https://atorus-research.github.io/metacore/reference/get_control_term.html).
  If no codelist is provided the codelist associated with the column
  supplied to `out_var` will be used. By default `codelist` is `NULL`.

- decode_to_code:

  Direction of the translation. Default value is `TRUE`, i.e., assumes
  the `input_var` is the decode column of the codelist. Set to `FALSE`
  if the `input_var` is the code column of the codelist.

- strict:

  A logical value indicating whether to perform strict checking against
  the codelist. If `TRUE` will issue a warning if values in the
  `input_var` column are not present in the codelist. If `FALSE` no
  warning is issued and values not present in the codelist will likely
  result in `NA` results.

## Value

Dataset with a new column added

## Examples

``` r
library(metacore)
library(tibble)
data <- tribble(
  ~USUBJID, ~VAR1, ~VAR2,
  1, "M", "Male",
  2, "F", "Female",
  3, "F", "Female",
  4, "U", "Unknown",
  5, "M", "Male",
)
spec <- spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE)
#> ✔ Metadata successfully imported
#> ℹ Dataset metadata imported with suppressed warnings
#> ℹ To use the Metacore object with metatools package, first subset a dataset
#>   using `metacore::select_dataset()`
#> 
dm_spec <- select_dataset(spec, "DM", quiet = TRUE)
#> ✔ DM dataset successfully selected
#> ℹ Dataset metadata specification subsetted with suppressed warnings
#> 
create_var_from_codelist(data, dm_spec, VAR2, SEX)
#> # A tibble: 5 × 4
#>   USUBJID VAR1  VAR2    SEX  
#>     <dbl> <chr> <chr>   <chr>
#> 1       1 M     Male    M    
#> 2       2 F     Female  F    
#> 3       3 F     Female  F    
#> 4       4 U     Unknown U    
#> 5       5 M     Male    M    
create_var_from_codelist(data, dm_spec, "VAR2", "SEX")
#> # A tibble: 5 × 4
#>   USUBJID VAR1  VAR2    SEX  
#>     <dbl> <chr> <chr>   <chr>
#> 1       1 M     Male    M    
#> 2       2 F     Female  F    
#> 3       3 F     Female  F    
#> 4       4 U     Unknown U    
#> 5       5 M     Male    M    
create_var_from_codelist(data, dm_spec, VAR1, SEX, decode_to_code = FALSE)
#> # A tibble: 5 × 4
#>   USUBJID VAR1  VAR2    SEX    
#>     <dbl> <chr> <chr>   <chr>  
#> 1       1 M     Male    Male   
#> 2       2 F     Female  Female 
#> 3       3 F     Female  Female 
#> 4       4 U     Unknown Unknown
#> 5       5 M     Male    Male   

# Example providing a custom codelist
# This example also reverses the direction of translation
load(metacore_example('pilot_ADaM.rda'))
adlb_spec <- select_dataset(metacore, "ADLBC", quiet = TRUE)
#> ✔ ADLBC dataset successfully selected
#> ℹ Dataset metadata specification subsetted with suppressed warnings
#> 
adlb <- tibble(PARAMCD = c("ALB", "ALP", "ALT", "AST", "BILI", "BUN"))
create_var_from_codelist(
   adlb,
   adlb_spec,
   PARAMCD,
   PARAM,
   codelist = get_control_term(adlb_spec, PARAMCD),
   decode_to_code = FALSE,
   strict = FALSE)
#> # A tibble: 6 × 2
#>   PARAMCD PARAM                           
#>   <chr>   <chr>                           
#> 1 ALB     Albumin (g/L)                   
#> 2 ALP     Alkaline Phosphatase (U/L)      
#> 3 ALT     Alanine Aminotransferase (U/L)  
#> 4 AST     Aspartate Aminotransferase (U/L)
#> 5 BILI    Bilirubin (umol/L)              
#> 6 BUN     Blood Urea Nitrogen (mmol/L)    

if (FALSE) { # \dontrun{
# Example expecting warning where `strict` == `TRUE`
adlb <- tibble(PARAMCD = c("ALB", "ALP", "ALT", "AST", "BILI", "BUN", "DUMMY1", "DUMMY2"))
create_var_from_codelist(
   adlb,
   adlb_spec,
   PARAMCD,
   PARAM,
   codelist = get_control_term(adlb_spec, PARAMCD),
   decode_to_code = FALSE,
   strict = TRUE)
} # }
```
